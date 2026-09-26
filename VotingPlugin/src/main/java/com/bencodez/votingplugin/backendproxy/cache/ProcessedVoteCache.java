package com.bencodez.votingplugin.backendproxy.cache;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import lombok.Getter;

/**
 * Tracks recently processed proxy vote ids so duplicate delivery is ignored.
 */
public class ProcessedVoteCache {

	private static final long DEFAULT_TTL_MILLIS = TimeUnit.MINUTES.toMillis(30);
	private static final int MAX_REDIS_DELIVERIES = 4096;
	public static final int MAX_LEGACY_REDIS_DELIVERY_BYTES = 256 * 1024;
	public static final int MAX_LEGACY_REDIS_TOTAL_BYTES = 4 * 1024 * 1024;

	@Getter
	private final ConcurrentHashMap<UUID, Long> processedVotes = new ConcurrentHashMap<>();
	private final java.util.Set<UUID> completedVotes = ConcurrentHashMap.newKeySet();
	private final java.util.Set<UUID> completedAwaitingReceipt = ConcurrentHashMap.newKeySet();
	private final long ttlMillis;
	private final DurableVoteReceiptStore durableReceipts;
	private final LinkedHashMap<String, Long> processedRedisDeliveries = new LinkedHashMap<>();
	private final LinkedHashMap<String, Integer> legacyRedisDeliveries = new LinkedHashMap<>();
	private long legacyRedisDeliveryBytes;
	private boolean legacyRedisHandoffOverflowed;
	private Object activeRedisSubscriber;
	private Object standbyRedisSubscriber;

	public ProcessedVoteCache() {
		this(DEFAULT_TTL_MILLIS, (DurableVoteReceiptStore) null);
	}

	public ProcessedVoteCache(long ttlMillis) {
		this(ttlMillis, (DurableVoteReceiptStore) null);
	}

	public ProcessedVoteCache(Path receiptFile) {
		this(DEFAULT_TTL_MILLIS, loadReceipts(receiptFile));
	}

	ProcessedVoteCache(long ttlMillis, Path receiptFile) {
		this(ttlMillis, loadReceipts(receiptFile));
	}

	private ProcessedVoteCache(long ttlMillis, DurableVoteReceiptStore durableReceipts) {
		this.ttlMillis = ttlMillis;
		this.durableReceipts = durableReceipts;
		if (durableReceipts != null) {
			Map<UUID, Long> receipts = durableReceipts.snapshot();
			processedVotes.putAll(receipts);
			completedVotes.addAll(receipts.keySet());
		}
	}

	private static DurableVoteReceiptStore loadReceipts(Path receiptFile) {
		try {
			return new DurableVoteReceiptStore(receiptFile);
		} catch (IOException failure) {
			throw new IllegalStateException("Unable to load durable backend vote receipts", failure);
		}
	}

	public boolean reserve(UUID voteId) {
		if (voteId == null) {
			return true;
		}

		long now = System.currentTimeMillis();
		long expiresAt = now + ttlMillis;

		while (true) {
			if (completedAwaitingReceipt.contains(voteId)) return false;
			Long currentExpiry = processedVotes.get(voteId);
			if (currentExpiry == null) {
				if (processedVotes.putIfAbsent(voteId, expiresAt) == null) {
					cleanup(now);
					return true;
				}
				continue;
			}

			if (currentExpiry > now) {
				return false;
			}

			if (processedVotes.replace(voteId, currentExpiry, expiresAt)) {
				completedVotes.remove(voteId);
				cleanup(now);
				return true;
			}
		}
	}

	/** Releases an admission that failed before any vote side effects ran. */
	public void release(UUID voteId) {
		if (voteId != null) processedVotes.remove(voteId);
	}

	/** Persists successful processing before the backend emits a delivery acknowledgement. */
	public boolean complete(UUID voteId) {
		if (voteId == null) return true;
		completedAwaitingReceipt.add(voteId);
		if (durableReceipts == null) {
			completedVotes.add(voteId);
			completedAwaitingReceipt.remove(voteId);
			return true;
		}
		long expiresAt = durableReceipts.complete(voteId);
		if (expiresAt <= 0L) return false;
		processedVotes.put(voteId, expiresAt);
		completedVotes.add(voteId);
		completedAwaitingReceipt.remove(voteId);
		return true;
	}

	/** Returns whether vote effects completed, including a receipt append awaiting retry. */
	public boolean hasCompletedEffects(UUID voteId) {
		return voteId != null && (completedVotes.contains(voteId) || completedAwaitingReceipt.contains(voteId));
	}

	/** Returns whether an acknowledgement-safe receipt is already durable. */
	public boolean hasDurableReceipt(UUID voteId) {
		return durableReceipts != null && durableReceipts.contains(voteId);
	}

	/** Durably retires a completed receipt after the proxy confirms outbox removal. */
	public boolean releaseCompletedReceipt(UUID voteId) {
		if (voteId == null) return false;
		if (durableReceipts == null) {
			completedVotes.remove(voteId);
			processedVotes.remove(voteId);
			return true;
		}
		long expiresAt = durableReceipts.release(voteId);
		if (expiresAt <= 0L) return false;
		completedVotes.add(voteId);
		processedVotes.put(voteId, expiresAt);
		return true;
	}

	/** Deduplicates one Redis envelope across overlapping subscribers during a validated handoff. */
	public synchronized boolean reserveRedisDelivery(String deliveryId) {
		if (deliveryId == null || !deliveryId.matches("[0-9a-fA-F-]{36}")) return true;
		long now = System.currentTimeMillis();
		processedRedisDeliveries.entrySet().removeIf(entry -> entry.getValue() <= now);
		Long current = processedRedisDeliveries.get(deliveryId);
		if (current != null && current > now) return false;
		processedRedisDeliveries.put(deliveryId, now + ttlMillis);
		while (processedRedisDeliveries.size() > MAX_REDIS_DELIVERIES) {
			Iterator<Map.Entry<String, Long>> oldest = processedRedisDeliveries.entrySet().iterator();
			if (!oldest.hasNext()) break;
			oldest.next();
			oldest.remove();
		}
		return true;
	}

	public synchronized boolean registerRedisSubscriber(Object subscriber) {
		if (activeRedisSubscriber == null) {
			activeRedisSubscriber = subscriber;
			return true;
		} else if (activeRedisSubscriber != subscriber) {
			standbyRedisSubscriber = subscriber;
			legacyRedisDeliveries.clear();
			legacyRedisDeliveryBytes = 0;
			legacyRedisHandoffOverflowed = false;
			return false;
		}
		return true;
	}

	/** Returns true only for the active subscriber and counts its legacy delivery during overlap. */
	public synchronized boolean reserveLegacyRedisDelivery(Object subscriber, String signature) {
		if (activeRedisSubscriber != subscriber) return false;
		if (standbyRedisSubscriber != null) {
			Integer count = legacyRedisDeliveries.get(signature);
			if (count != null) {
				legacyRedisDeliveries.put(signature,
						count == Integer.MAX_VALUE ? Integer.MAX_VALUE : count + 1);
			} else {
				int bytes = legacyRedisDeliveryBytes(signature);
				if (bytes <= MAX_LEGACY_REDIS_DELIVERY_BYTES
						&& legacyRedisDeliveries.size() < MAX_REDIS_DELIVERIES
						&& legacyRedisDeliveryBytes <= MAX_LEGACY_REDIS_TOTAL_BYTES - bytes) {
					legacyRedisDeliveries.put(signature, 1);
					legacyRedisDeliveryBytes += bytes;
				} else legacyRedisHandoffOverflowed = true;
			}
		}
		return true;
	}

	public synchronized boolean isLegacyRedisHandoffOverflowed() {
		return legacyRedisHandoffOverflowed;
	}

	public synchronized void activateRedisSubscriber(Object subscriber) {
		if (standbyRedisSubscriber != subscriber) {
			throw new IllegalStateException("Redis replacement subscriber is not registered");
		}
		activeRedisSubscriber = subscriber;
		standbyRedisSubscriber = null;
	}

	/** Restores the retired active listener after a promoted replacement is rolled back. */
	public synchronized void restoreRedisSubscriber(Object subscriber) {
		activeRedisSubscriber = subscriber;
		standbyRedisSubscriber = null;
		legacyRedisDeliveries.clear();
		legacyRedisDeliveryBytes = 0;
		legacyRedisHandoffOverflowed = false;
	}

	/** Consumes one matching delivery processed by the previous active subscriber. */
	public synchronized boolean consumeLegacyRedisDelivery(String signature) {
		Integer count = legacyRedisDeliveries.get(signature);
		if (count == null) return false;
		if (count <= 1) {
			legacyRedisDeliveries.remove(signature);
			legacyRedisDeliveryBytes -= legacyRedisDeliveryBytes(signature);
		} else legacyRedisDeliveries.put(signature, count - 1);
		return true;
	}

	public synchronized void finishRedisHandoff() {
		legacyRedisDeliveries.clear();
		legacyRedisDeliveryBytes = 0;
		legacyRedisHandoffOverflowed = false;
	}

	public synchronized void unregisterRedisSubscriber(Object subscriber) {
		if (standbyRedisSubscriber == subscriber) {
			standbyRedisSubscriber = null;
			legacyRedisDeliveries.clear();
			legacyRedisDeliveryBytes = 0;
			legacyRedisHandoffOverflowed = false;
		}
		if (activeRedisSubscriber == subscriber) activeRedisSubscriber = null;
	}

	private void cleanup(long now) {
		processedVotes.forEach((voteId, expiresAt) -> {
			if (expiresAt <= now && processedVotes.remove(voteId, expiresAt)) completedVotes.remove(voteId);
		});
	}

	/** Returns an exact UTF-8 length up to the per-delivery cap, then cap + 1. */
	public static int legacyRedisDeliveryBytes(String signature) {
		if (signature == null) return 0;
		int bytes = 0;
		for (int index = 0; index < signature.length(); index++) {
			char character = signature.charAt(index);
			if (character <= 0x7f) bytes++;
			else if (character <= 0x7ff) bytes += 2;
			else if (Character.isHighSurrogate(character) && index + 1 < signature.length()
					&& Character.isLowSurrogate(signature.charAt(index + 1))) {
				bytes += 4;
				index++;
			} else bytes += 3;
			if (bytes > MAX_LEGACY_REDIS_DELIVERY_BYTES) return MAX_LEGACY_REDIS_DELIVERY_BYTES + 1;
		}
		return bytes;
	}
}
