package com.bencodez.votingplugin.backendproxy.cache;

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
	private final long ttlMillis;
	private final LinkedHashMap<String, Long> processedRedisDeliveries = new LinkedHashMap<>();
	private final LinkedHashMap<String, Integer> legacyRedisDeliveries = new LinkedHashMap<>();
	private long legacyRedisDeliveryBytes;
	private Object activeRedisSubscriber;
	private Object standbyRedisSubscriber;

	public ProcessedVoteCache() {
		this(DEFAULT_TTL_MILLIS);
	}

	public ProcessedVoteCache(long ttlMillis) {
		this.ttlMillis = ttlMillis;
	}

	public boolean reserve(UUID voteId) {
		if (voteId == null) {
			return true;
		}

		long now = System.currentTimeMillis();
		long expiresAt = now + ttlMillis;

		while (true) {
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
				cleanup(now);
				return true;
			}
		}
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

	public synchronized void registerRedisSubscriber(Object subscriber) {
		if (activeRedisSubscriber == null) {
			activeRedisSubscriber = subscriber;
		} else if (activeRedisSubscriber != subscriber) {
			standbyRedisSubscriber = subscriber;
			legacyRedisDeliveries.clear();
			legacyRedisDeliveryBytes = 0;
		}
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
				}
			}
		}
		return true;
	}

	public synchronized void activateRedisSubscriber(Object subscriber) {
		if (standbyRedisSubscriber != subscriber) {
			throw new IllegalStateException("Redis replacement subscriber is not registered");
		}
		activeRedisSubscriber = subscriber;
		standbyRedisSubscriber = null;
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
	}

	public synchronized void unregisterRedisSubscriber(Object subscriber) {
		if (standbyRedisSubscriber == subscriber) {
			standbyRedisSubscriber = null;
			legacyRedisDeliveries.clear();
			legacyRedisDeliveryBytes = 0;
		}
		if (activeRedisSubscriber == subscriber) activeRedisSubscriber = null;
	}

	private void cleanup(long now) {
		processedVotes.entrySet().removeIf(entry -> entry.getValue() <= now);
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
