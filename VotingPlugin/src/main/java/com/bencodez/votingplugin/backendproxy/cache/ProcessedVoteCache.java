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

	@Getter
	private final ConcurrentHashMap<UUID, Long> processedVotes = new ConcurrentHashMap<>();
	private final long ttlMillis;
	private final LinkedHashMap<String, Long> processedRedisDeliveries = new LinkedHashMap<>();
	private final LinkedHashMap<String, Integer> legacyRedisDeliveries = new LinkedHashMap<>();
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
			} else if (legacyRedisDeliveries.size() < MAX_REDIS_DELIVERIES) {
				legacyRedisDeliveries.put(signature, 1);
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
		if (count <= 1) legacyRedisDeliveries.remove(signature);
		else legacyRedisDeliveries.put(signature, count - 1);
		return true;
	}

	public synchronized void finishRedisHandoff() {
		legacyRedisDeliveries.clear();
	}

	public synchronized void unregisterRedisSubscriber(Object subscriber) {
		if (standbyRedisSubscriber == subscriber) {
			standbyRedisSubscriber = null;
			legacyRedisDeliveries.clear();
		}
		if (activeRedisSubscriber == subscriber) activeRedisSubscriber = null;
	}

	private void cleanup(long now) {
		processedVotes.entrySet().removeIf(entry -> entry.getValue() <= now);
	}
}
