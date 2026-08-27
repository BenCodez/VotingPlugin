package com.bencodez.votingplugin.backendproxy.cache;

import java.util.UUID;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
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

	private void cleanup(long now) {
		processedVotes.entrySet().removeIf(entry -> entry.getValue() <= now);
	}
}
