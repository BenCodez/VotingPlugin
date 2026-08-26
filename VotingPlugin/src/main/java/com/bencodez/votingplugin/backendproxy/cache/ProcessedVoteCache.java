package com.bencodez.votingplugin.backendproxy.cache;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import lombok.Getter;

/**
 * Tracks recently processed proxy vote ids so duplicate delivery is ignored.
 */
public class ProcessedVoteCache {

	private static final long DEFAULT_TTL_MILLIS = TimeUnit.MINUTES.toMillis(30);

	@Getter
	private final ConcurrentHashMap<UUID, Long> processedVotes = new ConcurrentHashMap<>();
	private final long ttlMillis;

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

	private void cleanup(long now) {
		processedVotes.entrySet().removeIf(entry -> entry.getValue() <= now);
	}
}
