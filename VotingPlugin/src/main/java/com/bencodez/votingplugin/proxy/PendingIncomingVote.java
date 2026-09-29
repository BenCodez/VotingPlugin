package com.bencodez.votingplugin.proxy;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import lombok.Getter;

/**
 * A Votifier event owned by the platform while it waits for a usable proxy
 * runtime. Scheduler tasks are only wakeups; this object remains registered
 * until processing completes or the vote is handed to durable storage.
 */
public final class PendingIncomingVote {
	@Getter
	private final UUID voteId;
	@Getter
	private final String player;
	@Getter
	private final String service;
	@Getter
	private final long acceptedAt;
	private final AtomicBoolean processing = new AtomicBoolean();
	private final AtomicBoolean scheduled = new AtomicBoolean();
	@Getter
	private int storageAttempts;
	private int durableHandoffAttempts;

	public PendingIncomingVote(UUID voteId, String player, String service, long acceptedAt) {
		this.voteId = voteId;
		this.player = player;
		this.service = service;
		this.acceptedAt = acceptedAt;
	}

	/** Prevents duplicate scheduler wakeups from processing the same vote concurrently. */
	public boolean beginProcessing() {
		return processing.compareAndSet(false, true);
	}

	public void endProcessing() {
		processing.set(false);
	}

	public boolean beginScheduling() {
		return scheduled.compareAndSet(false, true);
	}

	public void endScheduling() {
		scheduled.set(false);
	}

	public int incrementStorageAttempts() {
		return ++storageAttempts;
	}

	/** Returns a capped backoff for retrying transfer into durable ownership. */
	public long nextDurableHandoffDelaySeconds() {
		long delay = 5L << Math.min(durableHandoffAttempts++, 4);
		return Math.min(delay, 60L);
	}
}
