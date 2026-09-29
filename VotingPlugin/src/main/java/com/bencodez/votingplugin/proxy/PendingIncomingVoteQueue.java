package com.bencodez.votingplugin.proxy;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/** Thread-safe process-lifetime ownership for accepted Votifier events. */
public final class PendingIncomingVoteQueue {
	private static final int MAX_PENDING = 4096;
	private final Map<UUID, PendingIncomingVote> votes = new LinkedHashMap<>();
	private boolean accepting = true;

	public synchronized PendingIncomingVote admit(String player, String service) {
		if (!accepting || votes.size() >= MAX_PENDING) return null;
		PendingIncomingVote vote = new PendingIncomingVote(UUID.randomUUID(), player, service,
				System.currentTimeMillis());
		votes.put(vote.getVoteId(), vote);
		return vote;
	}

	public synchronized boolean contains(UUID voteId) {
		return votes.containsKey(voteId);
	}

	public synchronized void complete(PendingIncomingVote vote) {
		votes.remove(vote.getVoteId(), vote);
	}

	public synchronized List<PendingIncomingVote> snapshot() {
		return new ArrayList<>(votes.values());
	}

	public synchronized int size() {
		return votes.size();
	}

	/** Prevents shutdown from missing a vote that was admitted concurrently. */
	public synchronized void closeAdmission() {
		accepting = false;
	}

	public synchronized boolean isAccepting() {
		return accepting;
	}
}
