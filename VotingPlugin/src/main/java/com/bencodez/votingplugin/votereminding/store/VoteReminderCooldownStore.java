package com.bencodez.votingplugin.votereminding.store;

import java.util.Map;
import java.util.UUID;

public interface VoteReminderCooldownStore {
	boolean tryClaimGlobal(UUID uuid, long nowMs, long globalCooldownMs);

	Map<String, Long> getPerReminderMap(UUID uuid);

	void setPerReminderLast(UUID uuid, String reminderName, long nowMs);

	boolean isPersistent(UUID uuid);
}
