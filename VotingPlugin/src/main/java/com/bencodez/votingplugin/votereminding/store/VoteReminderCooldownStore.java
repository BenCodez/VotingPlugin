package com.bencodez.votingplugin.votereminding.store;

import java.util.Map;
import java.util.UUID;

/**
 * Interface for storing and managing vote reminder cooldowns.
 */
public interface VoteReminderCooldownStore {
	/**
	 * Attempts to claim global cooldown for a player.
	 * @param uuid the player UUID
	 * @param nowMs current time in milliseconds
	 * @param globalCooldownMs global cooldown in milliseconds
	 * @return true if cooldown was claimed
	 */
	boolean tryClaimGlobal(UUID uuid, long nowMs, long globalCooldownMs);

	/**
	 * Gets per-reminder cooldown map for a player.
	 * @param uuid the player UUID
	 * @return map of reminder names to last execution times
	 */
	Map<String, Long> getPerReminderMap(UUID uuid);

	/**
	 * Sets the last execution time for a specific reminder.
	 * @param uuid the player UUID
	 * @param reminderName the reminder name
	 * @param nowMs current time in milliseconds
	 */
	void setPerReminderLast(UUID uuid, String reminderName, long nowMs);

	/**
	 * Checks if player data is persistent.
	 * @param uuid the player UUID
	 * @return true if data is persistent
	 */
	boolean isPersistent(UUID uuid);
}
