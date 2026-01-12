package com.bencodez.votingplugin.broadcast;

import java.util.Locale;

/**
 * Defines WHEN vote broadcasts occur.
 *
 * Set Type: NONE to disable broadcasting entirely.
 */
public enum VoteBroadcastType {

	NONE,

	/**
	 * Broadcast on every vote (even if the player is offline). Uses
	 * Format.BroadcastMsg (+ optional header/list if you choose).
	 */
	EVERY_VOTE,

	/**
	 * Broadcast on every vote ONLY if the voting player is online. Useful when
	 * processing offline votes but you don't want global spam for them.
	 */
	EVERY_VOTE_ONLINE_ONLY,

	/**
	 * Broadcast at most once per Duration per player. (Even if they vote multiple
	 * sites inside the cooldown.)
	 */
	COOLDOWN_PER_PLAYER,

	/**
	 * Batch sites per player for Duration, then broadcast one message listing
	 * sites.
	 */
	BATCH_WINDOW_PER_PLAYER,

	/**
	 * Broadcast only the first vote of the day per player (calendar day).
	 */
	FIRST_VOTE_OF_DAY,

	/**
	 * Every Duration, broadcast a global summary of who voted and how many sites.
	 * Uses context placeholders: %players%, %numberofplayers%, %sites%,
	 * %numberofsites%.
	 */
	INTERVAL_SUMMARY_GLOBAL;

	public static VoteBroadcastType parse(String s, VoteBroadcastType def) {
		if (s == null || s.trim().isEmpty()) {
			return def;
		}
		try {
			return VoteBroadcastType.valueOf(s.trim().toUpperCase(Locale.ROOT));
		} catch (Exception ignored) {
			return def;
		}
	}
}
