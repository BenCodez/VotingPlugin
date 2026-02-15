package com.bencodez.votingplugin.commands.gui.admin.votelog;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.bukkit.Material;

import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;

/**
 * Helper class for admin vote log GUI operations.
 */
public class AdminVoteLogHelpers {
	/**
	 * Get the material icon for a vote log event.
	 *
	 * @param event the vote log event
	 * @param status the status of the vote
	 * @return the material to use for the icon
	 */
	public static Material getMaterialForEvent(VoteLogMysqlTable.VoteLogEvent event, String status) {
		// Cached entries still override to chest (keeps your current behavior)
		if ("CACHED".equalsIgnoreCase(status)) {
			return Material.CHEST;
		}

		if (event == null) {
			return Material.PAPER;
		}

		switch (event) {
		case VOTE_RECEIVED:
			return Material.EMERALD;

		case VOTEMILESTONE:
			return Material.ANVIL;

		case VOTE_STREAK_REWARD:
			return Material.FIREWORK_ROCKET;

		case TOP_VOTER_REWARD:
			return Material.GOLD_BLOCK;

		default:
			return Material.PAPER;
		}
	}

	/**
	 * Get the color code for a vote log event.
	 *
	 * @param event the vote log event
	 * @return the color code string
	 */
	public static String getEventColor(VoteLogMysqlTable.VoteLogEvent event) {
		if (event == null) {
			return "&7";
		}

		switch (event) {
		case VOTE_RECEIVED:
			return "&a";
		case VOTEMILESTONE:
			return "&6";

		case VOTE_STREAK_REWARD:
			return "&d";

		case TOP_VOTER_REWARD:
			return "&c";

		default:
			return "&7";
		}
	}

	/**
	 * Returns a safe string, converting null to empty string.
	 *
	 * @param s the string to check
	 * @return the string or empty string if null
	 */
	public static String safe(String s) {
		return s == null ? "" : s;
	}

	/**
	 * Checks if a string is not null and not empty.
	 *
	 * @param s the string to check
	 * @return true if the string is not empty
	 */
	public static boolean notEmpty(String s) {
		return s != null && !s.trim().isEmpty();
	}

	/**
	 * Formats a timestamp in milliseconds to a readable date string.
	 *
	 * @param millis the timestamp in milliseconds
	 * @return the formatted date string
	 */
	public static String formatTime(long millis) {
		try {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			return sdf.format(new Date(millis));
		} catch (Exception e) {
			return String.valueOf(millis);
		}
	}

	/**
	 * Returns a shortened version of a UUID string.
	 *
	 * @param uuid the UUID string
	 * @return the first 8 characters of the UUID or "unknown"
	 */
	public static String shortUuid(String uuid) {
		if (uuid == null || uuid.isEmpty()) {
			return "unknown";
		}
		return uuid.length() > 8 ? uuid.substring(0, 8) : uuid;
	}

}
