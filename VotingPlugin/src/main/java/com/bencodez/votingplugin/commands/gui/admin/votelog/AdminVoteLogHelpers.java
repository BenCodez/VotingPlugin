package com.bencodez.votingplugin.commands.gui.admin.votelog;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.bukkit.Material;

import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;

public class AdminVoteLogHelpers {
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

	public static String safe(String s) {
		return s == null ? "" : s;
	}

	public static boolean notEmpty(String s) {
		return s != null && !s.trim().isEmpty();
	}

	public static String formatTime(long millis) {
		try {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			return sdf.format(new Date(millis));
		} catch (Exception e) {
			return String.valueOf(millis);
		}
	}

	public static String shortUuid(String uuid) {
		if (uuid == null || uuid.isEmpty()) {
			return "unknown";
		}
		return uuid.length() > 8 ? uuid.substring(0, 8) : uuid;
	}

}
