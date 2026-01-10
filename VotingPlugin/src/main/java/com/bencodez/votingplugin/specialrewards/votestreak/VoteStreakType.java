package com.bencodez.votingplugin.specialrewards.votestreak;

public enum VoteStreakType {
	DAILY,
	WEEKLY,
	MONTHLY;

	public static VoteStreakType from(String s) {
		try {
			return VoteStreakType.valueOf(s.trim().toUpperCase());
		} catch (Exception e) {
			throw new IllegalArgumentException("Invalid VoteStreak Type: " + s + " (expected DAILY/WEEKLY/MONTHLY)");
		}
	}
}
