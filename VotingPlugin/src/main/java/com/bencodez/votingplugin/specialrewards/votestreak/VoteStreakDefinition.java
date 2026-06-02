package com.bencodez.votingplugin.specialrewards.votestreak;

import lombok.Getter;

public final class VoteStreakDefinition {

	@Getter
	private final String id;
	@Getter
	private final VoteStreakType type;
	@Getter
	private final boolean enabled;

	@Getter
	private final int requiredAmount;
	@Getter
	private final int allowMissedAmount;
	@Getter
	private final int allowMissedPeriod;

	@Getter
	private final int votesRequired;

	@Getter
	private final boolean recurring;
	@Getter
	private final String progressGroup;
	@Getter
	private final String rewardPath;

	public VoteStreakDefinition(String id, VoteStreakType type, boolean enabled, int requiredAmount, int votesRequired,
			int allowMissedAmount, int allowMissedPeriod, boolean recurring) {
		this(id, type, enabled, requiredAmount, votesRequired, allowMissedAmount, allowMissedPeriod, recurring, "",
				"VoteStreaks." + id + ".Rewards");
	}

	public VoteStreakDefinition(String id, VoteStreakType type, boolean enabled, int requiredAmount, int votesRequired,
			int allowMissedAmount, int allowMissedPeriod, boolean recurring, String progressGroup) {
		this(id, type, enabled, requiredAmount, votesRequired, allowMissedAmount, allowMissedPeriod, recurring,
				progressGroup, "VoteStreaks." + id + ".Rewards");
	}

	public VoteStreakDefinition(String id, VoteStreakType type, boolean enabled, int requiredAmount, int votesRequired,
			int allowMissedAmount, int allowMissedPeriod, boolean recurring, String progressGroup, String rewardPath) {
		this.id = id;
		this.type = type;
		this.enabled = enabled;
		this.requiredAmount = requiredAmount;
		this.allowMissedAmount = Math.max(0, allowMissedAmount);
		this.allowMissedPeriod = Math.max(0, allowMissedPeriod);
		this.votesRequired = Math.max(1, votesRequired);
		this.recurring = recurring;
		this.progressGroup = progressGroup == null ? "" : progressGroup.trim();
		this.rewardPath = rewardPath == null || rewardPath.trim().isEmpty() ? "VoteStreaks." + id + ".Rewards"
				: rewardPath.trim();
	}

}
