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

	public VoteStreakDefinition(String id, VoteStreakType type, boolean enabled, int requiredAmount, int votesRequired,
			int allowMissedAmount, int allowMissedPeriod) {
		this.id = id;
		this.type = type;
		this.enabled = enabled;
		this.requiredAmount = requiredAmount;
		this.allowMissedAmount = Math.max(0, allowMissedAmount);
		this.allowMissedPeriod = Math.max(0, allowMissedPeriod);
		this.votesRequired = Math.max(1, votesRequired);
	}

}
