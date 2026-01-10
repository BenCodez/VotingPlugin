package com.bencodez.votingplugin.specialrewards.votemilestones;

public enum VoteMilestoneGroupSelect {
	/**
	 * Trigger every matching milestone in the group.
	 */
	ALL,

	/**
	 * If multiple milestones in the group match, only trigger the highest
	 * "priority" (largest AT value, else largest EVERY).
	 */
	HIGHEST,

	/**
	 * If multiple match, only trigger the first one encountered (stable ordering).
	 */
	FIRST;
}
