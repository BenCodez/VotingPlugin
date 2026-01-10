package com.bencodez.votingplugin.specialrewards.votemilestones;

import lombok.Getter;

/**
 * Represents a single VoteMilestone entry.
 *
 * rewardPath:
 * - Absolute path from SpecialRewards.yml root (e.g.
 * "VoteMilestones.MyMilestone.Rewards", "Cumulative.20.Rewards", "FirstVote")
 */
public class VoteMilestone {

	@Getter
	private final String id;
	@Getter
	private final boolean enabled;
	@Getter
	private final VoteMilestoneTotal total;

	@Getter
	private final AtMatcher atMatcher; // may be null
	@Getter
	private final Integer every; // may be null

	@Getter
	private final String rewardPath;

	@Getter
	private final VoteMilestoneGroupSelect groupSelect;

	@Getter
	private final String groupKey;

	@Getter
	private final VoteMilestoneLimit limit; 

	public VoteMilestone(String id, boolean enabled, VoteMilestoneTotal total, AtMatcher atMatcher, Integer every,
			String rewardPath, VoteMilestoneGroupSelect groupSelect, String groupKey, VoteMilestoneLimit limit) {
		this.id = id;
		this.enabled = enabled;
		this.total = total;
		this.atMatcher = atMatcher;
		this.every = every;
		this.rewardPath = rewardPath;
		this.groupSelect = groupSelect;
		this.groupKey = groupKey;
		this.limit = (limit == null ? VoteMilestoneLimit.none() : limit);
	}

	public boolean hasAtMatcherDebug() {
		return atMatcher != null;
	}

	public String getAtMatcherDebugString(int maxValues) {
		return atMatcher == null ? "none" : atMatcher.toDebugString(maxValues);
	}

	public boolean matches(long newTotal) {
		if (atMatcher != null && atMatcher.matches(newTotal)) {
			return true;
		}

		if (every != null && every > 0) {
			return newTotal > 0 && (newTotal % every == 0);
		}

		return false;
	}
}
