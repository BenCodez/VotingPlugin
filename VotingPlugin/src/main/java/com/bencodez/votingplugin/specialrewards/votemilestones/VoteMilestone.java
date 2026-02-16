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

	/**
	 * Constructs a new VoteMilestone.
	 *
	 * @param id milestone id
	 * @param enabled whether milestone is enabled
	 * @param total total type to check
	 * @param atMatcher matcher for specific values (may be null)
	 * @param every interval for repeating milestone (may be null)
	 * @param rewardPath path to rewards configuration
	 * @param groupSelect group selection mode
	 * @param groupKey group key
	 * @param limit retrigger limit
	 */
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

	/**
	 * Checks if this milestone has an at matcher for debugging.
	 *
	 * @return true if at matcher exists
	 */
	public boolean hasAtMatcherDebug() {
		return atMatcher != null;
	}

	/**
	 * Gets debug string representation of the at matcher.
	 *
	 * @param maxValues maximum number of values to include
	 * @return debug string or "none"
	 */
	public String getAtMatcherDebugString(int maxValues) {
		return atMatcher == null ? "none" : atMatcher.toDebugString(maxValues);
	}

	/**
	 * Checks if this milestone matches the given total.
	 *
	 * @param newTotal total to check against
	 * @return true if milestone matches
	 */
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
