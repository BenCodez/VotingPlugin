package com.bencodez.votingplugin.specialrewards.votemilestones;

import org.bukkit.configuration.MemoryConfiguration;

final class VoteMilestoneFactory {

	private VoteMilestoneFactory() {
	}

	static VoteMilestone atZero(String id, boolean enabled, VoteMilestoneTotal total, String rewardPath,
			VoteMilestoneGroupSelect groupSelect, String voteMilestoneGroupKey, VoteMilestoneLimit limit) {
		MemoryConfiguration tmp = new MemoryConfiguration();
		tmp.set("At", 1);

		AtMatcher at = AtMatcher.fromConfig(tmp, "At");
		return new VoteMilestone(id, enabled, total, at, null, rewardPath, groupSelect, voteMilestoneGroupKey, limit);
	}

	static VoteMilestone atExact(String id, boolean enabled, VoteMilestoneTotal total, long atValue, String rewardPath,
			VoteMilestoneGroupSelect groupSelect, String voteMilestoneGroupKey, VoteMilestoneLimit limit) {
		MemoryConfiguration tmp = new MemoryConfiguration();
		tmp.set("At", atValue);

		AtMatcher at = AtMatcher.fromConfig(tmp, "At");
		return new VoteMilestone(id, enabled, total, at, null, rewardPath, groupSelect, voteMilestoneGroupKey, limit);
	}

	static VoteMilestone every(String id, boolean enabled, VoteMilestoneTotal total, int every, String rewardPath,
			VoteMilestoneGroupSelect groupSelect, String voteMilestoneGroupKey, VoteMilestoneLimit limit) {
		return new VoteMilestone(id, enabled, total, null, every, rewardPath, groupSelect, voteMilestoneGroupKey, limit);
	}
}
