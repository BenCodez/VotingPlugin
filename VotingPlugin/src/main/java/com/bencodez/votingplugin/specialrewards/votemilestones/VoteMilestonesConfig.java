package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.LinkedHashMap;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;

public class VoteMilestonesConfig {

	private final Map<String, VoteMilestone> milestones;

	private VoteMilestonesConfig(Map<String, VoteMilestone> milestones) {
		this.milestones = milestones;
	}

	public Map<String, VoteMilestone> getMilestones() {
		return milestones;
	}

	public static VoteMilestonesConfig load(VotingPluginMain plugin, VoteMilestonesManager manager,
			ConfigurationSection root) {
		Map<String, VoteMilestone> out = new LinkedHashMap<>();

		// Load new format entries (VoteMilestones section)
		ConfigurationSection voteMilestonesRoot = root.getConfigurationSection("VoteMilestones");
		if (voteMilestonesRoot != null) {
			for (String id : voteMilestonesRoot.getKeys(false)) {
				ConfigurationSection sec = voteMilestonesRoot.getConfigurationSection(id);
				if (sec == null) {
					continue;
				}

				boolean enabled = sec.getBoolean("Enabled", true);

				String totalStr = sec.getString("Total", "ALLTIME_VOTES");
				VoteMilestoneTotal total = VoteMilestoneTotal.parse(totalStr);

				String groupSelectStr = sec.getString("Group", "Default");

				AtMatcher at = AtMatcher.fromConfig(sec, "At");
				Integer every = sec.contains("Every") ? sec.getInt("Every") : null;

				// NEW: per-milestone limit settings (default NONE)
				VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(sec);

				// New config: reward container is the milestone section itself
				VoteMilestone m = new VoteMilestone(id, enabled, total, at, every, "VoteMilestones." + id + ".Rewards",
						manager.getGroupModes().getOrDefault(groupSelectStr, VoteMilestoneGroupSelect.ALL),
						groupSelectStr, limit);

				out.put(id, m);
			}
		}

		// Merge legacy compiled milestones (new format wins on ID collision)
		ConfigurationSection legacyRoot = plugin.getSpecialRewardsConfig().getData();
		int totalSites = plugin.getVoteSitesEnabled() != null ? plugin.getVoteSitesEnabled().size() : 0;

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(legacyRoot, totalSites,
				plugin.getSpecialRewardsConfig().isOnlyOneCumulative(),
				plugin.getSpecialRewardsConfig().isResetMilestonesMonthly());
		plugin.debug(plugin.getSpecialRewardsConfig().isOnlyOneCumulative() + " Only one cumulative");
		for (Map.Entry<String, VoteMilestone> e : legacy.entrySet()) {
			out.putIfAbsent(e.getKey(), e.getValue());
		}

		return new VoteMilestonesConfig(out);
	}
}
