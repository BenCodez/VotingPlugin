package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.LinkedHashMap;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;

/**
 * Legacy migration: - Uses SpecialRewards.yml root as the reward container so
 * reward paths can be absolute. - FirstVote / FirstVoteToday: rewards are on
 * the section itself (no "Rewards" child) - Cumulative / MileStones: rewards
 * are under "...Rewards" - Totals are mapped into VoteMilestoneTotal (votes +
 * points + legacy counter)
 *
 * Adds per-milestone Limit support (default NONE).
 */
public final class VoteMilestonesMigrator {

	private VoteMilestonesMigrator() {
	}

	public static Map<String, VoteMilestone> compileLegacy(ConfigurationSection root, int totalSites,
			boolean onlyOneCumulative, boolean resetMilestonesMonthly) {
		Map<String, VoteMilestone> out = new LinkedHashMap<>();
		if (root == null) {
			return out;
		}

		compileFirstVote(root, out);
		compileFirstVoteToday(root, out);
		compileCumulative(root, out, onlyOneCumulative);
		compileMilestones(root, out, resetMilestonesMonthly);

		compileAllSites(root, out, totalSites);
		compileAlmostAllSites(root, out, totalSites);

		return out;
	}

	private static void compileFirstVote(ConfigurationSection root, Map<String, VoteMilestone> out) {
		ConfigurationSection sec = root.getConfigurationSection("FirstVote");
		if (sec == null || isDisabledObject(sec)) {
			return;
		}

		// rewards are on FirstVote section itself (legacy)
		VoteMilestone vm = VoteMilestoneFactory.atZero("Legacy_FirstVote", true, VoteMilestoneTotal.ALLTIME_VOTES,
				"FirstVote", VoteMilestoneGroupSelect.ALL, "Default", VoteMilestoneLimit.none());
		out.put(vm.getId(), vm);
	}

	private static void compileFirstVoteToday(ConfigurationSection root, Map<String, VoteMilestone> out) {
		ConfigurationSection sec = root.getConfigurationSection("FirstVoteToday");
		if (sec == null || isDisabledObject(sec)) {
			return;
		}

		// rewards are on FirstVoteToday section itself (legacy)
		VoteMilestone vm = VoteMilestoneFactory.atZero("Legacy_FirstVoteToday", true, VoteMilestoneTotal.DAILY_VOTES,
				"FirstVoteToday", VoteMilestoneGroupSelect.ALL, "Default", VoteMilestoneLimit.none());
		out.put(vm.getId(), vm);
	}

	private static void compileCumulative(ConfigurationSection root, Map<String, VoteMilestone> out,
			boolean onlyOneCumulative) {
		VoteMilestoneGroupSelect groupSelect = onlyOneCumulative ? VoteMilestoneGroupSelect.HIGHEST
				: VoteMilestoneGroupSelect.ALL;
		ConfigurationSection sec = root.getConfigurationSection("Cumulative");
		if (sec == null || isDisabledObject(sec)) {
			return;
		}

		for (String key : sec.getKeys(false)) {
			ConfigurationSection c = sec.getConfigurationSection(key);
			if (c == null) {
				continue;
			}
			if (!c.getBoolean("Enabled", true)) {
				continue;
			}

			int every;
			try {
				every = Integer.parseInt(key.replace("'", "").trim());
			} catch (Exception e) {
				continue;
			}
			if (every <= 0) {
				continue;
			}

			VoteMilestoneTotal total = mapTotalToUse(c.getString("TotalToUse", "AllTime"));

			// rewards are under Cumulative.<key>.Rewards in legacy config
			VoteMilestone vm = VoteMilestoneFactory.every("Legacy_Cumulative_" + every + "_" + total.name(), true,
					total, every, "Cumulative." + key + ".Rewards", groupSelect, "Legacy_Cumulative",
					VoteMilestoneLimit.none());

			out.put(vm.getId(), vm);
		}
	}

	private static void compileMilestones(ConfigurationSection root, Map<String, VoteMilestone> out,
			boolean resetMonthly) {
		ConfigurationSection sec = root.getConfigurationSection("MileStones");
		if (sec == null || isDisabledObject(sec)) {
			return;
		}

		for (String key : sec.getKeys(false)) {
			ConfigurationSection m = sec.getConfigurationSection(key);
			if (m == null) {
				continue;
			}
			if (!m.getBoolean("Enabled", true)) {
				continue;
			}

			long at;
			try {
				at = Long.parseLong(key.replace("'", "").trim());
			} catch (Exception e) {
				continue;
			}
			if (at <= 0) {
				continue;
			}

			// if resetMonthy is true use month total
			VoteMilestoneTotal totalToUse = resetMonthly ? VoteMilestoneTotal.MONTHLY_VOTES
					: VoteMilestoneTotal.ALLTIME_VOTES;

			// rewards are under MileStones.<key>.Rewards in legacy config
			VoteMilestone vm = VoteMilestoneFactory.atExact("Legacy_Milestone_" + at + "_ALLTIME", true, totalToUse, at,
					"MileStones." + key + ".Rewards", VoteMilestoneGroupSelect.ALL, "Default",
					VoteMilestoneLimit.none());

			out.put(vm.getId(), vm);
		}
	}

	private static void compileAllSites(ConfigurationSection root, Map<String, VoteMilestone> out, int totalSites) {
		String name = "AllSites";

		// Trigger when "unique sites voted today" reaches required number of sites
		long at = Math.max(1, totalSites);

		// IMPORTANT: ALLSITES_TODAY stays true after you hit it, so legacy AllSites
		// must be once-per-day
		VoteMilestoneLimit limit = new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0L, 0);

		VoteMilestone vm = VoteMilestoneFactory.atExact("Legacy_" + name, true, VoteMilestoneTotal.ALLSITES_TODAY, at,
				name, VoteMilestoneGroupSelect.ALL, "Default", limit);
		out.put(vm.getId(), vm);
	}

	private static void compileAlmostAllSites(ConfigurationSection root, Map<String, VoteMilestone> out,
			int totalSites) {
		String name = "AlmostAllSites";

		// "almost all" = totalSites - 1 (minimum 1)
		long at = Math.max(1, totalSites - 1);

		// Same issue as AllSites: once reached for the day, it would keep matching
		// without a limiter
		VoteMilestoneLimit limit = new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0L, 0);

		VoteMilestone vm = VoteMilestoneFactory.atExact("Legacy_" + name, true, VoteMilestoneTotal.ALLSITES_TODAY, at,
				name, VoteMilestoneGroupSelect.ALL, "Default", limit);
		out.put(vm.getId(), vm);
	}

	private static VoteMilestoneTotal mapTotalToUse(String s) {
		String v = (s == null ? "ALLTIME" : s).trim().toUpperCase();
		switch (v) {
		case "ALLTIME":
		case "ALL_TIME":
			return VoteMilestoneTotal.ALLTIME_VOTES;
		case "DAILY":
		case "DAY":
			return VoteMilestoneTotal.DAILY_VOTES;
		case "WEEKLY":
		case "WEEK":
			return VoteMilestoneTotal.WEEKLY_VOTES;
		case "MONTHLY":
		case "MONTH":
			return VoteMilestoneTotal.MONTHLY_VOTES;
		default:
			return VoteMilestoneTotal.ALLTIME_VOTES;
		}
	}

	private static boolean isDisabledObject(ConfigurationSection sec) {
		return sec.getKeys(false).isEmpty();
	}
}
