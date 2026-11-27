package com.bencodez.votingplugin.config;

import java.util.ArrayList;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;

/** Reports configuration problems at startup and reload. */
public final class VotingPluginConfigHealth {

	private final VotingPluginMain plugin;
	private boolean ymlError;

	public VotingPluginConfigHealth(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void check() {
		Config config = plugin.getConfigFile();
		SpecialRewardsConfig rewards = plugin.getSpecialRewardsConfig();
		boolean configReadFailed = config.isFailedToRead() || rewards.isFailedToRead();
		ymlError = configReadFailed || plugin.getConfigVoteSites().isFailedToRead()
				|| plugin.getBungeeSettings().isFailedToRead()
				|| plugin.getGui().isFailedToRead();

		if (ymlError) {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin,
					() -> plugin.getLogger().severe("Failed to load a file, check startup log"), 1);
		}
		if (!configReadFailed) {
			if (rewards.isEnableMonthlyAwards() && !config.isLoadTopVoterMonthly()
					&& !config.isUseMonthDateTotalsAsPrimaryTotal()) {
				warnIfAwardsConfigured(TopVoter.Monthly, rewards.getData(), rewards.getMonthlyPossibleRewardPlaces(),
						rewards::getMonthlyAwardRewardsPath);
			}
			if (rewards.isEnableWeeklyAwards() && !config.isLoadTopVoterWeekly()) {
				warnIfAwardsConfigured(TopVoter.Weekly, rewards.getData(), rewards.getWeeklyPossibleRewardPlaces(),
						rewards::getWeeklyAwardRewardsPath);
			}
			if (rewards.isEnableDailyRewards() && !config.isLoadTopVoterDaily()) {
				warnIfAwardsConfigured(TopVoter.Daily, rewards.getData(), rewards.getDailyPossibleRewardPlaces(),
						rewards::getDailyAwardRewardsPath);
			}
		}
	}

	private void warnIfAwardsConfigured(TopVoter period, ConfigurationSection data, Set<String> places,
			Function<String, String> rewardPath) {
		// Later entries win in TopVoterRanking. Track their covered ranks as merged ranges.
		TreeMap<Long, Long> coveredRanks = new TreeMap<>();
		for (String place : new ArrayList<>(places).reversed()) {
			AwardRange range = positiveRankRange(place);
			if (range == null) {
				continue;
			}
			long start = range.start();
			long end = range.end();
			var previous = coveredRanks.floorEntry(start);
			if (previous != null && previous.getValue() >= end) {
				continue;
			}
			if (hasReward(data, rewardPath.apply(place))) {
				plugin.getLogger().warning(period.name() + " top-voter awards are configured. Set LoadTopVoter."
						+ period.name() + " to true in Config.yml to load the rankings required for these awards.");
				return;
			}
			if (previous != null && previous.getValue() >= start) {
				start = previous.getKey();
			}
			var next = coveredRanks.ceilingEntry(start);
			while (next != null && next.getKey() <= end) {
				end = Math.max(end, next.getValue());
				coveredRanks.remove(next.getKey());
				next = coveredRanks.ceilingEntry(start);
			}
			coveredRanks.put(start, end);
		}
	}

	private static boolean hasReward(ConfigurationSection data, String path) {
		ConfigurationSection section = data.getConfigurationSection(path);
		return (section != null && !section.getKeys(false).isEmpty())
				|| data.getStringList(path).stream().anyMatch(name -> !name.isBlank())
				|| (data.isString(path) && !data.getString(path, "").isBlank());
	}

	private static AwardRange positiveRankRange(String place) {
		// Match the end-exclusive ranges in TopVoterRanking without expanding them.
		String[] bounds = place.split("-");
		if (bounds.length == 0) {
			return null;
		}
		try {
			int rank = Integer.parseInt(bounds[0]);
			long start = Math.max(rank, 1);
			long end = bounds.length == 1 ? (long) rank + 1 : Integer.parseInt(bounds[1]);
			return start < end ? new AwardRange(start, end) : null;
		} catch (NumberFormatException ignored) {
			return null;
		}
	}

	private record AwardRange(long start, long end) {
	}

	public boolean hasYmlError() {
		return ymlError;
	}
}
