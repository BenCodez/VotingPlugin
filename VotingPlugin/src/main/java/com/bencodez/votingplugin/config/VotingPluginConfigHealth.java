package com.bencodez.votingplugin.config;

import java.util.Set;
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
		for (String place : places) {
			if (!includesPositiveRank(place)) {
				continue;
			}
			String path = rewardPath.apply(place);
			ConfigurationSection section = data.getConfigurationSection(path);
			if ((section != null && !section.getKeys(false).isEmpty())
					|| !data.getStringList(path).isEmpty()
					|| (data.isString(path) && !data.getString(path, "").isBlank())) {
				plugin.getLogger().warning(period.name() + " top-voter awards are configured. Set LoadTopVoter."
						+ period.name() + " to true in Config.yml to load the rankings required for these awards.");
				return;
			}
		}
	}

	private static boolean includesPositiveRank(String place) {
		// Match the end-exclusive ranges in TopVoterRanking without expanding them.
		String[] bounds = place.split("-");
		if (bounds.length == 0) {
			return false;
		}
		try {
			int first = Integer.parseInt(bounds[0]);
			return bounds.length == 1 ? first > 0 : Math.max(first, 1) < Integer.parseInt(bounds[1]);
		} catch (NumberFormatException ignored) {
			return false;
		}
	}

	public boolean hasYmlError() {
		return ymlError;
	}
}
