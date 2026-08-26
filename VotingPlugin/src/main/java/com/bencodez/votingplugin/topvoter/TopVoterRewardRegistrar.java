package com.bencodez.votingplugin.topvoter;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined top-voter award reward paths. */
public final class TopVoterRewardRegistrar {

	private TopVoterRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		for (String path : plugin.getSpecialRewardsConfig().getMonthlyPossibleRewardPlaces()) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					plugin.getSpecialRewardsConfig().getMonthlyAwardRewardsPath(path), plugin.getSpecialRewardsConfig()));
		}
		for (String path : plugin.getSpecialRewardsConfig().getWeeklyPossibleRewardPlaces()) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					plugin.getSpecialRewardsConfig().getWeeklyAwardRewardsPath(path), plugin.getSpecialRewardsConfig()));
		}
		for (String path : plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces()) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					plugin.getSpecialRewardsConfig().getDailyAwardRewardsPath(path), plugin.getSpecialRewardsConfig()));
		}
	}
}
