package com.bencodez.votingplugin;

import com.bencodez.advancedcore.api.metrics.BStatsMetrics;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.votingplugin.user.UserManager;

public class VotingPluginMetrics {

	public void load(VotingPluginMain plugin) {
		BStatsMetrics metrics = new BStatsMetrics(plugin);

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_firstvote") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						plugin.getSpecialRewardsConfig().getFirstVoteRewardsPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_everysite") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(plugin.getConfigVoteSites().getData(),
						plugin.getConfigVoteSites().getEverySiteRewardPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_allsites") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						plugin.getSpecialRewardsConfig().getAllSitesRewardPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_cumulative") {

			@Override
			public String getValue() {
				if (plugin.getSpecialRewardsConfig().getCumulativeVotes().size() == 0) {
					return "False";
				} else {
					for (String cum : plugin.getSpecialRewardsConfig().getCumulativeVotes()) {
						if (plugin.getSpecialRewardsConfig().getCumulativeRewardEnabled(Integer.parseInt(cum))) {
							return "True";
						}
					}
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_voteparty") {

			@Override
			public String getValue() {
				if (!plugin.getSpecialRewardsConfig().getVotePartyEnabled()) {
					return "False";
				} else {
					return "True";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_milestone") {

			@Override
			public String getValue() {
				if (plugin.getSpecialRewardsConfig().getMilestoneVotes().size() == 0) {
					return "False";
				} else {
					for (String milestone : plugin.getSpecialRewardsConfig().getMilestoneVotes()) {
						if (plugin.getSpecialRewardsConfig().getMilestoneRewardEnabled(Integer.parseInt(milestone))) {
							return "True";
						}
					}
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_anysitereward") {

			@Override
			public String getValue() {
				if (RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						plugin.getSpecialRewardsConfig().getAnySiteRewardsPath())) {
					return "True";
				} else {
					return "False";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakday") {

			@Override
			public String getValue() {
				for (String s : plugin.getSpecialRewardsConfig().getVoteStreakVotes("Day")) {

					if (plugin.getSpecialRewardsConfig().getVoteStreakRewardEnabled("Day", s)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath("Day", s))) {
						return "True";
					}

				}
				return "False";
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakweek") {

			@Override
			public String getValue() {
				for (String s : plugin.getSpecialRewardsConfig().getVoteStreakVotes("Week")) {

					if (plugin.getSpecialRewardsConfig().getVoteStreakRewardEnabled("Week", s)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath("Week", s))) {
						return "True";
					}
				}

				return "False";
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votestreakmonth") {

			@Override
			public String getValue() {
				for (String s : plugin.getSpecialRewardsConfig().getVoteStreakVotes("Month")) {

					if (plugin.getSpecialRewardsConfig().getVoteStreakRewardEnabled("Month", s)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath("Month", s))) {
						return "True";
					}
				}

				return "False";
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofsites") {

			@Override
			public String getValue() {
				return "" + plugin.getVoteSites().size();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofrewards") {

			@Override
			public String getValue() {
				return "" + RewardHandler.getInstance().getRewards().size();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("autocreatevotesites") {

			@Override
			public String getValue() {
				return "" + plugin.getConfigFile().isAutoCreateVoteSites();
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofusers") {

			@Override
			public String getValue() {
				int total = UserManager.getInstance().getAllUUIDs().size();
				if (total < 1000) {
					return "<1000";
				} else if (total > 10000) {
					return ">10000";
				} else {
					return "1000-10000";
				}
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("data_storage") {

			@Override
			public String getValue() {
				return plugin.getOptions().getStorageType().toString();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("DisableCheckOnWorldChange") {

			@Override
			public String getValue() {
				return "" + plugin.getOptions().isDisableCheckOnWorldChange();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("votereminding_enabled") {

			@Override
			public String getValue() {
				return "" + plugin.getConfigFile().getVoteRemindingEnabled();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Monthly") {

			@Override
			public String getValue() {
				return "" + plugin.getConfigFile().getLoadTopVoterMonthly();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Weekly") {

			@Override
			public String getValue() {
				return "" + plugin.getConfigFile().getLoadTopVoterWeekly();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Daily") {

			@Override
			public String getValue() {
				return "" + plugin.getConfigFile().getLoadTopVoterDaily();
			}
		});
		metrics.addCustomChart(new BStatsMetrics.SimplePie("bungeemethod") {

			@Override
			public String getValue() {
				if (plugin.getBungeeSettings().isUseBungeecoord()) {
					return "" + plugin.getBungeeHandler().getMethod().toString();
				} else {
					return "Disabled";
				}
			}
		});
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			metrics.addCustomChart(new BStatsMetrics.SimplePie("bungeebroadcast") {

				@Override
				public String getValue() {
					return "" + plugin.getBungeeSettings().isBungeeBroadcast();
				}
			});
			metrics.addCustomChart(new BStatsMetrics.SimplePie("bungeebroadcastalways") {

				@Override
				public String getValue() {
					return "" + plugin.getBungeeSettings().isBungeeBroadcastAlways();
				}
			});

			metrics.addCustomChart(new BStatsMetrics.SimplePie("perserverrewards") {

				@Override
				public String getValue() {
					return "" + plugin.getBungeeSettings().isPerServerRewards();
				}
			});
			metrics.addCustomChart(new BStatsMetrics.SimplePie("perserverpoints") {

				@Override
				public String getValue() {
					return "" + plugin.getBungeeSettings().isPerServerPoints();
				}
			});
			metrics.addCustomChart(new BStatsMetrics.SimplePie("triggervotifierevent") {

				@Override
				public String getValue() {
					return "" + plugin.getBungeeSettings().isTriggerVotifierEvent();
				}
			});
		}

		metrics.addCustomChart(new BStatsMetrics.SimplePie("geyserprefixsupport") {

			@Override
			public String getValue() {
				return "" + plugin.getOptions().isGeyserPrefixSupport();
			}
		});

		metrics.addCustomChart(new BStatsMetrics.SimplePie("using_dev_build") {

			@Override
			public String getValue() {
				return "" + plugin.getProfile().equals("dev");
			}
		});
	}
}
