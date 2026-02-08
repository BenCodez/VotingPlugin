package com.bencodez.votingplugin;

import java.util.concurrent.Callable;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.metrics.BStatsMetrics;

/**
 * Registers bStats charts for VotingPlugin.
 * <p>
 * Updated for VotingPlugin 7.0:
 * <ul>
 * <li>Broadcast: Always logs broadcast_type if VoteBroadcast.Type exists. If Type == NONE, ignores other broadcast
 * settings.</li>
 * <li>VoteLogging: Only logs VoteLogging charts when VoteLogging.Enabled is true.</li>
 * <li>No "Unknown" values are emitted; charts are only registered when their data exists.</li>
 * <li>VoteMilestones chart is bucketed to reduce cardinality.</li>
 * </ul>
 */
public class VotingPluginMetrics {

	/**
	 * Register bStats metrics.
	 *
	 * @param plugin VotingPlugin main instance
	 */
	public void load(final VotingPluginMain plugin) {
		if (plugin == null) {
			return;
		}

		final BStatsMetrics metrics = new BStatsMetrics(plugin, 38);

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_everysite", new Callable<String>() {

			@Override
			public String call() throws Exception {
				if (plugin.getRewardHandler().hasRewards(plugin.getConfigVoteSites().getData(),
						plugin.getConfigVoteSites().getEverySiteRewardPath())) {
					return "True";
				}
				return "False";
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_voteparty", new Callable<String>() {

			@Override
			public String call() throws Exception {
				if (!plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
					return "False";
				}
				return "True";
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("placeholder_cache_level", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return plugin.getPlaceholders().getCacheLevel().toString();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_votemilestones", new Callable<String>() {

			@Override
			public String call() throws Exception {
				int size = plugin.getVoteMilestonesManager().getConfig().getMilestones().size();
				return bucketCount(size);
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("extrarewards_anysitereward", new Callable<String>() {

			@Override
			public String call() throws Exception {
				if (plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						plugin.getSpecialRewardsConfig().getAnySiteRewardsPath())) {
					return "True";
				}
				return "False";
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofsites", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getVoteSiteManager().getVoteSitesEnabled().size();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofrewards", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getRewardHandler().getRewards().size();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("autocreatevotesites", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isAutoCreateVoteSites();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("config_onlinemode", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getOptions().isOnlineMode();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("numberofusers", new Callable<String>() {

			@Override
			public String call() throws Exception {
				int total = plugin.getUserManager().getAllUUIDs().size();
				if (total > 800000) {
					return ">800000";
				}
				if (total > 700000) {
					return "700000-800000";
				}
				if (total > 600000) {
					return "600000-700000";
				} else if (total > 500000) {
					return "500000-600000";
				} else if (total > 400000) {
					return "400000-500000";
				} else if (total > 300000) {
					return "300000-400000";
				} else if (total > 200000) {
					return "200000-300000";
				} else if (total > 100000) {
					return "100000-200000";
				} else if (total > 10000) {
					return ">10000-100000";
				}
				return "<10000";
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("data_storage", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return plugin.getOptions().getStorageType().toString();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("DisableCheckOnWorldChange", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getOptions().isDisableCheckOnWorldChange();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Monthly", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isLoadTopVoterMonthly();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Weekly", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isLoadTopVoterWeekly();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("LoadTopVoter_Daily", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isLoadTopVoterDaily();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("bungeemethod", new Callable<String>() {

			@Override
			public String call() throws Exception {
				if (plugin.getBungeeSettings().isUseBungeecoord()) {
					return "" + plugin.getBungeeHandler().getMethod().toString();
				}
				return "Disabled";
			}
		}));

		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			metrics.addCustomChart(new BStatsMetrics.SimplePie("perserverrewards", new Callable<String>() {

				@Override
				public String call() throws Exception {
					return "" + plugin.getBungeeSettings().isPerServerRewards();
				}
			}));
			metrics.addCustomChart(new BStatsMetrics.SimplePie("perserverpoints", new Callable<String>() {

				@Override
				public String call() throws Exception {
					return "" + plugin.getBungeeSettings().isPerServerPoints();
				}
			}));
			metrics.addCustomChart(new BStatsMetrics.SimplePie("triggervotifierevent", new Callable<String>() {

				@Override
				public String call() throws Exception {
					return "" + plugin.getBungeeSettings().isTriggerVotifierEvent();
				}
			}));

			metrics.addCustomChart(new BStatsMetrics.SimplePie("globaldata_enabled", new Callable<String>() {

				@Override
				public String call() throws Exception {
					return "" + plugin.getBungeeSettings().isGloblalDataEnabled();
				}
			}));
			if (plugin.getBungeeSettings().isGloblalDataEnabled()) {
				metrics.addCustomChart(new BStatsMetrics.SimplePie("globaldata_usemainmysql", new Callable<String>() {

					@Override
					public String call() throws Exception {
						return "" + plugin.getBungeeSettings().isGloblalDataUseMainMySQL();
					}
				}));
			}
		}

		metrics.addCustomChart(new BStatsMetrics.SimplePie("persitecooldownevents", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isPerSiteCoolDownEvents();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("bedrockplayerprefix", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getOptions().getBedrockPlayerPrefix();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("using_dev_build", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getProfile().contains("dev");
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("votepointtransfering", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getConfigFile().isAllowVotePointTransfers();
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("votecooldown_check_enabled", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + (!plugin.getConfigFile().isDisableCoolDownCheck() && plugin.getRewardHandler()
						.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward"));
			}
		}));

		metrics.addCustomChart(new BStatsMetrics.SimplePie("background_task_time_taken", new Callable<String>() {

			@Override
			public String call() throws Exception {
				return "" + plugin.getLastBackgroundTaskTimeTaken();
			}
		}));

		if (!plugin.getBuildNumber().equals("NOTSET")) {
			metrics.addCustomChart(new BStatsMetrics.SimplePie("dev_build_number", new Callable<String>() {

				@Override
				public String call() throws Exception {
					return "" + plugin.getBuildNumber();
				}
			}));
		}

		/*
		 * ---------------------------------------------------------------------
		 * Broadcast metrics:
		 * - Always log broadcast_type if VoteBroadcast.Type exists (even if NONE)
		 * - If Type == NONE, ignore the rest of the broadcast settings
		 * ---------------------------------------------------------------------
		 */
		final ConfigurationSection root = safeConfig(plugin);
		final ConfigurationSection voteBroadcast = getSection(root, "VoteBroadcast");
		if (voteBroadcast != null && voteBroadcast.isString("Type")) {

			metrics.addCustomChart(new BStatsMetrics.SimplePie("broadcast_type", new Callable<String>() {
				@Override
				public String call() throws Exception {
					return voteBroadcast.getString("Type");
				}
			}));

			final String type = voteBroadcast.getString("Type", null);
			if (type != null && !"NONE".equalsIgnoreCase(type)) {

				if (voteBroadcast.isString("Duration")) {
					metrics.addCustomChart(new BStatsMetrics.SimplePie("broadcast_duration", new Callable<String>() {
						@Override
						public String call() throws Exception {
							return voteBroadcast.getString("Duration");
						}
					}));
				}

				if (voteBroadcast.isInt("MaxSitesListed")) {
					metrics.addCustomChart(new BStatsMetrics.SimplePie("broadcast_max_sites_listed",
							new Callable<String>() {
								@Override
								public String call() throws Exception {
									return bucketMaxSites(voteBroadcast.getInt("MaxSitesListed"));
								}
							}));
				}
			}
		}

		/*
		 * ---------------------------------------------------------------------
		 * VoteLogging metrics:
		 * - Only log when VoteLogging.Enabled is true
		 * ---------------------------------------------------------------------
		 */
		final ConfigurationSection voteLogging = getSection(root, "VoteLogging");
		if (voteLogging != null && voteLogging.getBoolean("Enabled", false)) {

			if (voteLogging.isInt("PurgeDays")) {
				metrics.addCustomChart(new BStatsMetrics.SimplePie("votelogging_purgedays", new Callable<String>() {
					@Override
					public String call() throws Exception {
						return bucketPurgeDays(voteLogging.getInt("PurgeDays"));
					}
				}));
			}

			if (voteLogging.isBoolean("UseMainMySQL")) {
				metrics.addCustomChart(new BStatsMetrics.SimplePie("votelogging_usemainmysql", new Callable<String>() {
					@Override
					public String call() throws Exception {
						return "" + voteLogging.getBoolean("UseMainMySQL");
					}
				}));
			}
		}
	}

	/**
	 * Safely gets the root configuration section.
	 *
	 * @param plugin plugin instance
	 * @return root config section, or null if inaccessible
	 */
	private ConfigurationSection safeConfig(final VotingPluginMain plugin) {
		try {
			return plugin.getConfigFile().getData();
		} catch (Exception ignored) {
			return null;
		}
	}

	/**
	 * Gets a child section safely.
	 *
	 * @param root root section
	 * @param key  child key
	 * @return child section or null
	 */
	private ConfigurationSection getSection(final ConfigurationSection root, final String key) {
		if (root == null) {
			return null;
		}
		try {
			return root.getConfigurationSection(key);
		} catch (Exception ignored) {
			return null;
		}
	}

	/**
	 * Buckets a milestone count to reduce bStats cardinality.
	 *
	 * @param count count
	 * @return bucket string
	 */
	private String bucketCount(final int count) {
		if (count <= 0) {
			return "0";
		}
		if (count == 1) {
			return "1";
		}
		if (count <= 5) {
			return "2-5";
		}
		if (count <= 10) {
			return "6-10";
		}
		if (count <= 25) {
			return "11-25";
		}
		if (count <= 50) {
			return "26-50";
		}
		if (count <= 100) {
			return "51-100";
		}
		return ">100";
	}

	/**
	 * Buckets MaxSitesListed to reduce bStats cardinality.
	 *
	 * @param maxSites max sites listed
	 * @return bucket string
	 */
	private String bucketMaxSites(final int maxSites) {
		if (maxSites <= 0) {
			return "0";
		}
		if (maxSites <= 5) {
			return "1-5";
		}
		if (maxSites <= 10) {
			return "6-10";
		}
		if (maxSites <= 25) {
			return "11-25";
		}
		return ">25";
	}

	/**
	 * Buckets purge days to reduce bStats cardinality.
	 *
	 * @param purgeDays purge days
	 * @return bucket string
	 */
	private String bucketPurgeDays(final int purgeDays) {
		if (purgeDays < 0) {
			return "disabled";
		}
		if (purgeDays == 0) {
			return "0";
		}
		if (purgeDays <= 7) {
			return "1-7";
		}
		if (purgeDays <= 30) {
			return "8-30";
		}
		if (purgeDays <= 90) {
			return "31-90";
		}
		if (purgeDays <= 365) {
			return "91-365";
		}
		return ">365";
	}
}
