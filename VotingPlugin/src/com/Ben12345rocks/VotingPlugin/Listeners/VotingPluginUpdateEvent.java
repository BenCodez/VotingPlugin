package com.Ben12345rocks.VotingPlugin.Listeners;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.PluginUpdateVersionEvent;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.BungeeSettings;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.SpecialRewardsConfig;

// TODO: Auto-generated Javadoc
/**
 * The Class AdvancedCoreUpdateEvent.
 */
public class VotingPluginUpdateEvent implements Listener {

	/** The plugin. */
	private static Main plugin;

	/**
	 * Instantiates a new advanced core update event.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public VotingPluginUpdateEvent(Main plugin) {
		VotingPluginUpdateEvent.plugin = plugin;
	}

	/**
	 * On plugin update.
	 *
	 * @param event
	 *            the event
	 */
	@SuppressWarnings("deprecation")
	@EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
	public void onPluginUpdate(PluginUpdateVersionEvent event) {
		if (event.getPlugin().getName().equals(plugin.getDescription().getName())) {
			if (!event.getOldVersion().equals("")) {
				plugin.getLogger().info("VotingPlugin Updated to " + plugin.getDescription().getVersion() + " from "
						+ event.getOldVersion());

				if (SpecialRewardsConfig.getInstance().isJustCreated()) {
					// copy old data over
					plugin.getLogger().info("Coping data from Config.yml into SpecialRewards.yml");

					String[] copies = new String[] { "VoteCoolDownEndedReward", "AnySiteRewards", "VoteStreak",
							"MileStones", "VoteParty", "Cumulative", "AllSites", "FirstVote", "DailyAwards",
							"WeeklyAwards", "MonthlyAwards" };
					for (String str : copies) {
						SpecialRewardsConfig.getInstance().getData().set(str,
								Config.getInstance().getData().getConfigurationSection(str));
					}

					String[] copies1 = new String[] { "ResetMilestonesMonthly", "EnableMonthlyAwards",
							"EnableWeeklyAwards", "EnableDailyRewards" };
					for (String str : copies1) {
						SpecialRewardsConfig.getInstance().getData().set(str,
								Config.getInstance().getData().getBoolean(str));
					}
					SpecialRewardsConfig.getInstance().saveData();
				}

				if (BungeeSettings.getInstance().isJustCreated()) {
					// copy old data over
					plugin.getLogger().info("Coping data from Config.yml into BungeeSettings.yml");

					String[] copies = new String[] { "BungeeServer", "SpigotServer" };
					for (String str : copies) {
						BungeeSettings.getInstance().getData().set(str,
								Config.getInstance().getData().getConfigurationSection(str));
					}

					BungeeSettings.getInstance().getData().set("UseBungeecord",
							Config.getInstance().isUseBungeecoord());

					String[] copies1 = new String[] { "BungeeBroadcastAlways", "BungeeDebug", "BungeeBroadcast" };
					for (String str : copies1) {
						BungeeSettings.getInstance().getData().set(str, Config.getInstance().getData().getBoolean(str));
					}

					String[] copies2 = new String[] { "BungeeMethod", "Server" };
					for (String str : copies2) {
						BungeeSettings.getInstance().getData().set(str, Config.getInstance().getData().getString(str));
					}
					BungeeSettings.getInstance().saveData();
				}
			}
		}
	}

}