package com.bencodez.votingplugin.placeholders;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.placeholder.NonPlayerPlaceHolder;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

import be.maximvdw.placeholderapi.PlaceholderAPI;
import be.maximvdw.placeholderapi.PlaceholderReplaceEvent;
import be.maximvdw.placeholderapi.PlaceholderReplacer;

// TODO: Auto-generated Javadoc
/**
 * The Class PlaceHolders.
 */
public class MVdWPlaceholders {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new place holders.
	 *
	 * @param plugin the plugin
	 */
	public MVdWPlaceholders(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void loadMVdWPlaceholders() {
		if (Bukkit.getPluginManager().isPluginEnabled("MVdWPlaceholderAPI")) {
			// The plugin is enabled
			for (final PlaceHolder<VotingPluginUser> place : plugin.getPlaceholders().getPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(VotingPluginMain.plugin, "VotingPlugin_" + str,
							new PlaceholderReplacer() {

								@Override
								public String onPlaceholderReplace(PlaceholderReplaceEvent event) {
									VotingPluginUser user = plugin.getVotingPluginUserManager()
											.getVotingPluginUser(event.getOfflinePlayer());
									if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders()
											&& user.hasPrimaryAccount()) {
										user = plugin.getVotingPluginUserManager()
												.getVotingPluginUser(user.getPrimaryAccount());
									}
									return place.placeholderRequest(user,
											event.getPlaceholder().substring("VotingPlugin_".length()));
								}

							});
				}

			}

			for (final NonPlayerPlaceHolder<VotingPluginUser> place : plugin.getPlaceholders()
					.getNonPlayerPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(VotingPluginMain.plugin, "VotingPlugin_" + str,
							new PlaceholderReplacer() {

								@Override
								public String onPlaceholderReplace(PlaceholderReplaceEvent event) {
									return place.placeholderRequest(
											event.getPlaceholder().substring("VotingPlugin_".length()));
								}

							});
				}

			}
		}
	}
}