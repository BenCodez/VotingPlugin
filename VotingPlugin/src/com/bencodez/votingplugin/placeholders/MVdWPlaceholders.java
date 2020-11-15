package com.bencodez.votingplugin.placeholders;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.usermanager.UserManager;

import be.maximvdw.placeholderapi.PlaceholderAPI;
import be.maximvdw.placeholderapi.PlaceholderReplaceEvent;
import be.maximvdw.placeholderapi.PlaceholderReplacer;

// TODO: Auto-generated Javadoc
/**
 * The Class PlaceHolders.
 */
public class MVdWPlaceholders {

	/** The instance. */
	static MVdWPlaceholders instance = new MVdWPlaceholders();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Gets the single instance of PlaceHolders.
	 *
	 * @return single instance of PlaceHolders
	 */
	public static MVdWPlaceholders getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new place holders.
	 */
	private MVdWPlaceholders() {
	}

	/**
	 * Instantiates a new place holders.
	 *
	 * @param plugin the plugin
	 */
	public MVdWPlaceholders(VotingPluginMain plugin) {
		MVdWPlaceholders.plugin = plugin;
	}

	public void loadMVdWPlaceholders() {
		if (Bukkit.getPluginManager().isPluginEnabled("MVdWPlaceholderAPI")) {
			// The plugin is enabled
			for (final PlaceHolder<User> place : PlaceHolders.getInstance().getPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(VotingPluginMain.plugin, "VotingPlugin_" + str, new PlaceholderReplacer() {

						@Override
						public String onPlaceholderReplace(PlaceholderReplaceEvent event) {
							User user = UserManager.getInstance().getVotingPluginUser(event.getOfflinePlayer());
							if (Config.getInstance().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
								user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
							}
							return place.placeholderRequest(event.getOfflinePlayer(), user,
									event.getPlaceholder().substring("VotingPlugin_".length()));
						}

					});
				}

			}

			for (final PlaceHolder<User> place : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(VotingPluginMain.plugin, "VotingPlugin_" + str, new PlaceholderReplacer() {

						@Override
						public String onPlaceholderReplace(PlaceholderReplaceEvent event) {
							return place.placeholderRequest(event.getOfflinePlayer(), null,
									event.getPlaceholder().substring("VotingPlugin_".length()));
						}

					});
				}

			}
		}
	}
}