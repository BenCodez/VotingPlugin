package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import org.bukkit.Bukkit;

import com.Ben12345rocks.AdvancedCore.Util.Placeholder.PlaceHolder;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

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
	static Main plugin = Main.plugin;

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
	 * @param plugin
	 *            the plugin
	 */
	public MVdWPlaceholders(Main plugin) {
		MVdWPlaceholders.plugin = plugin;
	}

	public void loadMVdWPlaceholders() {
		if (Bukkit.getPluginManager().isPluginEnabled("MVdWPlaceholderAPI")) {
			// The plugin is enabled
			for (final PlaceHolder<User> place : PlaceHolders.getInstance().getPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(Main.plugin, "VotingPlugin_" + str, new PlaceholderReplacer() {

						@Override
						public String onPlaceholderReplace(PlaceholderReplaceEvent event) {
							return place.placeholderRequest(event.getOfflinePlayer(),
									UserManager.getInstance().getVotingPluginUser(event.getOfflinePlayer()),
									event.getPlaceholder().substring("VotingPlugin_".length()));
						}

					});
				}

			}

			for (final PlaceHolder<User> place : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
				String str = place.getIdentifier();
				if (!str.endsWith("_")) {
					PlaceholderAPI.registerPlaceholder(Main.plugin, "VotingPlugin_" + str, new PlaceholderReplacer() {

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