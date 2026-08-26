package com.bencodez.votingplugin.voteshop;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined vote-shop reward paths. */
public final class VoteShopRewardRegistrar {

	private VoteShopRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		for (String identifier : plugin.getShopFile().getShopIdentifiers()) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("Shop." + identifier + ".Rewards",
					plugin.getShopFile()));
		}

		ConfigurationSection categories = plugin.getShopFile().getCategoriesSection();
		if (categories == null) {
			return;
		}
		for (String category : categories.getKeys(false)) {
			ConfigurationSection shop = plugin.getShopFile().getCategoryShopSection(category);
			if (shop == null) {
				continue;
			}
			for (String item : shop.getKeys(false)) {
				plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
						"Categories." + category + "." + item + ".Rewards", plugin.getShopFile()));
			}
		}
	}
}
