package com.bencodez.votingplugin.voteshop;

import org.bukkit.entity.Player;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopEntry;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;
import com.bencodez.votingplugin.voteshop.shop.VoteShopLoader;

import lombok.Getter;
import lombok.Setter;

/**
 * Central vote shop manager.
 */
@Getter
@Setter
public class VoteShopManager {

	private VoteShopDefinition definition;

	private VotingPluginMain plugin;

	private VoteShopPurchaseService purchaseService;

	/**
	 * Creates the manager.
	 *
	 * @param plugin the plugin
	 */
	public VoteShopManager(VotingPluginMain plugin) {
		this.plugin = plugin;
		reload();
	}

	/**
	 * Reloads the shop definition.
	 */
	public void reload() {
		VoteShopLoader loader = new VoteShopLoader();
		definition = loader.load(plugin.getShopFile());
		purchaseService = new VoteShopPurchaseService(plugin, definition);
	}

	/**
	 * Gets a main entry.
	 *
	 * @param identifier the identifier
	 * @return the entry or null
	 */
	public VoteShopEntry getMainEntry(String identifier) {
		if (identifier == null) {
			return null;
		}
		return definition.getMainEntries().get(identifier);
	}

	/**
	 * Gets an entry from a category.
	 *
	 * @param categoryId the category id
	 * @param identifier the identifier
	 * @return the entry or null
	 */
	public VoteShopEntry getCategoryEntry(String categoryId, String identifier) {
		VoteShopCategory category = definition.getCategory(categoryId);
		if (category == null || identifier == null) {
			return null;
		}
		return category.getEntries().get(identifier);
	}

	/**
	 * Gets a category.
	 *
	 * @param categoryId the category id
	 * @return the category or null
	 */
	public VoteShopCategory getCategory(String categoryId) {
		return definition.getCategory(categoryId);
	}

	/**
	 * Purchases an item.
	 *
	 * @param player the player
	 * @param user the user
	 * @param item the item
	 * @return the result
	 */
	public VoteShopPurchaseResult purchase(Player player, VotingPluginUser user, VoteShopItem item) {
		return purchaseService.purchase(player, user, item);
	}
}
