package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteConfirmation;

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop category remove GUI handler.
 */
public class AdminVoteVoteShopCategoryRemove extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;

	/**
	 * Constructor for AdminVoteVoteShopCategoryRemove.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteShopCategoryRemove(VotingPluginMain plugin, CommandSender player) {
		super(plugin, player);
		this.plugin = plugin;
	}

	@Override
	public void onDialog(Player player) {

	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {

	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory("Remove VoteShop Category");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		ConfigurationSection categories = plugin.getShopFile().getCategoriesSection();
		if (categories != null) {
			for (final String category : categories.getKeys(false)) {
				inv.addButton(new BInventoryButton(getCategoryItem(category).addLoreLine("&c&lClick to remove")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						new AdminVoteConfirmation(plugin, clickEvent.getPlayer(), "Remove category " + category + "?") {

							@Override
							public void onConfirm(Player p) {
								plugin.getShopFile().getData().set("Categories." + category, null);
								plugin.getShopFile().getData().set("Shop." + category, null);
								plugin.getShopFile().saveData();
								p.sendMessage("Removed category " + category);
								plugin.reload();
							}

							@Override
							public void onDeny(Player p) {
								new AdminVoteVoteShopCategories(plugin, p).open();
							}
						}.open();
					}
				});
			}
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Gets a category display item.
	 *
	 * @param category the category identifier
	 * @return the display item
	 */
	private ItemBuilder getCategoryItem(String category) {
		ConfigurationSection buttonSection = plugin.getShopFile().getData().getConfigurationSection("Shop." + category);
		if (buttonSection != null) {
			ConfigurationSection displaySection = plugin.getShopFile().getDisplaySection(buttonSection);
			if (displaySection != null) {
				return new ItemBuilder(displaySection);
			}
		}
		ConfigurationSection categorySection = plugin.getShopFile().getCategorySection(category);
		String name = categorySection == null ? "&b" + category : categorySection.getString("Name", "&b" + category);
		return new ItemBuilder(org.bukkit.Material.CHEST).setName(name);
	}

}
