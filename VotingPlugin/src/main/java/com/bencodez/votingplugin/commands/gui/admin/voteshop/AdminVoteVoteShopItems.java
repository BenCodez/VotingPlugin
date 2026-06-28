package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Material;
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

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop items GUI handler.
 */
public class AdminVoteVoteShopItems extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;
	@Getter
	@Setter
	private String category;

	/**
	 * Constructor for AdminVoteVoteShopItems.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteShopItems(VotingPluginMain plugin, CommandSender player) {
		this(plugin, player, null);
	}

	/**
	 * Constructor for AdminVoteVoteShopItems.
	 *
	 * @param plugin   the VotingPluginMain instance
	 * @param player   the command sender
	 * @param category the category identifier
	 */
	public AdminVoteVoteShopItems(VotingPluginMain plugin, CommandSender player, String category) {
		super(plugin, player);
		this.plugin = plugin;
		this.category = category;
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
		BInventory inv = new BInventory(category == null ? "Edit VoteShop Items" : "Edit Category Items: " + category);
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		for (String identifier : getIdentifiers()) {
			ConfigurationSection section = getItemSection(identifier);
			if (section == null) {
				continue;
			}
			inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getDisplaySection(section))) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), (String) getData("ident"), category)
							.open(GUIMethod.CHEST);

				}
			}.addData("ident", identifier));
		}

		if (category != null) {
			inv.addButton(new BInventoryButton(new ItemBuilder(Material.BARRIER).setName("&cBack to category")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteVoteShopCategory(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
				}
			});
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Gets the identifiers to display.
	 *
	 * @return the identifiers
	 */
	private Set<String> getIdentifiers() {
		if (category != null) {
			ConfigurationSection section = plugin.getShopFile().getCategoryShopSection(category);
			if (section != null) {
				return section.getKeys(false);
			}
			return Set.of();
		}
		return plugin.getShopFile().getShopIdentifiers();
	}

	/**
	 * Gets the item section.
	 *
	 * @param identifier the item identifier
	 * @return the item section
	 */
	private ConfigurationSection getItemSection(String identifier) {
		if (category != null) {
			return plugin.getShopFile().getData().getConfigurationSection("Categories." + category + ".Shop." + identifier);
		}
		return plugin.getShopFile().getShopIdentifierSection(identifier);
	}

}
