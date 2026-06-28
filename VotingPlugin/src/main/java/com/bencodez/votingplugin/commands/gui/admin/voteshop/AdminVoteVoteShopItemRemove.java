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
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteConfirmation;

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop item remove GUI handler.
 */
public class AdminVoteVoteShopItemRemove extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;
	@Getter
	@Setter
	private String category;

	/**
	 * Constructor for AdminVoteVoteShopItemRemove.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteShopItemRemove(VotingPluginMain plugin, CommandSender player) {
		this(plugin, player, null);
	}

	/**
	 * Constructor for AdminVoteVoteShopItemRemove.
	 *
	 * @param plugin   the VotingPluginMain instance
	 * @param player   the command sender
	 * @param category the category identifier
	 */
	public AdminVoteVoteShopItemRemove(VotingPluginMain plugin, CommandSender player, String category) {
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
		BInventory inv = new BInventory(category == null ? "Edit VoteShop Remove Item" : "Remove Category Item: " + category);
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		for (final String identifier : getIdentifiers()) {
			ConfigurationSection section = getItemSection(identifier);
			if (section == null) {
				continue;
			}
			inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getDisplaySection(section))
					.addLoreLine("&c&lClick to remove")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteConfirmation(plugin, clickEvent.getPlayer(), "Remove shop item " + identifier + "?") {

						@Override
						public void onConfirm(Player p) {
							removeShop(identifier);
							p.sendMessage("Removed " + identifier);
							plugin.reload();
						}

						@Override
						public void onDeny(Player p) {
							if (category == null) {
								new AdminVoteVoteShop(plugin, p).open();
							} else {
								new AdminVoteVoteShopCategory(plugin, p, category).open();
							}
						}
					}.open();

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

	/**
	 * Removes a shop item.
	 *
	 * @param identifier the identifier
	 */
	private void removeShop(String identifier) {
		if (category == null) {
			plugin.getShopFile().removeShop(identifier);
			return;
		}
		plugin.getShopFile().getData().set("Categories." + category + ".Shop." + identifier, null);
		plugin.getShopFile().saveData();
	}

}
