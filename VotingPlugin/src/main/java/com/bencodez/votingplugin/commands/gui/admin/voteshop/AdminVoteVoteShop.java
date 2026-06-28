package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.valuerequest.StringListener;
import com.bencodez.simpleapi.valuerequest.ValueRequest;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop GUI handler.
 */
public class AdminVoteVoteShop extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;

	/**
	 * Constructor for AdminVoteVoteShop.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteShop(VotingPluginMain plugin, CommandSender player) {
		super(plugin, player);
		this.plugin = plugin;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onDialog(Player player) {

	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {

	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory("Edit VoteShop");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DIAMOND).setName("&cEdit existing item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItems(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd voteshop item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(player, "", null, true,
						"Enter item name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								plugin.getShopFile().createShop(value);
								player.sendMessage(MessageAPI.colorize("&aShop item created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.HOPPER).setName("&aAdd voteshop item from hand")
				.addLoreLine("&7Hold an item in your main hand")
				.addLoreLine("&7then click this button.")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack item = getHeldItem(clickEvent);
				if (isEmpty(item)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShop(plugin, clickEvent.getPlayer()).open();
					return;
				}

				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(clickEvent.getPlayer(), "", null, true,
						"Enter item name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								createShopFromItem(value, item);
								player.sendMessage(MessageAPI.colorize("&aShop item created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.CHEST).setName("&bEdit categories")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopCategories(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItemRemove(plugin, clickEvent.getPlayer()).open();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Creates a new shop item and copies the held item to display and rewards.
	 *
	 * @param identifier the shop identifier
	 * @param itemStack  the item stack to copy
	 */
	private void createShopFromItem(String identifier, ItemStack itemStack) {
		String basePath = "Shop." + identifier;
		plugin.getShopFile().getData().set(basePath, null);
		plugin.getShopFile().getData().set(basePath + ".Identifier_Name", identifier);
		plugin.getShopFile().getData().set(basePath + ".Cost", 1);
		plugin.getShopFile().getData().set(basePath + ".Permission", "");
		plugin.getShopFile().getData().set(basePath + ".CloseGUI", true);
		plugin.getShopFile().getData().set(basePath + ".RequireConfirmation", false);
		ItemBuilder item = new ItemBuilder(itemStack.clone());
		Map<String, Object> map = item.getConfiguration(true);
		for (Entry<String, Object> entry : map.entrySet()) {
			plugin.getShopFile().getData().set(basePath + ".DisplayItem.ItemStack." + entry.getKey(), entry.getValue());
			plugin.getShopFile().getData().set(basePath + ".Rewards.Items.Item1.ItemStack." + entry.getKey(), entry.getValue());
		}
		plugin.getShopFile().saveData();
	}

	/**
	 * Gets the item the player is holding in their main hand.
	 *
	 * @param clickEvent the click event
	 * @return the held item
	 */
	private ItemStack getHeldItem(ClickEvent clickEvent) {
		return clickEvent.getPlayer().getInventory().getItemInMainHand().clone();
	}

	/**
	 * Checks if an item stack is empty.
	 *
	 * @param itemStack the item stack
	 * @return true if empty
	 */
	private boolean isEmpty(ItemStack itemStack) {
		return itemStack == null || itemStack.getType() == Material.AIR;
	}

}