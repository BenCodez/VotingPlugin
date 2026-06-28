package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
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
 * Admin vote shop categories GUI handler.
 */
public class AdminVoteVoteShopCategories extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;

	/**
	 * Constructor for AdminVoteVoteShopCategories.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteShopCategories(VotingPluginMain plugin, CommandSender player) {
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
		BInventory inv = new BInventory("Edit VoteShop Categories");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		ConfigurationSection categories = plugin.getShopFile().getCategoriesSection();
		if (categories != null) {
			for (String category : categories.getKeys(false)) {
				ConfigurationSection categorySection = plugin.getShopFile().getCategorySection(category);
				inv.addButton(new BInventoryButton(getCategoryItem(category, categorySection)) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						new AdminVoteVoteShopCategory(plugin, clickEvent.getPlayer(), (String) getData("category"))
								.open(GUIMethod.CHEST);
					}
				}.addData("category", category));
			}
		}

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd category")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(clickEvent.getPlayer(), "", null, true,
						"Enter category name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								createCategory(value, null);
								player.sendMessage(MessageAPI.colorize("&aCategory created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.HOPPER).setName("&aAdd category from hand")
				.addLoreLine("&7Drag an item onto this button")
				.addLoreLine("&7or click while holding an item.")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack itemStack = getHeldItem(clickEvent);
				if (isEmpty(itemStack)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShopCategories(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
					return;
				}
				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(clickEvent.getPlayer(), "", null, true,
						"Enter category name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								createCategory(value, itemStack);
								player.sendMessage(MessageAPI.colorize("&aCategory created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove category")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopCategoryRemove(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.BARRIER).setName("&cBack")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShop(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Creates a category and matching main shop category button.
	 *
	 * @param category  the category identifier
	 * @param itemStack optional display item
	 */
	private void createCategory(String category, ItemStack itemStack) {
		String categoryPath = "Categories." + category;
		String buttonPath = "Shop." + category;
		plugin.getShopFile().getData().set(categoryPath + ".Name", "&b" + category);
		plugin.getShopFile().getData().set(categoryPath + ".BackButton", true);
		plugin.getShopFile().getData().set(categoryPath + ".BackButtonItem.Material", "BARRIER");
		plugin.getShopFile().getData().set(categoryPath + ".BackButtonItem.Amount", 1);
		plugin.getShopFile().getData().set(categoryPath + ".BackButtonItem.Slot", -2);
		plugin.getShopFile().getData().set(categoryPath + ".BackButtonItem.Name", "&cBack");

		plugin.getShopFile().getData().set(buttonPath + ".Category", category);
		plugin.getShopFile().getData().set(buttonPath + ".Permission", "");
		plugin.getShopFile().getData().set(buttonPath + ".HideOnNoPermission", true);

		if (isEmpty(itemStack)) {
			plugin.getShopFile().getData().set(buttonPath + ".DisplayItem.Material", "CHEST");
			plugin.getShopFile().getData().set(buttonPath + ".DisplayItem.Amount", 1);
			plugin.getShopFile().getData().set(buttonPath + ".DisplayItem.Name", "&b" + category);
		} else {
			ItemBuilder item = new ItemBuilder(itemStack.clone());
			Map<String, Object> map = item.getConfiguration(true);
			for (Entry<String, Object> entry : map.entrySet()) {
				plugin.getShopFile().getData().set(buttonPath + ".DisplayItem.ItemStack." + entry.getKey(), entry.getValue());
			}
		}

		if (!plugin.getShopFile().getData().isConfigurationSection(categoryPath + ".Shop")) {
			plugin.getShopFile().getData().createSection(categoryPath + ".Shop");
		}
		plugin.getShopFile().saveData();
	}

	/**
	 * Gets a display item for a category.
	 *
	 * @param category the category identifier
	 * @param section  the category section
	 * @return the display item
	 */
	private ItemBuilder getCategoryItem(String category, ConfigurationSection section) {
		ConfigurationSection buttonSection = plugin.getShopFile().getData().getConfigurationSection("Shop." + category);
		if (buttonSection != null) {
			ConfigurationSection displaySection = plugin.getShopFile().getDisplaySection(buttonSection);
			if (displaySection != null) {
				return new ItemBuilder(displaySection).addLoreLine("&7Click to edit category");
			}
		}
		String name = section == null ? "&b" + category : section.getString("Name", "&b" + category);
		return new ItemBuilder(Material.CHEST).setName(name).addLoreLine("&7Click to edit category");
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
