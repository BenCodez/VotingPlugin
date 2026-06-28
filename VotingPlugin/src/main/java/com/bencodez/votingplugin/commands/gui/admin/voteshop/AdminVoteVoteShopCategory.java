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
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUI;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueBoolean;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueList;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueString;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.valuerequest.StringListener;
import com.bencodez.simpleapi.valuerequest.ValueRequest;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop category GUI handler.
 */
public class AdminVoteVoteShopCategory extends GUIHandler {

	@Getter
	@Setter
	private VotingPluginMain plugin;
	@Getter
	@Setter
	private String category;

	/**
	 * Constructor for AdminVoteVoteShopCategory.
	 *
	 * @param plugin   the VotingPluginMain instance
	 * @param player   the command sender
	 * @param category the category identifier
	 */
	public AdminVoteVoteShopCategory(VotingPluginMain plugin, CommandSender player, String category) {
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
		EditGUI inv = new EditGUI("Edit Category: " + category);
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		ConfigurationSection categorySection = getCategorySection();
		ConfigurationSection backButton = getBackButtonSection();
		ConfigurationSection buttonDisplay = getCategoryButtonDisplaySection();

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString("Name", categorySection.getString("Name", "&b" + category)) {

					@Override
					public void setValue(Player player, String name) {
						setCategoryData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BARRIER, 1),
				new EditGUIValueBoolean("BackButton", categorySection.getBoolean("BackButton", true)) {

					@Override
					public void setValue(Player player, boolean value) {
						setCategoryData(getKey(), value);
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(backButton.getString("Material", "BARRIER")),
				new EditGUIValueString("BackButtonItem.Material", backButton.getString("Material", "BARRIER")) {

					@Override
					public void setValue(Player player, String name) {
						setCategoryData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString("BackButtonItem.Name", backButton.getString("Name", "&cBack")) {

					@Override
					public void setValue(Player player, String name) {
						setCategoryData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BOOK, 1),
				new EditGUIValueList("BackButtonItem.Lore", backButton.getStringList("Lore")) {

					@Override
					public void setValue(Player player, ArrayList<String> value) {
						setCategoryData(getKey(), value);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.ITEM_FRAME, 1),
				new EditGUIValueNumber("BackButtonItem.Slot", backButton.getInt("Slot", -2)) {

					@Override
					public void setValue(Player player, Number num) {
						setCategoryData(getKey(), num.intValue());
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(buttonDisplay.getString("Material", "CHEST")),
				new EditGUIValueString("CategoryButton.Material", buttonDisplay.getString("Material", "CHEST")) {

					@Override
					public void setValue(Player player, String name) {
						setCategoryButtonDisplayData("Material", name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString("CategoryButton.Name", buttonDisplay.getString("Name", "&b" + category)) {

					@Override
					public void setValue(Player player, String name) {
						setCategoryButtonDisplayData("Name", name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BOOK, 1),
				new EditGUIValueList("CategoryButton.Lore", buttonDisplay.getStringList("Lore")) {

					@Override
					public void setValue(Player player, ArrayList<String> value) {
						setCategoryButtonDisplayData("Lore", value);
					}
				}));

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.HOPPER).setName("&aSet category button from hand")
				.addLoreLine("&7Hold an item in your main hand")
				.addLoreLine("&7then click this button.")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack itemStack = getHeldItem(clickEvent);
				if (isEmpty(itemStack)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShopCategory(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
					return;
				}
				setCategoryButtonItem(itemStack);
				clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&aUpdated category button item for " + category));
				new AdminVoteVoteShopCategory(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DIAMOND).setName("&cEdit category items")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItems(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd category item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(clickEvent.getPlayer(), "", null, true,
						"Enter item name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								createCategoryShopItem(value, null);
								player.sendMessage(MessageAPI.colorize("&aCategory shop item created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.HOPPER).setName("&aAdd category item from hand")
				.addLoreLine("&7Hold an item in your main hand")
				.addLoreLine("&7then click this button.")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack itemStack = getHeldItem(clickEvent);
				if (isEmpty(itemStack)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShopCategory(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
					return;
				}
				new ValueRequest(plugin, plugin.getDialogService(), null).requestString(clickEvent.getPlayer(), "", null, true,
						"Enter item name", new StringListener() {

							@Override
							public void onInput(Player player, String value) {
								createCategoryShopItem(value, itemStack);
								player.sendMessage(MessageAPI.colorize("&aCategory shop item created successfully: " + value));
								plugin.reload();
							}
						});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove category item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItemRemove(plugin, clickEvent.getPlayer(), category).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.BARRIER).setName("&cBack to categories")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopCategories(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Creates a category shop item.
	 *
	 * @param identifier the item identifier
	 * @param itemStack  optional display item
	 */
	private void createCategoryShopItem(String identifier, ItemStack itemStack) {
		String basePath = "Categories." + category + ".Shop." + identifier;
		plugin.getShopFile().getData().set(basePath + ".Identifier_Name", identifier);
		plugin.getShopFile().getData().set(basePath + ".Cost", 1);
		plugin.getShopFile().getData().set(basePath + ".Permission", "");
		plugin.getShopFile().getData().set(basePath + ".HideOnNoPermission", true);
		plugin.getShopFile().getData().set(basePath + ".CloseGUI", true);
		plugin.getShopFile().getData().set(basePath + ".RequireConfirmation", false);
		plugin.getShopFile().getData().set(basePath + ".Rewards.Items.Item1.Material", "STONE");
		plugin.getShopFile().getData().set(basePath + ".Rewards.Items.Item1.Amount", 1);
		if (isEmpty(itemStack)) {
			plugin.getShopFile().getData().set(basePath + ".DisplayItem.Material", "STONE");
			plugin.getShopFile().getData().set(basePath + ".DisplayItem.Amount", 1);
			plugin.getShopFile().getData().set(basePath + ".DisplayItem.Name", "&cPlaceholder item");
		} else {
			ItemBuilder item = new ItemBuilder(itemStack.clone());
			Map<String, Object> map = item.getConfiguration(true);
			for (Entry<String, Object> entry : map.entrySet()) {
				plugin.getShopFile().getData().set(basePath + ".DisplayItem.ItemStack." + entry.getKey(), entry.getValue());
			}
		}
		plugin.getShopFile().saveData();
	}

	/**
	 * Sets category data.
	 *
	 * @param path  the relative path
	 * @param value the value
	 */
	private void setCategoryData(String path, Object value) {
		plugin.getShopFile().getData().set("Categories." + category + "." + path, value);
		plugin.getShopFile().saveData();
		plugin.reload();
	}

	/**
	 * Sets category button display data.
	 *
	 * @param path  the relative display path
	 * @param value the value
	 */
	private void setCategoryButtonDisplayData(String path, Object value) {
		plugin.getShopFile().getData().set("Shop." + category + ".DisplayItem." + path, value);
		plugin.getShopFile().saveData();
		plugin.reload();
	}

	/**
	 * Sets the main shop category button item from an item stack.
	 *
	 * @param itemStack the item stack
	 */
	private void setCategoryButtonItem(ItemStack itemStack) {
		ItemBuilder item = new ItemBuilder(itemStack.clone());
		Map<String, Object> map = item.getConfiguration(true);
		plugin.getShopFile().getData().set("Shop." + category + ".DisplayItem", null);
		for (Entry<String, Object> entry : map.entrySet()) {
			plugin.getShopFile().getData().set("Shop." + category + ".DisplayItem.ItemStack." + entry.getKey(), entry.getValue());
		}
		plugin.getShopFile().saveData();
		plugin.reload();
	}

	/**
	 * Gets the category section.
	 *
	 * @return the category section
	 */
	private ConfigurationSection getCategorySection() {
		ConfigurationSection section = plugin.getShopFile().getCategorySection(category);
		if (section == null) {
			section = plugin.getShopFile().getData().createSection("Categories." + category);
		}
		return section;
	}

	/**
	 * Gets the back button section.
	 *
	 * @return the back button section
	 */
	private ConfigurationSection getBackButtonSection() {
		ConfigurationSection section = plugin.getShopFile().getData()
				.getConfigurationSection("Categories." + category + ".BackButtonItem");
		if (section == null) {
			section = plugin.getShopFile().getData().createSection("Categories." + category + ".BackButtonItem");
		}
		return section;
	}

	/**
	 * Gets the category button display section.
	 *
	 * @return the display section
	 */
	private ConfigurationSection getCategoryButtonDisplaySection() {
		ConfigurationSection section = plugin.getShopFile().getData()
				.getConfigurationSection("Shop." + category + ".DisplayItem");
		if (section == null) {
			section = plugin.getShopFile().getData().createSection("Shop." + category + ".DisplayItem");
		}
		return section;
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
