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
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueList;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueString;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.command.gui.RewardEditGUI;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

/**
 * Admin vote shop item GUI handler.
 */
public class AdminVoteVoteShopItem extends GUIHandler {

	@Getter
	@Setter
	private String identifier;
	@Getter
	@Setter
	private VotingPluginMain plugin;
	@Getter
	@Setter
	private String category;

	/**
	 * Constructor for AdminVoteVoteShopItem.
	 *
	 * @param plugin     the VotingPluginMain instance
	 * @param player     the command sender
	 * @param identifier the item identifier
	 */
	public AdminVoteVoteShopItem(VotingPluginMain plugin, CommandSender player, String identifier) {
		this(plugin, player, identifier, null);
	}

	/**
	 * Constructor for AdminVoteVoteShopItem.
	 *
	 * @param plugin     the VotingPluginMain instance
	 * @param player     the command sender
	 * @param identifier the item identifier
	 * @param category   the category identifier
	 */
	public AdminVoteVoteShopItem(VotingPluginMain plugin, CommandSender player, String identifier, String category) {
		super(plugin, player);
		this.plugin = plugin;
		this.identifier = identifier;
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
		EditGUI inv = new EditGUI("Edit VoteShop Item: " + identifier);
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		ConfigurationSection shopItemData = getShopItemSection();
		ConfigurationSection displayItemData = plugin.getShopFile().getDisplaySection(shopItemData);
		if (displayItemData == null) {
			displayItemData = shopItemData;
		}

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueString("Identifier_Name",
				shopItemData.getString("Identifier_Name", identifier)) {

			@Override
			public void setValue(Player player, String name) {
				setPathData(getKey(), name);
			}
		}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.OAK_DOOR, 1),
				new EditGUIValueString("Permission", shopItemData.getString("Permission", "")) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.EMERALD, 1),
				new EditGUIValueNumber("Cost", shopItemData.getInt("Cost")) {

					@Override
					public void setValue(Player player, Number num) {
						setPathData(getKey(), num.intValue());
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BARRIER, 1),
				new EditGUIValueNumber("Limit", shopItemData.getInt("Limit", -1)) {

					@Override
					public void setValue(Player player, Number num) {
						setPathData(getKey(), num.intValue());
					}
				}));

		inv.addButton(new EditGUIButton(new ItemBuilder(displayItemData.getString("Material", "STONE")).setAmount(1),
				new EditGUIValueString(getDisplayPath("Material"), displayItemData.getString("Material", "STONE")) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString(getDisplayPath("Name"), displayItemData.getString("Name", "")) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BOOK, 1),
				new EditGUIValueList(getDisplayPath("Lore"), displayItemData.getStringList("Lore")) {

					@Override
					public void setValue(Player player, ArrayList<String> value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new BInventoryButton(new ItemBuilder(getPreviewItem(player)).addLoreLine("&cClick to copy held item display data")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack itemStack = getHeldItem(clickEvent);
				if (isEmpty(itemStack)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), identifier, category).open(GUIMethod.CHEST);
					return;
				}
				setDisplayItemData(itemStack);
				clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&aUpdated display item data for " + identifier));
				new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), identifier, category).open(GUIMethod.CHEST);
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(getPreviewItem(player)).addLoreLine("&cClick to copy held exact item data")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemStack itemStack = getHeldItem(clickEvent);
				if (isEmpty(itemStack)) {
					clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&cHold an item in your main hand first."));
					new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), identifier, category).open(GUIMethod.CHEST);
					return;
				}
				setItemData(itemStack, true);
				clickEvent.getPlayer().sendMessage(MessageAPI.colorize("&aUpdated exact ItemStack data for " + identifier));
				new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), identifier, category).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DISPENSER, 1).setName("&cRewards")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined(getBasePath() + ".Rewards"));
			}
		});

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	/**
	 * Sets DisplayItem data using exact ItemStack serialization.
	 *
	 * @param itemStack the item stack to copy
	 */
	private void setDisplayItemData(ItemStack itemStack) {
		ItemBuilder item = new ItemBuilder(itemStack.clone());
		Map<String, Object> map = item.getConfiguration(true);
		plugin.getShopFile().getData().set(getBasePath() + ".DisplayItem", null);
		for (Entry<String, Object> entry : map.entrySet()) {
			setPathData("DisplayItem.ItemStack." + entry.getKey(), entry.getValue());
		}
	}

	/**
	 * Sets item display or exact ItemStack data.
	 *
	 * @param itemStack the item stack to copy
	 * @param exactData true to copy exact ItemStack data
	 */
	private void setItemData(ItemStack itemStack, boolean exactData) {
		ItemBuilder item = new ItemBuilder(itemStack.clone());
		Map<String, Object> map = item.getConfiguration(exactData);
		for (Entry<String, Object> entry : map.entrySet()) {
			setPathData(exactData ? "ItemStack." + entry.getKey() : getDisplayPath(entry.getKey()), entry.getValue());
		}
	}

	/**
	 * Sets data for the current shop item.
	 *
	 * @param path  the relative path
	 * @param value the value
	 */
	private void setPathData(String path, Object value) {
		plugin.getShopFile().getData().set(getBasePath() + "." + path, value);
		plugin.getShopFile().saveData();
		plugin.reload();
	}

	/**
	 * Gets the base config path for this item.
	 *
	 * @return the config path
	 */
	private String getBasePath() {
		if (category != null && !category.isEmpty()) {
			return "Categories." + category + ".Shop." + identifier;
		}
		return "Shop." + identifier;
	}

	/**
	 * Gets the current shop item section.
	 *
	 * @return the shop item section
	 */
	private ConfigurationSection getShopItemSection() {
		ConfigurationSection section = plugin.getShopFile().getData().getConfigurationSection(getBasePath());
		if (section == null) {
			section = plugin.getShopFile().getData().createSection(getBasePath());
		}
		return section;
	}

	/**
	 * Gets the display path for this item.
	 *
	 * @param path the display child path
	 * @return the relative display path
	 */
	private String getDisplayPath(String path) {
		if (getShopItemSection().isConfigurationSection("DisplayItem")) {
			return "DisplayItem." + path;
		}
		return path;
	}

	/**
	 * Gets a preview item for the current player.
	 *
	 * @param player the player
	 * @return the preview item
	 */
	private ItemStack getPreviewItem(Player player) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		if (isEmpty(itemStack)) {
			return new ItemStack(Material.HOPPER);
		}
		return itemStack.clone();
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
