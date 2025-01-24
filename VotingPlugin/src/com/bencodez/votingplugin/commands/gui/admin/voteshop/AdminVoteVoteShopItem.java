package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

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
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteVoteShopItem extends GUIHandler {

	private String identifier;
	private VotingPluginMain plugin;

	public AdminVoteVoteShopItem(VotingPluginMain plugin, CommandSender player, String identifier) {
		super(plugin, player);
		this.plugin = plugin;
		this.identifier = identifier;
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
		// to add
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1), new EditGUIValueString("Identifier_Name",
				plugin.getShopFile().getShopIdentifierIdentifierName(identifier)) {

			@Override
			public void setValue(Player player, String name) {
				setPathData(getKey(), name);
			}
		}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.OAK_DOOR, 1),
				new EditGUIValueString("Permission", plugin.getShopFile().getVoteShopPermission(identifier)) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.EMERALD, 1),
				new EditGUIValueNumber("Cost", plugin.getShopFile().getShopIdentifierCost(identifier)) {

					@Override
					public void setValue(Player player, Number num) {
						setPathData(getKey(), num.intValue());
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BARRIER, 1),
				new EditGUIValueNumber("Limit", plugin.getShopFile().getShopIdentifierLimit(identifier)) {

					@Override
					public void setValue(Player player, Number num) {
						setPathData(getKey(), num.intValue());
					}
				}));

		// display item
		ConfigurationSection displayItemData = plugin.getShopFile().getShopIdentifierSection(identifier);
		inv.addButton(new EditGUIButton(new ItemBuilder(displayItemData.getString("Material")).setAmount(1),
				new EditGUIValueString("Material", displayItemData.getString("Material")) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.PAPER, 1),
				new EditGUIValueString("Name", displayItemData.getString("Name")) {

					@Override
					public void setValue(Player player, String name) {
						setPathData(getKey(), name);
					}
				}));
		inv.addButton(new EditGUIButton(new ItemBuilder(Material.BOOK, 1),
				new EditGUIValueList("Lore", displayItemData.getStringList("Lore")) {

					@Override
					public void setValue(Player player, ArrayList<String> value) {
						setPathData(getKey(), value);
					}
				}));
		inv.addButton(new BInventoryButton(
				new ItemBuilder(player.getInventory().getItemInMainHand().clone()).addLoreLine("&cClick to add")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemBuilder item = new ItemBuilder(clickEvent.getPlayer().getInventory().getItemInMainHand().clone());
				Map<String, Object> map = item.getConfiguration(false);
				for (Entry<String, Object> entry : map.entrySet()) {
					setPathData(entry.getKey(), entry.getValue());
				}
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(player.getInventory().getItemInMainHand().clone())
				.addLoreLine("&cClick to add with exact data")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				ItemBuilder item = new ItemBuilder(clickEvent.getPlayer().getInventory().getItemInMainHand().clone());
				Map<String, Object> map = item.getConfiguration(true);
				for (Entry<String, Object> entry : map.entrySet()) {
					setPathData("ItemStack." + entry.getKey(), entry.getValue());
				}
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DISPENSER, 1).setName("&cRewards")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				RewardEditGUI.getInstance().openRewardGUI(clickEvent.getPlayer(),
						plugin.getRewardHandler().getDirectlyDefined("Shop." + identifier + ".Rewards"));
			}
		});

		// implement item reward?

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

	private void setPathData(String path, Object value) {
		plugin.getShopFile().getData().set("Shop." + identifier + "." + path, value);
		plugin.getShopFile().saveData();
		plugin.reload();
	}

}
