package com.Ben12345rocks.VotingPlugin.VoteShop;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Configs.ConfigRewards;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteShop {
	static VoteShop instance = new VoteShop();

	static Main plugin = Main.plugin;

	public static VoteShop getInstance() {
		return instance;
	}

	private VoteShop() {
	}

	public VoteShop(Main plugin) {
		VoteShop.plugin = plugin;
	}

	public void voteShop(Player player) {
		BInventory inv = new BInventory("VoteShop");

		for (String identifier : ConfigGUI.getInstance().getIdentifiers()) {

			ItemStack item = new ItemStack(
					Material.getMaterial(ConfigGUI.getInstance()
							.getIdentifierItemMaterial(identifier)),
					ConfigGUI.getInstance().getIdentifierItemAmount(identifier),
					(short) ConfigGUI.getInstance().getIdentifierItemData(
							identifier));
			item = Utils.getInstance().setDurabilty(
					item,
					ConfigGUI.getInstance().getIdentifierItemDurability(
							identifier));

			String skull = ConfigGUI.getInstance().getIdentifierItemSkull(
					identifier);
			if (skull != null) {

				item = Utils.getInstance().setSkullOwner(item,
						skull.replace("%Player%", player.getName()));
			}

			String itemName = ConfigGUI.getInstance().getIdentifierItemName(
					identifier);
			if (itemName != null) {
				itemName = itemName.replace("%Player%", player.getName());
			}
			ArrayList<String> lore = ConfigGUI.getInstance()
					.getIdentifierItemLore(identifier);
			lore = Utils.getInstance().replaceIgnoreCase(lore, "%Player%",
					player.getName());

			inv.addButton(
					ConfigGUI.getInstance().getIdentifierSlot(identifier),
					new BInventoryButton(itemName, Utils.getInstance()
							.convertArray(lore), item) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = (Player) event.getWhoClicked();
							if (player != null) {
								player.closeInventory();
								User user = new User(player);
								int points = ConfigGUI.getInstance()
										.getIdentifierCost(identifier);
								String identifier = ConfigGUI.getInstance()
										.getIdentifierFromSlot(event.getSlot());
								if (identifier != null) {
									if (user.removePoints(points)) {
										for (String reward : ConfigGUI
												.getInstance()
												.getIdentifierRewards(
														identifier)) {
											if (reward != "") {
												ConfigRewards.getInstance()
														.getReward(reward)
														.giveReward(user, true);
											}
										}
										user.sendMessage(ConfigFormat
												.getInstance()
												.getShopPurchaseMsg()
												.replace("%Identifier%",
														identifier)
												.replace("%Points%",
														"" + points));
									} else {
										user.sendMessage(ConfigFormat
												.getInstance()
												.getShopFailedMsg()
												.replace("%Identifier%",
														identifier)
												.replace("%Points%",
														"" + points));
									}
								}
							}

						}
					});
		}

		BInventory.openInventory(player, inv);
	}
}
