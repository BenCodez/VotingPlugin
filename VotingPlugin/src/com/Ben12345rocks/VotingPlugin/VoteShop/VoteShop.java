package com.Ben12345rocks.VotingPlugin.VoteShop;

import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

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

			ItemBuilder builder = new ItemBuilder(ConfigGUI.getInstance().getIdentifierSection(identifier));

			inv.addButton(ConfigGUI.getInstance().getIdentifierSlot(identifier),
					new BInventoryButton(builder.toItemStack()) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();

							User user = UserManager.getInstance().getVotingPluginUser(player);
							int points = ConfigGUI.getInstance().getIdentifierCost(identifier);
							String identifier = ConfigGUI.getInstance().getIdentifierFromSlot(event.getSlot());
							if (identifier != null) {
								if (user.removePoints(points)) {
									for (String reward : ConfigGUI.getInstance().getIdentifierRewards(identifier)) {
										RewardHandler.getInstance().giveReward(user, reward, true);
									}
									user.sendMessage(ConfigFormat.getInstance().getShopPurchaseMsg()
											.replace("%Identifier%", identifier).replace("%Points%", "" + points));
								} else {
									user.sendMessage(ConfigFormat.getInstance().getShopFailedMsg()
											.replace("%Identifier%", identifier).replace("%Points%", "" + points));
								}
							}
						}

					});
		}

		BInventory.openInventory(player, inv);
	}
}
