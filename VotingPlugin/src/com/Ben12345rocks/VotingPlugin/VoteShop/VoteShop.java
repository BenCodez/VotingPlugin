package com.Ben12345rocks.VotingPlugin.VoteShop;

import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Commands.GUI.PlayerGUIs;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class VoteShop {
	static VoteShop instance = new VoteShop();

	public static VoteShop getInstance() {
		return instance;
	}

	private VoteShop() {
	}

	public void voteShop(Player player) {
		if (!Config.getInstance().getVoteShopEnabled()) {
			player.sendMessage(StringUtils.getInstance().colorize("&cVote shop disabled"));
			return;
		}
		BInventory inv = new BInventory(Config.getInstance().getVoteShopName());

		for (String identifier : Config.getInstance().getIdentifiers()) {

			String perm = Config.getInstance().getVoteShopPermission(identifier);
			boolean hasPerm = false;
			if (perm.isEmpty()) {
				hasPerm = true;
			} else {
				hasPerm = player.hasPermission(perm);
			}

			if (hasPerm) {
				ItemBuilder builder = new ItemBuilder(Config.getInstance().getIdentifierSection(identifier));

				inv.addButton(Config.getInstance().getIdentifierSlot(identifier), new BInventoryButton(builder) {

					@Override
					public void onClick(ClickEvent event) {
						Player player = event.getWhoClicked();

						User user = UserManager.getInstance().getVotingPluginUser(player);
						int points = Config.getInstance().getIdentifierCost(identifier);
						String identifier = Config.getInstance().getIdentifierFromSlot(event.getSlot());
						if (identifier != null) {
							if (user.removePoints(points)) {
								RewardHandler.getInstance().giveReward(user, Config.getInstance().getData(),
										Config.getInstance().getIdentifierRewardsPath(identifier));
								user.sendMessage(Config.getInstance().getFormatShopPurchaseMsg()
										.replace("%Identifier%", identifier).replace("%Points%", "" + points));
							} else {
								user.sendMessage(Config.getInstance().getFormatShopFailedMsg()
										.replace("%Identifier%", identifier).replace("%Points%", "" + points));
							}
						}
					}

				});
			}
		}

		if (Config.getInstance().getVoteShopBackButton()) {
			inv.addButton(new BInventoryButton(PlayerGUIs.getInstance().getBackButton()) {

				@Override
				public void onClick(ClickEvent event) {
					PlayerGUIs.getInstance().openVoteGUI(event.getPlayer(),
							UserManager.getInstance().getVotingPluginUser(player));
				}
			});

		}

		inv.openInventory(player);
	}
}
