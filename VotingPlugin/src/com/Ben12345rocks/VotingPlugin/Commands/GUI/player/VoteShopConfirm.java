package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteShopConfirm extends GUIHandler {

	private User user;
	private Main plugin;
	private String identifier;

	public VoteShopConfirm(Main plugin, CommandSender player, User user, String identifier) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.identifier = identifier;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		PlayerUtils.getInstance().setPlayerMeta(player, "ident", identifier);
		BInventory inv = new BInventory(GUI.getInstance().getChestShopConfirmPurchaseTitle());
		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestShopConfirmPurchaseYesItem())) {

			@Override
			public void onClick(ClickEvent event) {
				int points = GUI.getInstance().getChestShopIdentifierCost(identifier);
				int limit = GUI.getInstance().getChestShopIdentifierLimit(identifier);
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("identifier", identifier);
				placeholders.put("points", "" + points);
				placeholders.put("limit", "" + limit);
				if (user.removePoints(points)) {

					RewardHandler.getInstance().giveReward(user, GUI.getInstance().getData(),
							GUI.getInstance().getChestShopIdentifierRewardsPath(identifier),
							new RewardOptions().setPlaceholders(placeholders));

					user.sendMessage(StringParser.getInstance()
							.replacePlaceHolder(Config.getInstance().getFormatShopPurchaseMsg(), placeholders));
					if (limit > 0) {
						user.setVoteShopIdentifierLimit(identifier, user.getVoteShopIdentifierLimit(identifier) + 1);
					}
				} else {
					user.sendMessage(StringParser.getInstance()
							.replacePlaceHolder(Config.getInstance().getFormatShopFailedMsg(), placeholders));
				}
				if (GUI.getInstance().getChestVoteShopCloseGUI(identifier)) {
					event.getButton().getInv().closeInv(player, null);
				} else {
					new VoteShop(plugin, event.getPlayer(), user);
				}

			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestShopConfirmPurchaseNoItem())) {

			@Override
			public void onClick(ClickEvent event) {
				if (GUI.getInstance().getChestVoteShopCloseGUI(identifier)) {
					event.getButton().getInv().closeInv(player, null);
				} else {
					new VoteShop(plugin, event.getPlayer(), user);
				}
			}
		});
		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		return null;
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
