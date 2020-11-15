package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteShopConfirm extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;
	private String identifier;

	public VoteShopConfirm(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, String identifier) {
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
