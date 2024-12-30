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
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.simpleapi.player.PlayerUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteShopConfirm extends GUIHandler {

	private String identifier;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteShopConfirm(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, String identifier) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.identifier = identifier;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
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
		PlayerUtils.setPlayerMeta(plugin, player, "ident", identifier);
		BInventory inv = new BInventory(plugin.getShopFile().getShopConfirmPurchaseTitle());
		inv.dontClose();
		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseYesItem())) {

			@Override
			public void onClick(ClickEvent event) {
				user.cache();
				int points = plugin.getShopFile().getShopIdentifierCost(identifier);
				int limit = plugin.getShopFile().getShopIdentifierLimit(identifier);
				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("identifier", identifier);
				placeholders.put("points", "" + points);
				placeholders.put("limit", "" + limit);
				if (user.removePoints(points, true)) {
					plugin.getLogger().info("VoteShop: " + user.getPlayerName() + "/" + user.getUUID() + " bought "
							+ identifier + " for " + points);
					if (plugin.getConfigFile().isTrackShopPurchases()) {
						plugin.getServerData().addVoteShopPurchase(identifier);
					}

					plugin.getRewardHandler().giveReward(user, plugin.getShopFile().getData(),
							plugin.getShopFile().getShopIdentifierRewardsPath(identifier),
							new RewardOptions().setPlaceholders(placeholders));

					user.sendMessage(PlaceholderUtils
							.replacePlaceHolder(plugin.getShopFile().getVoteShopPurchase(identifier), placeholders));
					if (limit > 0) {
						user.setVoteShopIdentifierLimit(identifier, user.getVoteShopIdentifierLimit(identifier) + 1);
					}
				} else {
					user.sendMessage(PlaceholderUtils
							.replacePlaceHolder(plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
				}
				if (plugin.getShopFile().getVoteShopCloseGUI(identifier)) {
					event.getButton().getInv().closeInv(player, null);
				} else {
					new VoteShop(plugin, event.getPlayer(), user).open();
				}

			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseNoItem())) {

			@Override
			public void onClick(ClickEvent event) {
				if (plugin.getShopFile().getVoteShopCloseGUI(identifier)) {
					event.getButton().getInv().closeInv(player, null);
				} else {
					new VoteShop(plugin, event.getPlayer(), user).open();
				}
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
