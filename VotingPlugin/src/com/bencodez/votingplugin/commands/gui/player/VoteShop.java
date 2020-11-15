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
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

public class VoteShop extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;

	public VoteShop(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {

	}

	@Override
	public void onChest(Player player) {
		if (!GUI.getInstance().getChestVoteShopEnabled()) {
			player.sendMessage(StringParser.getInstance().colorize(GUI.getInstance().getChestVoteShopDisabled()));
			return;
		}

		BInventory inv = new BInventory(GUI.getInstance().getChestVoteShopName());
		inv.dontClose();

		for (final String identifier : GUI.getInstance().getChestShopIdentifiers()) {

			String perm = GUI.getInstance().getChestVoteShopPermission(identifier);
			boolean hasPerm = false;
			if (perm.isEmpty()) {
				hasPerm = true;
			} else {
				hasPerm = player.hasPermission(perm);
			}

			int limit = GUI.getInstance().getChestShopIdentifierLimit(identifier);

			boolean limitPass = true;
			if (limit > 0) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
				if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
					limitPass = false;
				}
			}

			if (!GUI.getInstance().getChestVoteShopNotBuyable(identifier)) {
				if (hasPerm && (limitPass || !GUI.getInstance().isChestVoteShopHideLimitedReached())) {
					ItemBuilder builder = new ItemBuilder(GUI.getInstance().getChestShopIdentifierSection(identifier));

					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();

							VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
							if (Config.getInstance().isClearCacheOnVoteShopPurchase()
									|| BungeeSettings.getInstance().isUseBungeecoord()) {
								user.clearCache();
							}
							String identifier = (String) getData("identifier");
							int limit = (int) getData("Limit");
							int points = GUI.getInstance().getChestShopIdentifierCost(identifier);
							if (identifier != null) {
								if (GUI.getInstance().getChestVoteShopCloseGUI(identifier)) {
									event.getButton().getInv().closeInv(player, null);
								}

								// limit fail-safe, should never be needed, except in rare cases
								boolean limitPass = true;
								if (limit > 0) {
									if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
										limitPass = false;
									}
								}

								if (limitPass) {
									if (!GUI.getInstance().isChestVoteShopRequireConfirmation()) {
										HashMap<String, String> placeholders = new HashMap<String, String>();
										placeholders.put("identifier",
												GUI.getInstance().getChestShopIdentifierIdentifierName(identifier));
										placeholders.put("points", "" + points);
										placeholders.put("limit", "" + limit);
										if (user.removePoints(points)) {

											RewardHandler.getInstance().giveReward(user, GUI.getInstance().getData(),
													GUI.getInstance().getChestShopIdentifierRewardsPath(identifier),
													new RewardOptions().setPlaceholders(placeholders));

											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													Config.getInstance().getFormatShopPurchaseMsg(), placeholders));
											if (limit > 0) {
												user.setVoteShopIdentifierLimit(identifier,
														user.getVoteShopIdentifierLimit(identifier) + 1);
											}
										} else {
											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													Config.getInstance().getFormatShopFailedMsg(), placeholders));
										}
									} else {
										new VoteShopConfirm(plugin, player, user, identifier).open(GUIMethod.CHEST);
										;
									}
								} else {
									user.sendMessage(GUI.getInstance().getChestVoteShopLimitReached());
								}
							}
						}

					}.addData("identifier", identifier).addData("Limit", limit));
				}
			} else {
				if (hasPerm) {
					ItemBuilder builder = new ItemBuilder(GUI.getInstance().getChestShopIdentifierSection(identifier));
					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {

						}

					}.dontClose().addData("identifier", identifier).addData("Limit", limit));
				}
			}
		}

		if (GUI.getInstance().getChestVoteShopBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}

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
