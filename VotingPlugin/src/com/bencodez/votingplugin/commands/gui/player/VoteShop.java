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
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteShop extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteShop(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
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
		if (!plugin.getGui().getChestVoteShopEnabled()) {
			player.sendMessage(StringParser.getInstance().colorize(plugin.getGui().getChestVoteShopDisabled()));
			return;
		}

		if (this.user == null) {
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		}
		VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
		/*
		 * if (!plugin.getConfigFile().isExtraVoteShopCheck()) { user.cacheAsync(); }
		 */
		BInventory inv = new BInventory(plugin.getGui().getChestVoteShopName());
		inv.addPlaceholder("points", "" + user.getPoints());
		inv.addPlaceholder("sitesavailable", "" + user.getSitesNotVotedOn());
		inv.dontClose();

		for (final String identifier : plugin.getGui().getChestShopIdentifiers()) {

			String perm = plugin.getGui().getChestVoteShopPermission(identifier);
			boolean hasPerm = false;
			if (perm.isEmpty()) {
				hasPerm = true;
			} else {
				if (perm.startsWith("!")) {
					hasPerm = !player.hasPermission(perm.substring(1));
				} else {
					hasPerm = player.hasPermission(perm);
				}
			}

			int limit = plugin.getGui().getChestShopIdentifierLimit(identifier);

			boolean limitPass = true;
			if (limit > 0) {
				if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
					limitPass = false;
				}
			}

			if (!plugin.getGui().getChestVoteShopNotBuyable(identifier)) {
				if (hasPerm && (limitPass || !plugin.getGui().isChestVoteShopHideLimitedReached())) {
					ItemBuilder builder = new ItemBuilder(plugin.getGui().getChestShopIdentifierSection(identifier));

					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();

							VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
							if (plugin.getConfigFile().isExtraVoteShopCheck()) {
								user.cache();
							}

							String identifier = (String) getData("identifier");
							int limit = (int) getData("Limit");
							int points = plugin.getGui().getChestShopIdentifierCost(identifier);
							if (identifier != null) {
								// if (plugin.getGui().getChestVoteShopCloseGUI(identifier)) {
								// event.closeInventory();
								// }

								// limit fail-safe, should never be needed, except in rare cases
								boolean limitPass = true;
								if (limit > 0) {
									if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
										limitPass = false;
									}
								}

								if (limitPass) {
									if (!plugin.getGui().isChestVoteShopRequireConfirmation(identifier)) {
										HashMap<String, String> placeholders = new HashMap<String, String>();
										placeholders.put("identifier",
												plugin.getGui().getChestShopIdentifierIdentifierName(identifier));
										placeholders.put("points", "" + points);
										placeholders.put("limit", "" + limit);
										if (user.removePoints(points, true)) {
											plugin.getLogger().info("VoteShop: " + user.getPlayerName() + "/"
													+ user.getUUID() + " bought " + identifier + " for " + points);
											if (plugin.getConfigFile().isTrackShopPurchases()) {
												plugin.getServerData().addVoteShopPurchase(identifier);
											}

											plugin.getRewardHandler().giveReward(user, plugin.getGui().getData(),
													plugin.getGui().getChestShopIdentifierRewardsPath(identifier),
													new RewardOptions().setPlaceholders(placeholders));

											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													plugin.getGui().getCHESTVoteShopPurchase(identifier),
													placeholders));
											if (limit > 0) {
												user.setVoteShopIdentifierLimit(identifier,
														user.getVoteShopIdentifierLimit(identifier) + 1);
											}
										} else {
											user.sendMessage(StringParser.getInstance().replacePlaceHolder(
													plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
										}
									} else {
										new VoteShopConfirm(plugin, player, user, identifier).open(GUIMethod.CHEST);

									}
								} else {
									user.sendMessage(plugin.getGui().getChestVoteShopLimitReached());
								}
								plugin.getCommandLoader().processSlotClick(player, user, identifier);
								if (plugin.getGui().getChestVoteShopReopenGUIOnPurchase()) {
									plugin.getCommandLoader().processSlotClick(player, user, "shop");
								}
							}
						}

					}.addData("identifier", identifier).addData("Limit", limit));
				}
			} else {
				if (hasPerm) {
					ItemBuilder builder = new ItemBuilder(plugin.getGui().getChestShopIdentifierSection(identifier));
					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							user.sendMessage(plugin.getConfigFile().getFormatShopNotPurchasable());
						}

					}.addData("identifier", identifier).addData("Limit", limit));
				}
			}
		}

		for (final String str : plugin.getGui().getChestGUIVoteShopExtraItems()) {
			inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIVoteShopExtraItems(str))) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					plugin.getCommandLoader().processSlotClick(player, user, str);
					new RewardBuilder(plugin.getGui().getData(),
							"CHEST.VoteShopExtraItems." + str + "." + clickEvent.getButton().getLastRewardsPath(player))
							.setGiveOffline(false).send(clickEvent.getPlayer());
				}
			});
		}

		if (plugin.getGui().getChestVoteShopBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
