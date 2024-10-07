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
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
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
		if (!plugin.getShopFile().isVoteShopEnabled()) {
			player.sendMessage(MessageAPI.colorize(plugin.getShopFile().getVoteShopDisabled()));
			return;
		}

		if (this.user == null) {
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		}
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		/*
		 * if (!plugin.getConfigFile().isExtraVoteShopCheck()) { user.cacheAsync(); }
		 */
		BInventory inv = new BInventory(plugin.getShopFile().getVoteShopName());
		inv.addPlaceholder("points", "" + user.getPoints());
		inv.addPlaceholder("sitesavailable", "" + user.getSitesNotVotedOn());
		inv.dontClose();

		for (final String identifier : plugin.getShopFile().getShopIdentifiers()) {

			String perm = plugin.getShopFile().getVoteShopPermission(identifier);
			boolean hasPerm = false;
			if (perm.isEmpty()) {
				hasPerm = true;
			} else {
				String p = "";
				if (perm.startsWith("!")) {
					p = PlaceholderUtils.replacePlaceHolders(player, perm.substring(1));
					hasPerm = !player.hasPermission(p);
				} else {
					p = PlaceholderUtils.replacePlaceHolders(player, perm);
					hasPerm = player.hasPermission(p);
				}
				if (!hasPerm) {
					plugin.extraDebug("VoteShop: " + player.getName() + "/" + player.getUniqueId()
							+ " does not have permission `" + p + "` for " + identifier);
				}
			}

			int limit = plugin.getShopFile().getShopIdentifierLimit(identifier);

			boolean limitPass = true;
			if (limit > 0) {
				if (user.getVoteShopIdentifierLimit(identifier) >= limit) {
					limitPass = false;
				}
			}

			if (!plugin.getShopFile().getVoteShopNotBuyable(identifier)) {
				if ((hasPerm || !plugin.getShopFile().getVoteShopHideOnNoPermission(identifier))
						&& (limitPass || !plugin.getShopFile().isVoteShopHideLimitedReached())) {
					ItemBuilder builder = new ItemBuilder(plugin.getShopFile().getShopIdentifierSection(identifier));

					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = event.getWhoClicked();

							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
							if (plugin.getConfigFile().isExtraVoteShopCheck()) {
								user.cache();
							}

							String identifier = (String) getData("identifier");
							int limit = (int) getData("Limit");
							int points = plugin.getShopFile().getShopIdentifierCost(identifier);
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
									if (!plugin.getShopFile().isVoteShopRequireConfirmation(identifier)) {
										HashMap<String, String> placeholders = new HashMap<String, String>();
										placeholders.put("identifier",
												plugin.getShopFile().getShopIdentifierIdentifierName(identifier));
										placeholders.put("points", "" + points);
										placeholders.put("limit", "" + limit);
										if (user.removePoints(points, true)) {
											plugin.getLogger().info("VoteShop: " + user.getPlayerName() + "/"
													+ user.getUUID() + " bought " + identifier + " for " + points);
											if (plugin.getConfigFile().isTrackShopPurchases()) {
												plugin.getServerData().addVoteShopPurchase(identifier);
											}

											plugin.getRewardHandler().giveReward(user, plugin.getShopFile().getData(),
													plugin.getShopFile().getShopIdentifierRewardsPath(identifier),
													new RewardOptions().setPlaceholders(placeholders));

											user.sendMessage(PlaceholderUtils.replacePlaceHolder(
													plugin.getShopFile().getVoteShopPurchase(identifier),
													placeholders));
											if (limit > 0) {
												user.setVoteShopIdentifierLimit(identifier,
														user.getVoteShopIdentifierLimit(identifier) + 1);
											}
										} else {
											user.sendMessage(PlaceholderUtils.replacePlaceHolder(
													plugin.getConfigFile().getFormatShopFailedMsg(), placeholders));
										}
									} else {
										plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

											@Override
											public void run() {
												new VoteShopConfirm(plugin, player, user, identifier)
														.open(GUIMethod.CHEST);
											}
										});

									}
								} else {
									user.sendMessage(plugin.getShopFile().getVoteShopLimitReached());
								}
								plugin.getCommandLoader().processSlotClick(player, user, identifier);
								if (plugin.getShopFile().isVoteShopReopenGUIOnPurchase()) {
									plugin.getCommandLoader().processSlotClick(player, user, "shop");
								}
							}
						}

					}.addData("identifier", identifier).addData("Limit", limit));
				}
			} else {
				if (hasPerm) {
					ItemBuilder builder = new ItemBuilder(plugin.getShopFile().getShopIdentifierSection(identifier));
					inv.addButton(new BInventoryButton(builder) {

						@Override
						public void onClick(ClickEvent event) {
							user.sendMessage(plugin.getConfigFile().getFormatShopNotPurchasable());
						}

					}.addData("identifier", identifier).addData("Limit", limit));
				}
			}
		}

		for (final String str : plugin.getShopFile().getVoteShopExtraItems()) {
			inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getGUIVoteShopExtraItems(str))) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					plugin.getCommandLoader().processSlotClick(player, user, str);
					new RewardBuilder(plugin.getShopFile().getData(),
							"ExtraItems." + str + "." + clickEvent.getButton().getLastRewardsPath(player))
							.setGiveOffline(false).send(clickEvent.getPlayer());
				}
			});
		}

		if (plugin.getShopFile().isVoteShopBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
