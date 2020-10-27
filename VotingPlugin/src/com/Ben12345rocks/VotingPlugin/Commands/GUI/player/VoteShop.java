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
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.BungeeSettings;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class VoteShop extends GUIHandler {

	private User user;
	private Main plugin;

	public VoteShop(Main plugin, CommandSender player, User user) {
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
				User user = UserManager.getInstance().getVotingPluginUser(player);
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

							User user = UserManager.getInstance().getVotingPluginUser(player);
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
