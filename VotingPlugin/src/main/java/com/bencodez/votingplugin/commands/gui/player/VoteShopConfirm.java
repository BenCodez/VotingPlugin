package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.simpleapi.player.PlayerUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

/**
 * Confirmation GUI for vote shop purchases.
 */
public class VoteShopConfirm extends GUIHandler {

	private VoteShopCategory category;

	private VoteShopItem item;

	private VotingPluginMain plugin;

	private VotingPluginUser user;

	/**
	 * Creates the GUI.
	 *
	 * @param plugin   the plugin
	 * @param player   the sender
	 * @param user     the user
	 * @param item     the item
	 * @param category the category or null
	 */
	public VoteShopConfirm(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, VoteShopItem item,
			VoteShopCategory category) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.item = item;
		this.category = category;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(final Player player) {
		PlayerUtils.setPlayerMeta(plugin, player, "ident", item.getIdentifier());
		BInventory inv = new BInventory(plugin.getShopFile().getShopConfirmPurchaseTitle());
		inv.dontClose();
		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseYesItem())) {

			@Override
			public void onClick(ClickEvent event) {
				user.cache();
				VoteShopPurchaseResult result = plugin.getVoteShopManager().purchase(player, user, item);
				if (result != VoteShopPurchaseResult.SUCCESS) {
					plugin.getVoteShopManager().getPurchaseService().sendFailureMessage(player, user, item, result);
					returnToPrevious(event.getPlayer());
					return;
				}

				plugin.getCommandLoader().processSlotClick(player, user, item.getIdentifier());
				if (item.isCloseGUI()) {
					event.getButton().getInv().closeInv(player, null);
				} else {
					returnToPrevious(event.getPlayer());
				}
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseNoItem())) {

			@Override
			public void onClick(ClickEvent event) {
				if (item.isCloseGUI()) {
					event.closeInventory()
				} else {
					returnToPrevious(event.getPlayer());
				}
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void onDialog(Player player) {
		PlayerUtils.setPlayerMeta(plugin, player, "ident", item.getIdentifier());

		plugin.getDialogService().confirmation(player).placeholder("player", user.getPlayerName())
				.placeholder("identifier", item.getIdentifier())
				.title(plugin.getShopFile().getShopConfirmPurchaseTitle())
				.body("&7Confirm purchase of &e%identifier%&7?")
				.yesText(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseYesItem()).getName())
				.noText(new ItemBuilder(plugin.getShopFile().getShopConfirmPurchaseNoItem()).getName())
				.onYes(payload -> {
					Player clicked = player.getServer().getPlayer(payload.owner());
					if (clicked == null) {
						return;
					}

					user.cache();
					VoteShopPurchaseResult result = plugin.getVoteShopManager().purchase(clicked, user, item);
					if (result != VoteShopPurchaseResult.SUCCESS) {
						plugin.getVoteShopManager().getPurchaseService().sendFailureMessage(clicked, user, item,
								result);
						returnToPrevious(clicked);
						return;
					}

					plugin.getCommandLoader().processSlotClick(clicked, user, item.getIdentifier());
					if (!item.isCloseGUI()) {
						returnToPrevious(clicked);
					}
				}).onNo(payload -> {
					Player clicked = player.getServer().getPlayer(payload.owner());
					if (clicked != null) {
						returnToPrevious(clicked);
					}
				}).open();
	}

	/**
	 * Returns to the previous GUI.
	 *
	 * @param player the player
	 */
	protected void returnToPrevious(Player player) {
		if (category != null) {
			new VoteShopCategoryMenu(plugin, player, user, category).open(GUIMethod.CHEST);
		} else {
			new VoteShop(plugin, player, user).open(GUIMethod.CHEST);
		}
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}