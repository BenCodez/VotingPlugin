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
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.VoteShopManager;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseResult;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopDefinition;
import com.bencodez.votingplugin.voteshop.shop.VoteShopEntry;
import com.bencodez.votingplugin.voteshop.shop.VoteShopExtraItem;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

/**
 * Main vote shop GUI.
 */
public class VoteShop extends GUIHandler {

	private VotingPluginMain plugin;

	private VotingPluginUser user;

	/**
	 * Creates the GUI.
	 *
	 * @param plugin the plugin
	 * @param player the sender
	 * @param user   the user
	 */
	public VoteShop(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
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
	public void onDialog(Player player) {

	}

	@Override
	public void onChest(Player player) {
		VoteShopManager manager = plugin.getVoteShopManager();
		VoteShopDefinition definition = manager.getDefinition();

		if (!definition.isEnabled()) {
			player.sendMessage(MessageAPI.colorize(definition.getDisabledMessage()));
			return;
		}

		VotingPluginUser currentUser = getUser(player);
		BInventory inv = new BInventory(definition.getTitle());
		inv.addPlaceholder("points", String.valueOf(currentUser.getPoints()));
		inv.addPlaceholder("sitesavailable", String.valueOf(currentUser.getSitesNotVotedOn()));
		inv.dontClose();

		for (VoteShopEntry entry : definition.getMainEntries().values()) {
			if (entry instanceof VoteShopItem) {
				addItemButton(inv, player, currentUser, (VoteShopItem) entry, null);
			} else if (entry instanceof VoteShopCategoryButton) {
				addCategoryButton(inv, player, currentUser, (VoteShopCategoryButton) entry);
			}
		}

		for (VoteShopExtraItem extraItem : definition.getExtraItems().values()) {
			addExtraItemButton(inv, player, currentUser, extraItem);
		}

		if (definition.isBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(currentUser));
		}

		inv.openInventory(player);
	}

	/**
	 * Adds an item button.
	 *
	 * @param inv         the inventory
	 * @param player      the player
	 * @param currentUser the user
	 * @param item        the item
	 * @param category    the category, null for main shop
	 */
	protected void addItemButton(BInventory inv, final Player player, final VotingPluginUser currentUser,
			final VoteShopItem item, final VoteShopCategory category) {
		VoteShopPurchaseResult validation = plugin.getVoteShopManager().getPurchaseService().validatePurchase(player,
				currentUser, item);
		boolean hasPermission = plugin.getVoteShopManager().getPurchaseService().hasPermission(player,
				item.getPermission());

		if ((!hasPermission && item.isHideOnNoPermission()) || (validation == VoteShopPurchaseResult.LIMIT_REACHED
				&& plugin.getVoteShopManager().getDefinition().isHideLimitedReached())) {
			return;
		}

		inv.addButton(new BInventoryButton(new ItemBuilder(item.getDisplaySection())) {

			@Override
			public void onClick(ClickEvent event) {
				VotingPluginUser clickedUser = getUser(event.getPlayer());
				if (plugin.getConfigFile().isExtraVoteShopCheck()) {
					clickedUser.cache();
				}

				if (item.isNotBuyable()) {
					clickedUser.sendMessage(plugin.getConfigFile().getFormatShopNotPurchasable());
					return;
				}

				VoteShopPurchaseResult clickValidation = plugin.getVoteShopManager().getPurchaseService()
						.validatePurchase(event.getPlayer(), clickedUser, item);
				if (clickValidation != VoteShopPurchaseResult.SUCCESS) {
					plugin.getVoteShopManager().getPurchaseService().sendFailureMessage(event.getPlayer(), clickedUser,
							item, clickValidation);
					return;
				}

				if (item.isRequireConfirmation()) {
					if (plugin.getShopFile().isShopConfirmPurchaseUseDialog()) {
						new VoteShopConfirm(plugin, event.getPlayer(), clickedUser, item, category)
								.open(GUIMethod.DIALOG);
						return;
					} else {
						new VoteShopConfirm(plugin, event.getPlayer(), clickedUser, item, category)
								.open(GUIMethod.CHEST);
						return;
					}
				}

				handlePurchase(event.getPlayer(), clickedUser, item, category);
			}
		}.addData("identifier", item.getIdentifier()));
	}

	/**
	 * Adds a category button.
	 *
	 * @param inv         the inventory
	 * @param player      the player
	 * @param currentUser the user
	 * @param button      the button
	 */
	protected void addCategoryButton(BInventory inv, final Player player, final VotingPluginUser currentUser,
			final VoteShopCategoryButton button) {
		boolean hasPermission = plugin.getVoteShopManager().getPurchaseService().hasPermission(player,
				button.getPermission());
		if (!hasPermission && button.isHideOnNoPermission()) {
			return;
		}

		final VoteShopCategory category = plugin.getVoteShopManager().getCategory(button.getCategoryId());
		if (category == null) {
			plugin.extraDebug(
					"VoteShop: missing category '" + button.getCategoryId() + "' for entry " + button.getIdentifier());
			return;
		}

		inv.addButton(new BInventoryButton(new ItemBuilder(button.getDisplaySection())) {

			@Override
			public void onClick(ClickEvent event) {
				new VoteShopCategoryMenu(plugin, event.getPlayer(), currentUser, category).open(GUIMethod.CHEST);
			}
		}.addData("identifier", button.getIdentifier()));
	}

	/**
	 * Adds an extra item button.
	 *
	 * @param inv         the inventory
	 * @param player      the player
	 * @param currentUser the user
	 * @param extraItem   the extra item
	 */
	protected void addExtraItemButton(BInventory inv, final Player player, final VotingPluginUser currentUser,
			final VoteShopExtraItem extraItem) {
		inv.addButton(new BInventoryButton(new ItemBuilder(extraItem.getDisplaySection())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				plugin.getCommandLoader().processSlotClick(player, currentUser, extraItem.getIdentifier());
				new RewardBuilder(plugin.getShopFile().getData(),
						"ExtraItems." + extraItem.getIdentifier() + "."
								+ clickEvent.getButton().getLastRewardsPath(player))
						.setGiveOffline(false).send(clickEvent.getPlayer());
			}
		});
	}

	/**
	 * Handles a completed purchase.
	 *
	 * @param player      the player
	 * @param currentUser the user
	 * @param item        the item
	 * @param category    the category or null
	 */
	protected void handlePurchase(Player player, VotingPluginUser currentUser, VoteShopItem item,
			VoteShopCategory category) {
		VoteShopPurchaseResult result = plugin.getVoteShopManager().purchase(player, currentUser, item);
		if (result != VoteShopPurchaseResult.SUCCESS) {
			plugin.getVoteShopManager().getPurchaseService().sendFailureMessage(player, currentUser, item, result);
			return;
		}

		plugin.getCommandLoader().processSlotClick(player, currentUser, item.getIdentifier());
		if (plugin.getVoteShopManager().getDefinition().isReopenGuiOnPurchase()) {
			if (category != null) {
				new VoteShopCategoryMenu(plugin, player, currentUser, category).open(GUIMethod.CHEST);
			} else {
				plugin.getCommandLoader().processSlotClick(player, currentUser, "shop");
			}
		}
	}

	/**
	 * Gets the current user.
	 *
	 * @param player the player
	 * @return the user
	 */
	protected VotingPluginUser getUser(Player player) {
		if (user == null) {
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		}
		return plugin.getVotingPluginUserManager().getVotingPluginUser(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}