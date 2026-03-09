package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategory;
import com.bencodez.votingplugin.voteshop.shop.VoteShopCategoryButton;
import com.bencodez.votingplugin.voteshop.shop.VoteShopEntry;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

/**
 * Vote shop category GUI.
 */
public class VoteShopCategoryMenu extends GUIHandler {

	private VoteShopCategory category;

	private VotingPluginMain plugin;

	@SuppressWarnings("unused")
	private VotingPluginUser user;

	/**
	 * Creates the GUI.
	 *
	 * @param plugin   the plugin
	 * @param player   the sender
	 * @param user     the user
	 * @param category the category
	 */
	public VoteShopCategoryMenu(VotingPluginMain plugin, CommandSender player, VotingPluginUser user,
			VoteShopCategory category) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
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
	public void onDialog(Player player) {
		
	}

	@Override
	public void onChest(Player player) {
		if (!plugin.getVoteShopManager().getDefinition().isEnabled()) {
			player.sendMessage(MessageAPI.colorize(plugin.getVoteShopManager().getDefinition().getDisabledMessage()));
			return;
		}

		VotingPluginUser currentUser = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		BInventory inv = new BInventory(category.getName());
		inv.addPlaceholder("points", String.valueOf(currentUser.getPoints()));
		inv.addPlaceholder("sitesavailable", String.valueOf(currentUser.getSitesNotVotedOn()));
		inv.dontClose();

		for (VoteShopEntry entry : category.getEntries().values()) {
			if (entry instanceof VoteShopItem) {
				addItemButton(inv, player, currentUser, (VoteShopItem) entry);
			} else if (entry instanceof VoteShopCategoryButton) {
				addCategoryButton(inv, player, currentUser, (VoteShopCategoryButton) entry);
			}
		}

		if (category.isBackButton()) {
			addBackButton(inv, player, currentUser, category.getId());
		}

		inv.openInventory(player);
	}

	private void addBackButton(BInventory inv, Player player, VotingPluginUser user, String category) {

		if (!plugin.getShopFile().isCategoryBackButtonEnabled(category)) {
			return;
		}

		ConfigurationSection section = plugin.getShopFile().getCategoryBackButtonItem(category);

		if (section == null) {
			section = plugin.getShopFile().getDefaultBackButtonItem();
		}

		ItemBuilder builder;

		if (section != null) {
			builder = new ItemBuilder(section);
		} else {
			builder = new ItemBuilder(Material.ARROW);
			builder.setName("&cBack");
		}

		inv.addButton(new BInventoryButton(builder) {

			@Override
			public void onClick(ClickEvent event) {
				new VoteShop(plugin, player, user).open();
			}

		});
	}

	/**
	 * Adds an item button.
	 *
	 * @param inv         the inventory
	 * @param player      the player
	 * @param currentUser the user
	 * @param item        the item
	 */
	protected void addItemButton(BInventory inv, final Player player, final VotingPluginUser currentUser,
			final VoteShopItem item) {
		new VoteShop(plugin, player, currentUser).addItemButton(inv, player, currentUser, item, category);
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
		inv.addButton(new BInventoryButton(new ItemBuilder(button.getDisplaySection())) {

			@Override
			public void onClick(ClickEvent event) {
				VoteShopCategory target = plugin.getVoteShopManager().getCategory(button.getCategoryId());
				if (target != null) {
					new VoteShopCategoryMenu(plugin, event.getPlayer(), currentUser, target).open(GUIMethod.CHEST);
				}
			}
		});
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}