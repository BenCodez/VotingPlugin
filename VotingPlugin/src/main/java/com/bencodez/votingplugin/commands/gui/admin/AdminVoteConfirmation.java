package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Abstract class for admin vote confirmation dialogs.
 */
public abstract class AdminVoteConfirmation extends GUIHandler {

	@SuppressWarnings("unused")
	private VotingPluginMain plugin;
	private String title;

	/**
	 * Constructor for AdminVoteConfirmation.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 * @param title the title of the confirmation dialog
	 */
	public AdminVoteConfirmation(VotingPluginMain plugin, CommandSender player, String title) {
		super(plugin, player);
		this.plugin = plugin;
		this.title = title;
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
	public void onChest(Player player) {
		BInventory inv = new BInventory(title);
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aConfirm")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				onConfirm(clickEvent.getPlayer());
			}
		});
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&aCancel")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				onDeny(clickEvent.getPlayer());
			}
		});
		inv.openInventory(player);
	}

	/**
	 * Called when the user confirms the action.
	 *
	 * @param p the player who confirmed
	 */
	public abstract void onConfirm(Player p);

	/**
	 * Called when the user denies the action.
	 *
	 * @param p the player who denied
	 */
	public abstract void onDeny(Player p);

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
