package com.bencodez.votingplugin.commands.gui.admin.voteshop;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteVoteShopItems extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteVoteShopItems(VotingPluginMain plugin, CommandSender player) {
		super(plugin, player);
		this.plugin = plugin;
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
		BInventory inv = new BInventory("Edit VoteShop Items");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");

		for (String identifier : plugin.getGui().getChestShopIdentifiers()) {
			inv.addButton(
					new BInventoryButton(new ItemBuilder(plugin.getGui().getChestShopIdentifierSection(identifier))) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							new AdminVoteVoteShopItem(plugin, clickEvent.getPlayer(), (String) getData("ident"))
									.open(GUIMethod.CHEST);

						}
					}.addData("ident", identifier));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
