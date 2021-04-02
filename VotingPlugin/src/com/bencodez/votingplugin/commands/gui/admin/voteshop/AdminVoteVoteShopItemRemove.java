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
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteConfirmation;

public class AdminVoteVoteShopItemRemove extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteVoteShopItemRemove(VotingPluginMain plugin, CommandSender player) {
		super(player);
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
		BInventory inv = new BInventory("Edit VoteShop Remove Item");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");
		
		for (final String identifier : plugin.getGui().getChestShopIdentifiers()) {
			inv.addButton(
					new BInventoryButton(new ItemBuilder(plugin.getGui().getChestShopIdentifierSection(identifier))
							.addLoreLine("&c&lClick to remove")) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							new AdminVoteConfirmation(plugin, clickEvent.getPlayer(),
									"Remove shop item " + identifier + "?") {

								@Override
								public void onDeny(Player p) {
									new AdminVoteVoteShop(plugin, p).open();
								}

								@Override
								public void onConfirm(Player p) {
									plugin.getGui().removeShop(identifier);
									p.sendMessage("Removed " + identifier);
									plugin.reload();
								}
							};

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
