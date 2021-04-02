package com.bencodez.votingplugin.commands.gui.admin.voteshop;

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
import com.bencodez.advancedcore.api.valuerequest.ValueRequest;
import com.bencodez.advancedcore.api.valuerequest.listeners.StringListener;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteVoteShop extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteVoteShop(VotingPluginMain plugin, CommandSender player) {
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
		BInventory inv = new BInventory("Edit VoteShop");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteShop");
		inv.addButton(new BInventoryButton(new ItemBuilder(Material.DIAMOND).setName("&cEdit existing item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItems(plugin, clickEvent.getPlayer()).open(GUIMethod.CHEST);
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.EMERALD_BLOCK).setName("&aAdd voteshop item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new ValueRequest().requestString(player, new StringListener() {

					@Override
					public void onInput(Player player, String value) {
						plugin.getGui().createShop(value);
						plugin.reload();
					}
				});
			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(Material.REDSTONE_BLOCK).setName("&cRemove item")) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteVoteShopItemRemove(plugin, clickEvent.getPlayer()).open();
			}
		});
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
