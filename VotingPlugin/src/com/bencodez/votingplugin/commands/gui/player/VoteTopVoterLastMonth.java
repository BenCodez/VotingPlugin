package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Set;

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
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteTopVoterLastMonth extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;

	public VoteTopVoterLastMonth(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {
		Set<Entry<VotingPluginUser, Integer>> users = null;

		users = plugin.getLastMonthTopVoter().entrySet();

		BInventory inv = new BInventory(GUI.getInstance().getChestVoteTopName());
		inv.addPlaceholder("topvoter", "Last Month");
		if (!Config.getInstance().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		int pos = 1;
		for (Entry<VotingPluginUser, Integer> entry : users) {
			ItemBuilder playerItem;

			if (GUI.getInstance().isChestVoteTopUseSkull()) {
				playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
			} else {
				playerItem = new ItemBuilder(Material.valueOf(GUI.getInstance().getChestVoteTopPlayerItemMaterial()));
			}

			playerItem.setLore(new ArrayList<String>());

			inv.addButton(new BInventoryButton(playerItem.setName(GUI.getInstance().getChestVoteTopItemName())
					.addLoreLine(GUI.getInstance().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
					.addPlaceholder("player", entry.getKey().getPlayerName())
					.addPlaceholder("votes", "" + entry.getValue())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					VotingPluginUser user = (VotingPluginUser) getData("User");
					new VoteGUI(plugin, player, user).open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodGUI().toUpperCase()));
				}
			}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey()));
			pos++;
		}

		if (GUI.getInstance().getChestVoteTopBackButton()) {
			inv.getPageButtons().add(CommandLoader.getInstance().getBackButton(user).setSlot(1));
		}

		inv.setPages(true);
		inv.setMaxInvSize(GUI.getInstance().getChestVoteTopSize());
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
