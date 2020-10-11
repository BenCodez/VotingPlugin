package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteTopVoterLastMonth extends GUIHandler {

	private User user;
	private Main plugin;

	public VoteTopVoterLastMonth(Main plugin, CommandSender player, User user) {
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
		Set<Entry<User, Integer>> users = null;

		users = plugin.getLastMonthTopVoter().entrySet();

		BInventory inv = new BInventory(Config.getInstance().getGUIVoteTopName());
		inv.addPlaceholder("topvoter", "Last Month");
		if (!Config.getInstance().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		int pos = 1;
		for (Entry<User, Integer> entry : users) {
			ItemBuilder playerItem;

			if (Config.getInstance().isGuiVoteTopUseSkull()) {
				playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
			} else {
				playerItem = new ItemBuilder(Material.valueOf(Config.getInstance().getGuiVoteTopPlayerItemMaterial()));
			}

			playerItem.setLore(new ArrayList<String>());

			inv.addButton(new BInventoryButton(playerItem.setName(Config.getInstance().getGUIVoteTopItemName())
					.addLoreLine(Config.getInstance().getGUIVoteTopItemLore()).addPlaceholder("position", "" + pos)
					.addPlaceholder("player", entry.getKey().getPlayerName())
					.addPlaceholder("votes", "" + entry.getValue())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					User user = (User) getData("User");
					new VoteGUI(plugin, player, user).open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodGUI().toUpperCase()));
				}
			}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey()));
			pos++;
		}

		if (Config.getInstance().getGUIVoteTopBackButton()) {
			inv.getPageButtons().add(CommandLoader.getInstance().getBackButton(user).setSlot(1));
		}

		inv.setPages(true);
		inv.setMaxInvSize(Config.getInstance().getGUIVoteTopSize());
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
