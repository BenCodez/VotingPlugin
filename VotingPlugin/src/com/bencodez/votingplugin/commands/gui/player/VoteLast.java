package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteLast extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;

	public VoteLast(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		// TODO
	}
	
	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();

		String playerName = user.getPlayerName();

		msg.add(StringParser.getInstance().replaceIgnoreCase(Config.getInstance().getFormatCommandsVoteLastTitle(),
				"%player%", playerName));

		for (VoteSite voteSite : plugin.getVoteSites()) {
			msg.add(user.voteCommandLastLine(voteSite));
		}

		return ArrayUtils.getInstance().colorize(msg);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteLastName());
		inv.addPlaceholder("player", user.getPlayerName());
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(), new UpdatingBInventoryButton(site.getItem().setName(site.getDisplayName())
					.setLore(user.voteCommandLastLine(site)).setAmountNone(1), 1000, 1000) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}

				@Override
				public ItemBuilder onUpdate(Player p) {
					return site.getItem().setName(site.getDisplayName()).setLore(user.voteCommandLastLine(site))
							.setAmountNone(1);
				}
			});
		}

		if (GUI.getInstance().getChestVoteLastBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}

		inv.openInventory(player);
	}
	
	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodLast().toUpperCase()));
	}


}
