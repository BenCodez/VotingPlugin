package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;

public class AdminVoteVotePlayer extends GUIHandler {

	private String playerName;
	private VotingPluginMain plugin;

	public AdminVoteVotePlayer(VotingPluginMain plugin, CommandSender player, String playerName) {
		super(plugin, player);
		this.plugin = plugin;
		this.playerName = playerName;
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
		EditGUI inv = new EditGUI("Trigger vote for " + playerName);
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Vote|VotingPlugin.Admin");

		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(new BInventoryButton(site.getItem().setName(site.getKey())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					VoteSite site = (VoteSite) getData("site");
					PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, playerName, site.getServiceSite(), false);
					sendMessage(clickEvent.getPlayer(), "&cTriggering vote...");
					if (voteEvent.getVoteSite() != null) {
						if (!voteEvent.getVoteSite().isVaidServiceSite()) {
							sendMessage(clickEvent.getPlayer(),
									"&cPossible issue with service site, has the server gotten the vote from "
											+ voteEvent.getServiceSite() + "?");
						}
					}
					plugin.getServer().getPluginManager().callEvent(voteEvent);

					if (plugin.isYmlError()) {
						sendMessage(clickEvent.getPlayer(),
								"&3Detected yml error, please check server log for details");
					}
				}
			}.addData("site", site));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
