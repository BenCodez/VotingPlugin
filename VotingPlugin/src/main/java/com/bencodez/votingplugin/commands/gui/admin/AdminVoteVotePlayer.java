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
import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * Admin vote player GUI handler.
 */
public class AdminVoteVotePlayer extends GUIHandler {

	private String playerName;
	private VotingPluginMain plugin;

	/**
	 * Constructor for AdminVoteVotePlayer.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 * @param playerName the player name
	 */
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

		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
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
					dispatchVote(clickEvent.getPlayer(), voteEvent);

					if (plugin.isYmlError()) {
						sendMessage(clickEvent.getPlayer(),
								"&3Detected yml error, please check server log for details");
					}
				}
			}.addData("site", site));
		}

		inv.openInventory(player);
	}

	void dispatchVote(Player player, PlayerVoteEvent voteEvent) {
		// Dispatch from the player-owned thread so PlayerVoteListener can capture
		// Bukkit state before it hands storage and accounting to the vote worker.
		plugin.getServer().getPluginManager().callEvent(voteEvent);
		voteEvent.getProcessingCompletion().whenComplete((completed, failure) -> {
			if (failure == null && !completed.isProcessingIncomplete()) return;
			plugin.getBukkitScheduler().runTask(plugin,
					() -> sendMessage("&cVote could not be processed because shared storage is unavailable."),
					player);
		});
	}
	
	@Override
	public void onDialog(Player player) {
		
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
