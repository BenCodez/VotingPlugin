package com.bencodez.votingplugin.commands.gui.admin.milestones;

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
import com.bencodez.votingplugin.commands.gui.admin.AdminVoteConfirmation;

/**
 * Admin vote milestone remove GUI handler.
 */
public class AdminVoteVoteMilestoneRemove extends GUIHandler {

	private VotingPluginMain plugin;

	/**
	 * Constructor for AdminVoteVoteMilestoneRemove.
	 *
	 * @param plugin the VotingPluginMain instance
	 * @param player the command sender
	 */
	public AdminVoteVoteMilestoneRemove(VotingPluginMain plugin, CommandSender player) {
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
	public void onDialog(Player player) {

	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory("Remove VoteMilestones");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.VoteMilestones");
		for (final String votes : plugin.getVoteMilestonesManager().getConfig().getMilestones().keySet()) {
			inv.addButton(new BInventoryButton(
					new ItemBuilder(Material.PAPER, 1).setName(votes).addLoreLine("&c&lClick to remove")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteConfirmation(plugin, clickEvent.getPlayer(), "Remove milestone " + votes + "?") {

						@Override
						public void onConfirm(Player p) {
							plugin.getSpecialRewardsConfig().removeVoteMilestone(votes);
							p.sendMessage("Removed milestone " + votes);
							plugin.reload();
						}

						@Override
						public void onDeny(Player p) {
							new AdminVoteVoteMilestones(plugin, p);
						}
					}.open();

				}
			});
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
