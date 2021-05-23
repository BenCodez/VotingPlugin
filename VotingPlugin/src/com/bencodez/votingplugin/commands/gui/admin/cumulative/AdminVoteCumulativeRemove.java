package com.bencodez.votingplugin.commands.gui.admin.cumulative;

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

public class AdminVoteCumulativeRemove extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteCumulativeRemove(VotingPluginMain plugin, CommandSender player) {
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
		BInventory inv = new BInventory("Remove cumulative");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.Cumulative");
		for (final String votes : plugin.getSpecialRewardsConfig().getCumulativeVotes()) {
			inv.addButton(new BInventoryButton(
					new ItemBuilder(Material.PAPER, 1).setName(votes).addLoreLine("&c&lClick to remove")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteConfirmation(plugin, clickEvent.getPlayer(), "Remove cumulative " + votes + "?") {

						@Override
						public void onConfirm(Player p) {
							plugin.getSpecialRewardsConfig().removeCumulative(votes);
							p.sendMessage("Removed cumulative " + votes);
							plugin.reload();
						}

						@Override
						public void onDeny(Player p) {
							new AdminVoteCumulative(plugin, p);
						}
					}.open();
					;

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
