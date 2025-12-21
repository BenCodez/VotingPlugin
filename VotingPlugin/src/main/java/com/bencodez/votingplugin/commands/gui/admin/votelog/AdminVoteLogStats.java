package com.bencodez.votingplugin.commands.gui.admin.votelog;

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
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;

/**
 * Admin GUI: VoteLog stats overview.
 *
 * Uses VoteLogMysqlTable instance passed in from CommandLoader.
 *
 * Shows:
 * - total / immediate / cached counts (VoteLogCounts)
 * - unique voters
 * - top services
 */
public class AdminVoteLogStats extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;

	private final int days;
	private final int topServicesLimit;

	public AdminVoteLogStats(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, int days) {
		this(plugin, sender, table, null, days, 10);
	}

	public AdminVoteLogStats(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, VotingPluginUser user,
			int days, int topServicesLimit) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.user = user;
		if (this.user == null && sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
		this.days = days;
		this.topServicesLimit = topServicesLimit <= 0 ? 10 : topServicesLimit;
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
		try {
			String title = "&aVoteLog - Stats";
			if (days > 0) {
				title += " &7(" + days + "d)";
			}

			BInventory inv = new BInventory(title);
			inv.requirePermission("VotingPlugin.Commands.AdminVote.VoteLog.Stats");

			if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
				inv.dontClose();
			}

			if (table == null) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.BARRIER).setName("&cVoteLog unavailable")
						.addLoreLine("&7VoteLog MySQL table is not loaded/enabled.")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						// no-op
					}
				});

				inv.setPages(true);
				inv.setMaxInvSize(54);
				inv.openInventory(player);
				return;
			}

			// counts
			VoteLogMysqlTable.VoteLogCounts counts = table.getCounts(days);
			long uniques = table.getUniqueVoters(days);

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.BOOK).setName("&aOverview")
					.addLoreLine("&7Total Votes: &f" + counts.total)
					.addLoreLine("&7Immediate: &f" + counts.immediate)
					.addLoreLine("&7Cached: &f" + counts.cached)
					.addLoreLine("&7Unique Voters: &f" + uniques)
					.addLoreLine(days > 0 ? "&7Window: &fLast " + days + " days" : "&7Window: &fAll time")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					// no-op
				}
			});

			// top services
			inv.addButton(new BInventoryButton(new ItemBuilder(Material.MAP).setName("&aTop Services")
					.addLoreLine("&7Top " + topServicesLimit + " services/sites:")
					.addLoreLine("&8(Click does nothing)")) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					// no-op
				}
			});

			int shown = 0;
			for (VoteLogMysqlTable.ServiceCount sc : table.getTopServices(days, topServicesLimit)) {
				shown++;
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&a#" + shown + " &f" + safe(sc.service))
						.addLoreLine("&7Votes: &f" + sc.votes)
						.addLoreLine(days > 0 ? "&7Window: &fLast " + days + " days" : "&7Window: &fAll time")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						// no-op (could open AdminVoteLogService in future)
					}
				});
			}

			if (shown == 0) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&eNo service data")
						.addLoreLine("&7No services found in the log.")) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						// no-op
					}
				});
			}

			if (user != null && plugin.getGui().isChestVoteTopBackButton()) {
				inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(7));
			}

			// Use pages so the bottom bar is consistent with other GUIs (even if not needed)
			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private String safe(String s) {
		return s == null ? "" : s;
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
