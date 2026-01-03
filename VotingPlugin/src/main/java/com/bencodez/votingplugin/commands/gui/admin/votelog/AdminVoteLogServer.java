package com.bencodez.votingplugin.commands.gui.admin.votelog;

import java.util.ArrayList;
import java.util.List;

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
 * Admin GUI: VoteLog entries for a given server (paged).
 *
 * Uses VoteLogMysqlTable instance passed in from CommandLoader.
 */
public class AdminVoteLogServer extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;
	private final int page;

	private final String server;
	private final int days;

	private static final int PAGE_SIZE = 45;

	public AdminVoteLogServer(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, String server,
			int days, int page) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.server = server;
		this.days = Math.max(0, days);
		this.page = Math.max(0, page);

		if (sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
	}

	public AdminVoteLogServer(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, String server, int days, int page) {
		this(plugin, sender, table, server, days, page);
		this.user = user;
		if (this.user == null && sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
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
			BInventory inv = new BInventory("&aVoteLog - Server &7(" + AdminVoteLogHelpers.safe(server) + ")");
			inv.requirePermission("VotingPlugin.Commands.AdminVote.VoteLog");

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

			int needed = (page + 1) * PAGE_SIZE;
			List<VoteLogMysqlTable.VoteLogEntry> rows = table.getByServer(server, days, needed);

			int start = page * PAGE_SIZE;
			int end = Math.min(rows.size(), start + PAGE_SIZE);

			if (start >= rows.size()) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&eNo entries")
						.addLoreLine("&7No vote log entries on this page.").addLoreLine("&7Try going back a page.")) {
					@Override
					public void onClick(ClickEvent clickEvent) {
						// no-op
					}
				});
			} else {
				for (int i = start; i < end; i++) {
					inv.addButton(makeEntryButton(rows.get(i)));
				}
			}

			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private BInventoryButton makeEntryButton(final VoteLogMysqlTable.VoteLogEntry entry) {
		VoteLogMysqlTable.VoteLogEvent event =
				entry.event != null ? VoteLogMysqlTable.VoteLogEvent.valueOf(entry.event) : null;

		Material mat = AdminVoteLogHelpers.getMaterialForEvent(event, entry.status);
		String eventColor = AdminVoteLogHelpers.getEventColor(event);

		String timeStr = AdminVoteLogHelpers.formatTime(entry.voteTime);
		String uuidShort = AdminVoteLogHelpers.shortUuid(entry.playerUuid);
		String voteShort = AdminVoteLogHelpers.shortUuid(entry.voteId);

		ItemBuilder item = new ItemBuilder(mat)
				.setName(eventColor + AdminVoteLogHelpers.safe(entry.event) + " &7(" + voteShort + ")");

		if (AdminVoteLogHelpers.notEmpty(entry.playerName) || AdminVoteLogHelpers.notEmpty(entry.playerUuid)) {
			item.addLoreLine("&7Player: &f" + AdminVoteLogHelpers.safe(entry.playerName) + " &7(" + uuidShort + ")");
		}
		if (entry.voteTime > 0) {
			item.addLoreLine("&7Time: &f" + timeStr);
		}
		if (AdminVoteLogHelpers.notEmpty(entry.server)) {
			item.addLoreLine("&7Server: &f" + AdminVoteLogHelpers.safe(entry.server));
		}
		if (AdminVoteLogHelpers.notEmpty(entry.service)) {
			item.addLoreLine("&7Service: &f" + AdminVoteLogHelpers.safe(entry.service));
		}
		if (AdminVoteLogHelpers.notEmpty(entry.context)) {
			item.addLoreLine("&7Context: &f" + AdminVoteLogHelpers.safe(entry.context));
		}
		if (AdminVoteLogHelpers.notEmpty(entry.status)) {
			item.addLoreLine("&7Status: &f" + AdminVoteLogHelpers.safe(entry.status));
		}
		if (entry.proxyCachedTotal != 0) {
			item.addLoreLine("&7Cached Total: &f" + entry.proxyCachedTotal);
		}
		if (AdminVoteLogHelpers.notEmpty(entry.voteId)) {
			item.addLoreLine("&7VoteId: &f" + AdminVoteLogHelpers.safe(entry.voteId));
		}

		return new BInventoryButton(item) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				// vote_id is the correlation id
				new AdminVoteLogVoteId(plugin, clickEvent.getPlayer(), table, entry.voteId, days, 0).open();
			}
		};
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
