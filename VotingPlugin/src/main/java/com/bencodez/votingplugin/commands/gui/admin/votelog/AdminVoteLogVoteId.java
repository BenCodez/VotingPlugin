package com.bencodez.votingplugin.commands.gui.admin.votelog;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
 * Admin GUI: VoteLog entries for a given voteId (paged).
 *
 * vote_id is NOT unique; this shows ALL rows for the correlation id.
 */
public class AdminVoteLogVoteId extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;
	private final int page;

	private final String voteId;
	private final int days;

	private static final int PAGE_SIZE = 45;

	public AdminVoteLogVoteId(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, String voteId,
			int days, int page) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.voteId = voteId;
		this.days = days;
		this.page = Math.max(0, page);

		if (sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
	}

	public AdminVoteLogVoteId(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, String voteId, int days, int page) {
		this(plugin, sender, table, voteId, days, page);
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
			String title = "&aVoteLog - VoteId";
			if (voteId != null && !voteId.isEmpty()) {
				title += " &7(" + shortId(voteId) + ")";
			}

			BInventory inv = new BInventory(title);
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

			if (voteId == null || voteId.isEmpty()) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&eNo voteId provided")
						.addLoreLine("&7Missing voteId.")) {
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

			// vote_id is not unique -> show ALL rows for it (paged)
			List<VoteLogMysqlTable.VoteLogEntry> rows = table.getByVoteIdAll(voteId, days, needed);

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

	private BInventoryButton makeEntryButton(VoteLogMysqlTable.VoteLogEntry entry) {
		VoteLogMysqlTable.VoteLogEvent event = entry.event != null ? VoteLogMysqlTable.VoteLogEvent.valueOf(entry.event)
				: null;

		Material mat = AdminVoteLogHelpers.getMaterialForEvent(event, entry.status);

		String eventColor = AdminVoteLogHelpers.getEventColor(event);

		String timeStr = formatTime(entry.voteTime);
		String uuidShort = shortId(entry.playerUuid);
		String voteShort = shortId(entry.voteId);

		ItemBuilder item = new ItemBuilder(mat)
				.setName(eventColor + AdminVoteLogHelpers.safe(entry.event) + " &7(" + voteShort + ")");

		// Only add lore lines when the value is actually set / meaningful
		// (prevents blank/0/default noise)
		if (notEmpty(entry.playerName) || notEmpty(entry.playerUuid)) {
			item.addLoreLine("&7Player: &f" + safe(entry.playerName) + " &7(" + uuidShort + ")");
		}

		if (entry.voteTime > 0) {
			item.addLoreLine("&7Time: &f" + timeStr);
		}

		if (notEmpty(entry.service)) {
			item.addLoreLine("&7Service: &f" + safe(entry.service));
		}

		if (notEmpty(entry.context)) {
			item.addLoreLine("&7Context: &f" + safe(entry.context));
		}

		if (notEmpty(entry.status)) {
			item.addLoreLine("&7Status: &f" + safe(entry.status));
		}

		if (entry.proxyCachedTotal != 0) {
			item.addLoreLine("&7Cached Total: &f" + entry.proxyCachedTotal);
		}

		if (notEmpty(entry.voteId)) {
			item.addLoreLine("&7VoteId: &f" + safe(entry.voteId));
		}

		return new BInventoryButton(item) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				// read-only
			}
		};
	}

	private String formatTime(long millis) {
		try {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			return sdf.format(new Date(millis));
		} catch (Exception e) {
			return String.valueOf(millis);
		}
	}

	private boolean notEmpty(String s) {
		return s != null && !s.trim().isEmpty();
	}

	private String shortId(String s) {
		if (s == null || s.isEmpty()) {
			return "unknown";
		}
		return s.length() > 8 ? s.substring(0, 8) : s;
	}

	private String safe(String s) {
		return s == null ? "" : s;
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
