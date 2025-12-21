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
 * Admin GUI: Recent VoteLog entries (paged).
 *
 * Uses the VoteLogMysqlTable instance passed in from CommandLoader.
 */
public class AdminVoteLogRecent extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;
	private final int page;

	// How many entries per "page" we fetch/display (inventory paging is
	// button-based, but we page like VoteTopVoter does)
	private static final int PAGE_SIZE = 45;

	public AdminVoteLogRecent(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, int page) {
		this(plugin, sender, table, null, page);
	}

	public AdminVoteLogRecent(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, int page) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.user = user;
		if (this.user == null && sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
		this.page = Math.max(0, page);
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
			BInventory inv = new BInventory("&aVoteLog - Recent");
			inv.requirePermission("VotingPlugin.Commands.AdminVote.VoteLog.Recent");

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

			// Fetch enough rows to reach this page (same approach you use in VoteTopVoter)
			int needed = (page + 1) * PAGE_SIZE;
			List<VoteLogMysqlTable.VoteLogEntry> rows = table.getRecent(needed);

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

			if (user != null && plugin.getGui().isChestVoteTopBackButton()) {
				inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(7));
			}

			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private BInventoryButton makeEntryButton(VoteLogMysqlTable.VoteLogEntry entry) {
		Material mat = "CACHED".equalsIgnoreCase(entry.status) ? Material.CHEST : Material.EMERALD;

		String timeStr = formatTime(entry.voteTime);
		String uuidShort = shortUuid(entry.playerUuid);

		// NOTE: This assumes VoteLogEntry has fields: voteTime, playerUuid, playerName,
		// service, status, proxyCachedTotal, event
		// If your VoteLogEntry currently doesn't have 'event' yet, remove the event
		// line.
		ItemBuilder item = new ItemBuilder(mat).setName("&a" + safe(entry.playerName) + " &7(" + uuidShort + ")")
				.addLoreLine("&7Time: &f" + timeStr).addLoreLine("&7Service: &f" + safe(entry.service))
				.addLoreLine("&7Status: &f" + safe(entry.status))
				.addLoreLine("&7Cached Total: &f" + entry.proxyCachedTotal);

		// If you added event to the table/DTO, show it.
		// If not, comment this out.
		try {
			// reflect-safe: won't hard fail compile if field doesn't exist
			// (but if you prefer strict, just do: .addLoreLine("&7Event: &f" +
			// safe(entry.event));
			String event = (String) entry.getClass().getField("event").get(entry);
			item.addLoreLine("&7Event: &f" + safe(event));
		} catch (Throwable ignored) {
			// event not present yet
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

	private String shortUuid(String uuid) {
		if (uuid == null || uuid.isEmpty()) {
			return "unknown";
		}
		return uuid.length() > 8 ? uuid.substring(0, 8) : uuid;
	}

	private String safe(String s) {
		return s == null ? "" : s;
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
