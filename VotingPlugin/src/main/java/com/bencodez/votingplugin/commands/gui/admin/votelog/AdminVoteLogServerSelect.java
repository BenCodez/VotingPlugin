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
 * Admin GUI: Select a server (distinct list), then open AdminVoteLogServer.
 */
public class AdminVoteLogServerSelect extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;

	private final int days;
	private final int page;

	private static final int PAGE_SIZE = 45;

	public AdminVoteLogServerSelect(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, int days, int page) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.user = user;
		this.days = Math.max(0, days);
		this.page = Math.max(0, page);

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
			String title = "&aVoteLog - Servers";
			if (days > 0) {
				title += " &7(" + days + "d)";
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

			int needed = (page + 1) * PAGE_SIZE;
			List<String> servers = table.getDistinctServers(days, needed);

			int start = page * PAGE_SIZE;
			int end = Math.min(servers.size(), start + PAGE_SIZE);

			if (start >= servers.size()) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&eNo servers")
						.addLoreLine("&7No servers on this page.").addLoreLine("&7Try going back a page.")) {
					@Override
					public void onClick(ClickEvent clickEvent) {
						// no-op
					}
				});
			} else {
				for (int i = start; i < end; i++) {
					String srv = servers.get(i);
					inv.addButton(makeServerButton(srv));
				}
			}

			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private BInventoryButton makeServerButton(final String server) {
		ItemBuilder item = new ItemBuilder(Material.PAPER).setName("&a" + AdminVoteLogHelpers.safe(server))
				.addLoreLine("&7Click to open votelog for this server")
				.addLoreLine(days > 0 ? "&7Window: &fLast " + days + " days" : "&7Window: &fAll time");

		return new BInventoryButton(item) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteLogServer(plugin, clickEvent.getPlayer(), table, server, days, 0).open();
			}
		};
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
