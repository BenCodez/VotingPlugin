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
 * Admin GUI: Select a service from DB (distinct services), then open
 * AdminVoteLogService.
 */
public class AdminVoteLogServiceSelectAll extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;

	private final int days;
	private final int page;

	private static final int PAGE_SIZE = 45;

	public AdminVoteLogServiceSelectAll(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, int days, int page) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.user = user;
		this.days = days;
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
			String title = "&aVoteLog - Services";
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
					}
				});
				inv.setPages(true);
				inv.setMaxInvSize(54);
				inv.openInventory(player);
				return;
			}

			int needed = (page + 1) * PAGE_SIZE;
			List<String> services = table.getDistinctServices(days, needed);

			int start = page * PAGE_SIZE;
			int end = Math.min(services.size(), start + PAGE_SIZE);

			if (start >= services.size()) {
				inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&eNo services")
						.addLoreLine("&7No services on this page.").addLoreLine("&7Try going back a page.")) {
					@Override
					public void onClick(ClickEvent clickEvent) {
					}
				});
			} else {
				for (int i = start; i < end; i++) {
					String svc = services.get(i);
					inv.addButton(makeServiceButton(svc));
				}
			}

			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private BInventoryButton makeServiceButton(String service) {
		ItemBuilder item = new ItemBuilder(Material.PAPER).setName("&a" + AdminVoteLogHelpers.safe(service))
				.addLoreLine("&7Click to open votelog for this service")
				.addLoreLine(days > 0 ? "&7Window: &fLast " + days + " days" : "&7Window: &fAll time");

		return new BInventoryButton(item) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteLogService(plugin, clickEvent.getPlayer(), table, service, days, 0).open();
			}
		};
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
