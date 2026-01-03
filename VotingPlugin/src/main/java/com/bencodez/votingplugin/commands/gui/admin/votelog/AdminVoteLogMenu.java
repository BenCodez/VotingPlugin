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

public class AdminVoteLogMenu extends GUIHandler {

	private final VotingPluginMain plugin;
	private final VoteLogMysqlTable table;
	private VotingPluginUser user;

	private final int days; // window used by selectors/stats (0 = all time)

	// Default to "voting" (Vote received)
	private final VoteLogMysqlTable.VoteLogEvent defaultEvent = VoteLogMysqlTable.VoteLogEvent.VOTE_RECEIVED;

	public AdminVoteLogMenu(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table, int days) {
		this(plugin, sender, table, null, days);
	}

	public AdminVoteLogMenu(VotingPluginMain plugin, CommandSender sender, VoteLogMysqlTable table,
			VotingPluginUser user, int days) {
		super(plugin, sender);
		this.plugin = plugin;
		this.table = table;
		this.user = user;
		if (this.user == null && sender instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) sender);
		}
		this.days = Math.max(0, days);
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
			String title = "&aVoteLog - Admin";
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

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.NAME_TAG).setName("&aVotes by Player")
					.addLoreLine("&7List stored player names")
					.addLoreLine("&7Click a player to view their votelog")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteLogPlayerSelect(plugin, clickEvent.getPlayer(), table, user, days, 0).open();
				}
			});

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.NAME_TAG).setName("&aVotes by Player (Online)")
					.addLoreLine("&7List online players")
					.addLoreLine("&7Click a player to view their votelog")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteLogPlayerSelectOnline(plugin, clickEvent.getPlayer(), table, user, days, 0).open();
				}
			});

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.PAPER).setName("&aVotes by Service")
					.addLoreLine("&7List all services found")
					.addLoreLine("&7Click a service to view votelog")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteLogServiceSelect(plugin, clickEvent.getPlayer(), table, user, days, 0).open();
				}
			});

			// NEW: Votes by Server
			inv.addButton(new BInventoryButton(new ItemBuilder(Material.COMPASS).setName("&aVotes by Server")
					.addLoreLine("&7List all servers found")
					.addLoreLine("&7Click a server to view votelog")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteLogServerSelect(plugin, clickEvent.getPlayer(), table, user, days, 0).open();
				}
			});

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.MAP).setName("&aStats")
					.addLoreLine("&7Overview counts/uniques/top services")
					.addLoreLine(days > 0 ? "&7Window: &fLast " + days + " days" : "&7Window: &fAll time")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					new AdminVoteLogStats(plugin, clickEvent.getPlayer(), table, user, days, 10).open();
				}
			});

			inv.addButton(new BInventoryButton(new ItemBuilder(Material.CLOCK).setName("&aRecent Logs")
					.addLoreLine("&7Recent logs filtered by event")
					.addLoreLine("&7Default: &f" + defaultEvent.name())
					.addLoreLine("&8(Use bottom buttons to switch event)")) {
				@Override
				public void onClick(ClickEvent clickEvent) {
					// Open recent with default event filter
					new AdminVoteLogRecent(plugin, clickEvent.getPlayer(), table, user, 0, days, defaultEvent).open();
				}
			});

			addEventSwitchButtons(inv);

			inv.setPages(true);
			inv.setMaxInvSize(54);
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void addEventSwitchButtons(BInventory inv) {
		inv.getPageButtons().add(new BInventoryButton(new ItemBuilder(Material.LIME_DYE)
				.setName("&aEvent: &fVOTE_RECEIVED").addLoreLine("&7Show recent vote received events")) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteLogRecent(plugin, clickEvent.getPlayer(), table, user, 0, days,
						VoteLogMysqlTable.VoteLogEvent.VOTE_RECEIVED).open();
			}
		}.setSlot(3));

		// More... (open selector)
		inv.getPageButtons().add(new BInventoryButton(
				new ItemBuilder(Material.BOOK).setName("&aEvent: &fMore...").addLoreLine("&7Pick any event filter")) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteLogEventSelect(plugin, clickEvent.getPlayer(), table, user, days).open();
			}
		}.setSlot(4));

		// All events
		inv.getPageButtons().add(new BInventoryButton(new ItemBuilder(Material.GRAY_DYE).setName("&aEvent: &fALL")
				.addLoreLine("&7Show recent logs for all events")) {
			@Override
			public void onClick(ClickEvent clickEvent) {
				new AdminVoteLogRecent(plugin, clickEvent.getPlayer(), table, user, 0, days, null).open();
			}
		}.setSlot(5));
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}
}
