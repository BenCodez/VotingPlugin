package com.Ben12345rocks.VotingPlugin.Commands.GUI.admin;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permission;

import com.Ben12345rocks.AdvancedCore.CommandAPI.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.BookWrapper;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.Layout;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;

import xyz.upperlevel.spigot.book.BookUtil;

public class AdminVotePerms extends GUIHandler {

	private Main plugin;
	private int page;
	private String playerName;

	public AdminVotePerms(Main plugin, CommandSender player) {
		super(player);
		this.plugin = plugin;
	}

	public AdminVotePerms(Main plugin, CommandSender player, int page, String playerName) {
		super(player);
		this.plugin = plugin;
		this.page = page;
		this.playerName = playerName;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper("Placeholders");
		book.addToCurrentPage(BookUtil.TextBuilder.of(book.colorize("&c&lCommand : Permissions")).build());

		boolean includePlayer = playerName != null;
		Player p = null;
		if (includePlayer) {
			p = Bukkit.getPlayer(playerName);
			if (p == null) {
				// not online
				includePlayer = false;
				player.sendMessage(book.colorize("&cPlayer not online"));
			}
		}
		for (CommandHandler handle : plugin.getVoteCommand()) {
			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(handle.getHelpLineCommand("/vote"))
					.color(ChatColor.AQUA).onHover(BookUtil.HoverAction.showText(handle.getHelpMessage())).build());
			book.addLayout(layout);
			if (includePlayer) {
				Layout hasPerm = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
				boolean perm = handle.hasPerm(p);
				ChatColor hasPermColor = ChatColor.RED;
				if (perm) {
					hasPermColor = ChatColor.DARK_GREEN;
				}
				String str = ChatColor.AQUA + handle.getPerm() + " : " + hasPermColor + perm;
				hasPerm.replaceTextComponent("[Json]",
						BookUtil.TextBuilder.of(str).onHover(BookUtil.HoverAction.showText(str)).build());
				book.addLayout(hasPerm);
			}
		}

		for (CommandHandler handle : plugin.getAdminVoteCommand()) {
			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(handle.getHelpLineCommand("/adminvote"))
					.color(ChatColor.AQUA).onHover(BookUtil.HoverAction.showText(handle.getHelpMessage())).build());
			book.addLayout(layout);
			if (includePlayer) {
				Layout hasPerm = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
				boolean perm = handle.hasPerm(p);
				ChatColor hasPermColor = ChatColor.RED;
				if (perm) {
					hasPermColor = ChatColor.DARK_GREEN;
				}
				String str = ChatColor.AQUA + handle.getPerm() + " : " + hasPermColor + perm;
				hasPerm.replaceTextComponent("[Json]",
						BookUtil.TextBuilder.of(str).onHover(BookUtil.HoverAction.showText(str)).build());
				book.addLayout(hasPerm);
			}
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			if (includePlayer) {
				Layout hasPerm = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
				boolean permB = p.hasPermission(perm);
				ChatColor hasPermColor = ChatColor.RED;
				if (permB) {
					hasPermColor = ChatColor.DARK_GREEN;
				}
				String str = ChatColor.AQUA + perm.getName() + " : " + hasPermColor + perm;
				hasPerm.replaceTextComponent("[Json]",
						BookUtil.TextBuilder.of(str).onHover(BookUtil.HoverAction.showText(str)).build());
				book.addLayout(hasPerm);
			} else {
				Layout hasPerm = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));

				String str = ChatColor.AQUA + perm.getName();
				hasPerm.replaceTextComponent("[Json]",
						BookUtil.TextBuilder.of(str).onHover(BookUtil.HoverAction.showText(str)).build());
				book.addLayout(hasPerm);
			}
		}

		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		if (playerName != null) {
			sendMessage(listPerms(sender, playerName, page));
		} else {
			sendMessage(listPerms(sender));
		}
	}

	@Override
	public void onChest(Player player) {

	}

	@Override
	public void open() {
		try {	
			open(GUIMethod.BOOK);
		} catch (Exception e) {
			open(GUIMethod.CHAT);
		}
	}

	public String[] listPerms(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("&c&lCommand : Permissions (seperated by |)");

		for (CommandHandler handle : plugin.getVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&a" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : false");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/vote") + " : " + handle.getPerm());
			}

		}

		for (CommandHandler handle : plugin.getAdminVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&a" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : false");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/av") + " : " + handle.getPerm());
			}
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			if (sender instanceof Player) {
				if (sender.hasPermission(perm)) {
					msg.add("&a" + perm.getName() + " : true");
				} else {
					msg.add("&c" + perm.getName() + " : false");
				}
			} else {
				msg.add(perm.getName());
			}
		}

		msg = ArrayUtils.getInstance().colorize(msg);

		return ArrayUtils.getInstance().convert(msg);
	}

	public ArrayList<String> listPerms(CommandSender sender, String player, int page) {
		Player p = Bukkit.getPlayer(player);
		ArrayList<String> text = new ArrayList<String>();
		if (p != null) {

			ArrayList<String> msg = new ArrayList<String>();

			for (CommandHandler handle : plugin.getVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&a" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : false");
				}

			}

			for (CommandHandler handle : plugin.getAdminVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&a" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : false");
				}
			}

			for (Permission perm : plugin.getDescription().getPermissions()) {
				if (p.hasPermission(perm)) {
					msg.add("&a" + perm.getName() + " : true");
				} else {
					msg.add("&c" + perm.getName() + " : false");
				}
			}

			msg = ArrayUtils.getInstance().colorize(msg);

			int pagesize = Config.getInstance().getFormatPageSize();

			int maxPage = msg.size() / pagesize;
			if ((msg.size() % pagesize) != 0) {
				maxPage++;
			}

			text.add("&c&lCommand : Permissions (seperated by |) " + page + "/" + maxPage);

			for (int i = pagesize * page - pagesize; i < msg.size() && i < pagesize * page; i++) {
				text.add(msg.get(i));
			}
		} else {
			text.add("&cPlayer not online: " + player);
		}
		return ArrayUtils.getInstance().colorize(text);

	}

}
