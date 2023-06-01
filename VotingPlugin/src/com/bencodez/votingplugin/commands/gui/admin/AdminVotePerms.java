package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;
import java.util.Set;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permission;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVotePerms extends GUIHandler {

	private int page;
	private String playerName;
	private VotingPluginMain plugin;

	public AdminVotePerms(VotingPluginMain plugin, CommandSender player, int page) {
		super(plugin, player);
		this.plugin = plugin;
		this.page = page;
	}

	public AdminVotePerms(VotingPluginMain plugin, CommandSender player, int page, String playerName) {
		super(plugin, player);
		this.plugin = plugin;
		this.page = page;
		this.playerName = playerName;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@SuppressWarnings("deprecation")
	public String[] listPerms(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();

		for (CommandHandler handle : plugin.getVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&6" + handle.getHelpLineCommand("/vote") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &atrue");
				} else {
					msg.add("&6" + handle.getHelpLineCommand("/vote") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &cfalse");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/vote") + " : " + handle.getPerm().split(Pattern.quote("|"))[0]);
			}

		}

		for (CommandHandler handle : plugin.getAdminVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&6" + handle.getHelpLineCommand("/av") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &atrue");
				} else {
					msg.add("&6" + handle.getHelpLineCommand("/av") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &cfalse");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/av") + " : " + handle.getPerm().split(Pattern.quote("|"))[0]);
			}
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			if (sender instanceof Player) {
				Set<String> child = perm.getChildren().keySet();
				if (child.size() > 0) {
					if (sender.hasPermission(perm)) {
						msg.add("&6" + perm.getName() + " : &atrue");
					} else {
						msg.add("&6" + perm.getName() + " : &cfalse");
					}
				} else {
					if (sender.hasPermission(perm)) {
						msg.add("&6" + perm.getName() + " : &atrue");
					} else {
						msg.add("&6" + perm.getName() + " : &cfalse");
					}
				}

			} else {
				msg.add(perm.getName());
			}
		}

		int pagesize = plugin.getConfigFile().getFormatPageSize();

		int maxPage = msg.size() / pagesize;
		if ((msg.size() % pagesize) != 0) {
			maxPage++;
		}

		ArrayList<String> text = new ArrayList<String>();

		text.add("&c&lCommand : Permissions " + page + "/" + maxPage);

		for (int i = pagesize * page - pagesize; i < msg.size() && i < pagesize * page; i++) {
			text.add(msg.get(i));
		}

		text = ArrayUtils.getInstance().colorize(text);

		return ArrayUtils.getInstance().convert(text);
	}

	@SuppressWarnings("deprecation")
	public ArrayList<String> listPerms(CommandSender sender, String player, int page) {
		Player p = Bukkit.getPlayer(player);
		ArrayList<String> text = new ArrayList<String>();
		if (p != null) {

			ArrayList<String> msg = new ArrayList<String>();

			for (CommandHandler handle : plugin.getVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&6" + handle.getHelpLineCommand("/vote") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &atrue");
				} else {
					msg.add("&6" + handle.getHelpLineCommand("/vote") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &cfalse");
				}

			}

			for (CommandHandler handle : plugin.getAdminVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&6" + handle.getHelpLineCommand("/av") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &atrue");
				} else {
					msg.add("&6" + handle.getHelpLineCommand("/av") + " : "
							+ handle.getPerm().split(Pattern.quote("|"))[0] + " : &cfalse");
				}
			}

			for (Permission perm : plugin.getDescription().getPermissions()) {
				Set<String> child = perm.getChildren().keySet();
				if (child.size() > 0) {
					if (p.hasPermission(perm)) {
						msg.add("&6" + perm.getName() + " : &atrue");
					} else {
						msg.add("&6" + perm.getName() + " : &cfalse");
					}
				} else {
					if (p.hasPermission(perm)) {
						msg.add("&6" + perm.getName() + " : &atrue");
					} else {
						msg.add("&6" + perm.getName() + " : &cfalse");
					}
				}
			}

			msg = ArrayUtils.getInstance().colorize(msg);

			int pagesize = plugin.getConfigFile().getFormatPageSize();

			int maxPage = msg.size() / pagesize;
			if ((msg.size() % pagesize) != 0) {
				maxPage++;
			}

			text.add("&c&lCommand : Permissions for " + player + " " + page + "/" + maxPage);

			for (int i = pagesize * page - pagesize; i < msg.size() && i < pagesize * page; i++) {
				text.add(msg.get(i));
			}
		} else {
			text.add("&cPlayer not online: " + player);
		}
		return ArrayUtils.getInstance().colorize(text);

	}

	@SuppressWarnings("deprecation")
	public String[] listPermsDev(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("Command");
		msg.add("  Permissions (seperated by |");
		msg.add("  Help messasge");
		msg.add("  ");

		for (CommandHandler handle : plugin.getVoteCommand()) {
			msg.add(handle.getHelpLineCommand("/vote"));
			msg.add("  " + handle.getPerm());
			msg.add("  " + handle.getHelpMessage());
		}

		for (CommandHandler handle : plugin.getAdminVoteCommand()) {
			msg.add(handle.getHelpLineCommand("/av"));
			msg.add("  " + handle.getPerm());
			msg.add("  " + handle.getHelpMessage());
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			msg.add(perm.getName() + " : " + perm.getDescription());
		}

		msg = ArrayUtils.getInstance().colorize(msg);

		return ArrayUtils.getInstance().convert(msg);
	}

	@Override
	public void onBook(Player player) {
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
		open(GUIMethod.CHAT);
	}

}
