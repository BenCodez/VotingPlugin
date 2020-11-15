package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permission;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;

public class AdminVotePerms extends GUIHandler {

	private VotingPluginMain plugin;
	private int page;
	private String playerName;

	public AdminVotePerms(VotingPluginMain plugin, CommandSender player) {
		super(player);
		this.plugin = plugin;
	}

	public AdminVotePerms(VotingPluginMain plugin, CommandSender player, int page, String playerName) {
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
