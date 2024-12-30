package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;

import net.md_5.bungee.api.chat.TextComponent;

public class AdminVoteHelp extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;

	public AdminVoteHelp(VotingPluginMain plugin, CommandSender player, int page) {
		super(plugin, player);
		this.plugin = plugin;
		this.page = page;
	}

	/**
	 * Admin help.
	 *
	 * @param sender the sender
	 * @param page   the page
	 * @return the array list
	 */
	public ArrayList<TextComponent> adminHelp(CommandSender sender, int page) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<TextComponent> msg = new ArrayList<>();
		ArrayList<TextComponent> text = adminHelpText(sender);

		int maxPage = text.size() / pagesize;
		if ((text.size() % pagesize) != 0) {
			maxPage++;
		}

		msg.add(MessageAPI.stringToComp("&6&lVotingPlugin Admin Help " + (page) + "/" + maxPage));
		msg.add(MessageAPI.stringToComp("&6&l() = Needed"));
		msg.add(MessageAPI.stringToComp("&6&lAliases: adminvote, av"));

		for (int i = pagesize * (page - 1); (i < text.size()) && (i < ((page) * pagesize)); i++) {
			msg.add(text.get(i));
		}

		return msg;
	}

	/**
	 * Admin help text.
	 *
	 * @param sender the sender
	 * @return the array list
	 */
	public ArrayList<TextComponent> adminHelpText(CommandSender sender) {
		ArrayList<TextComponent> msg = new ArrayList<>();
		HashMap<String, TextComponent> unsorted = new HashMap<>();

		boolean requirePerms = plugin.getConfigFile().isFormatCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.getAdminVoteCommand()) {
			if (!requirePerms || cmdHandle.hasPerm(sender)) {
				unsorted.put(cmdHandle.getHelpLineCommand("/adminvote"), cmdHandle.getHelpLine("/adminvote"));
			}
		}
		ArrayList<String> unsortedList = new ArrayList<>();
		unsortedList.addAll(unsorted.keySet());
		Collections.sort(unsortedList, String.CASE_INSENSITIVE_ORDER);
		for (String cmd : unsortedList) {
			msg.add(unsorted.get(cmd));
		}

		return msg;
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
		sendMessageJson(adminHelp(sender, page));
	}

	@Override
	public void onChest(Player player) {

	}

	@Override
	public void open() {
		open(GUIMethod.CHAT);
	}

}
