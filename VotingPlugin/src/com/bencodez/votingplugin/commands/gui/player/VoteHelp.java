package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.votingplugin.VotingPluginMain;

import net.md_5.bungee.api.chat.TextComponent;

public class VoteHelp extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;

	public VoteHelp(VotingPluginMain plugin, CommandSender player, int page) {
		super(player);
		this.plugin = plugin;
		this.page = page;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	public ArrayList<TextComponent> helpText(CommandSender sender) {
		ArrayList<TextComponent> msg = new ArrayList<TextComponent>();
		HashMap<String, TextComponent> unsorted = new HashMap<String, TextComponent>();

		boolean requirePerms = plugin.getConfigFile().getFormatCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.getVoteCommand()) {
			if (requirePerms && cmdHandle.hasPerm(sender)) {
				unsorted.put(cmdHandle.getHelpLineCommand("/vote"),
						cmdHandle.getHelpLine("/vote", plugin.getConfigFile().getFormatCommandsVoteHelpLine()));
			} else if (!requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/vote"), cmdHandle.getHelpLine("/vote"));
			}
		}
		ArrayList<String> unsortedList = new ArrayList<String>();
		unsortedList.addAll(unsorted.keySet());
		Collections.sort(unsortedList, String.CASE_INSENSITIVE_ORDER);
		for (String cmd : unsortedList) {
			msg.add(unsorted.get(cmd));
		}

		return msg;
	}

	@Override
	public void onBook(Player player) {

	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessageJson(voteHelpText(sender));
	}

	@Override
	public void onChest(Player player) {

	}

	@Override
	public void open() {
		open(GUIMethod.CHAT);
	}

	public ArrayList<TextComponent> voteHelpText(CommandSender sender) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<TextComponent> msg = new ArrayList<TextComponent>();
		ArrayList<TextComponent> text = helpText(sender);

		int maxPage = text.size() / pagesize;
		if ((text.size() % pagesize) != 0) {
			maxPage++;
		}

		msg.add(StringParser.getInstance()
				.stringToComp(plugin.getConfigFile().getFormatCommandsVoteHelpTitle() + " " + page + "/" + maxPage));

		for (int i = pagesize * (page - 1); (i < text.size()) && (i < ((page) * pagesize)); i++) {
			msg.add(text.get(i));
		}

		return msg;
	}

}
