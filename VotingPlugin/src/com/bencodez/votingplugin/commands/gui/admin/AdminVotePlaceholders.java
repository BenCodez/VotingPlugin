package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.bookgui.BookWrapper;
import com.bencodez.advancedcore.api.bookgui.Layout;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class AdminVotePlaceholders extends GUIHandler {

	@SuppressWarnings("unused")
	private VotingPluginMain plugin;

	public AdminVotePlaceholders(VotingPluginMain plugin, CommandSender player) {
		super(player);
		this.plugin = plugin;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper("Placeholders");

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
			String msg = "";
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg = "%VotingPlugin_" + identifier + "% - " + placeholder.getDescription();
			} else {
				msg = "%VotingPlugin_" + identifier + "%";
			}

			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(identifier).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(msg)).build());
			book.addLayout(layout);
		}

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
			String msg = "";
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg = "%VotingPlugin_" + identifier + "% - " + placeholder.getDescription();
			} else {
				msg = "%VotingPlugin_" + identifier + "%";
			}

			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(identifier).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(msg)).build());
			book.addLayout(layout);

		}

		book.open(player);

	}

	@Override
	public void onChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("&cPlaceholders:");
		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg.add("%VotingPlugin_" + identifier + "% - " + placeholder.getDescription());
			} else {
				msg.add("%VotingPlugin_" + identifier + "%");
			}
		}

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg.add("%VotingPlugin_" + identifier + "% - " + placeholder.getDescription());
			} else {
				msg.add("%VotingPlugin_" + identifier + "%");
			}
		}

		sendMessage(msg);
	}

	@Override
	public void onChest(Player player) {

	}

	@Override
	public void open() {
		open(GUIMethod.CHAT);
	}

}
