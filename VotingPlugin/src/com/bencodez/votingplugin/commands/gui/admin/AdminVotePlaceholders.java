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
import com.bencodez.advancedcore.api.placeholder.NonPlayerPlaceHolder;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class AdminVotePlaceholders extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVotePlaceholders(VotingPluginMain plugin, CommandSender player) {
		super(plugin, player);
		this.plugin = plugin;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper("Placeholders");

		for (PlaceHolder<VotingPluginUser> placeholder : plugin.getPlaceholders().getPlaceholders()) {
			String msg = "";
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg = "VotingPlugin_" + identifier + " - " + placeholder.getDescription();
			} else {
				msg = "VotingPlugin_" + identifier + "";
			}

			Layout layout = new Layout(new ArrayList<>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(identifier).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(msg)).build());
			book.addLayout(layout);
		}

		for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : plugin.getPlaceholders().getNonPlayerPlaceholders()) {
			String msg = "";
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg = "VotingPlugin_" + identifier + " - " + placeholder.getDescription();
			} else {
				msg = "VotingPlugin_" + identifier + "";
			}

			Layout layout = new Layout(new ArrayList<>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(identifier).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(msg)).build());
			book.addLayout(layout);

		}

		book.open(player);

	}

	@Override
	public void onChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<>();
		msg.add("&cPlaceholders:");
		for (PlaceHolder<VotingPluginUser> placeholder : plugin.getPlaceholders().getPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg.add("VotingPlugin_" + identifier + " - " + placeholder.getDescription());
			} else {
				msg.add("VotingPlugin_" + identifier + "");
			}
		}

		for (NonPlayerPlaceHolder<VotingPluginUser> placeholder : plugin.getPlaceholders().getNonPlayerPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "#";
			}
			if (placeholder.hasDescription()) {
				msg.add("VotingPlugin_" + identifier + " - " + placeholder.getDescription());
			} else {
				msg.add("VotingPlugin_" + identifier + "");
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
