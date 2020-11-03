package com.Ben12345rocks.VotingPlugin.Commands.GUI.admin;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Placeholder.PlaceHolder;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.BookWrapper;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.Layout;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Util.PlaceHolders.PlaceHolders;

import xyz.upperlevel.spigot.book.BookUtil;

public class AdminVotePlaceholders extends GUIHandler {

	@SuppressWarnings("unused")
	private Main plugin;

	public AdminVotePlaceholders(Main plugin, CommandSender player) {
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

		for (PlaceHolder<User> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
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

		for (PlaceHolder<User> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
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
		for (PlaceHolder<User> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
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

		for (PlaceHolder<User> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
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
		try {
			open(GUIMethod.BOOK);
		} catch (Exception e) {
			e.printStackTrace();
			open(GUIMethod.CHAT);
		}
	}

}
