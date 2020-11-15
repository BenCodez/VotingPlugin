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
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

import xyz.upperlevel.spigot.book.BookUtil;

public class AdminVotePlaceholdersPlayer extends GUIHandler {

	@SuppressWarnings("unused")
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public AdminVotePlaceholdersPlayer(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper("Placeholders");

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "1";
			}
			String value = placeholder.placeholderRequest(user.getOfflinePlayer(), user, identifier);
			String msg = identifier + " = " + value;
			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(msg).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(value)).build());
			book.addLayout(layout);
		}

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {

			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "1";
			}
			String value = placeholder.placeholderRequest(user.getOfflinePlayer(), user, identifier);
			String msg = identifier + " = " + value;

			Layout layout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			layout.replaceTextComponent("[Json]", BookUtil.TextBuilder.of(msg).color(ChatColor.AQUA)
					.onHover(BookUtil.HoverAction.showText(value)).build());
			book.addLayout(layout);
		}
		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("&cPlaceholders:");
		VotingPluginUser placeholderUser = user;
		if (Config.getInstance().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
			placeholderUser = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
		}
		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "1";
			}
			msg.add("%VotingPlugin_" + identifier + "% = "
					+ placeholder.placeholderRequest(user.getOfflinePlayer(), placeholderUser, identifier));
		}

		for (PlaceHolder<VotingPluginUser> placeholder : PlaceHolders.getInstance().getNonPlayerPlaceholders()) {
			String identifier = placeholder.getIdentifier();
			if (identifier.endsWith("_")) {
				identifier += "1";
			}

			msg.add("%VotingPlugin_" + identifier + "% = "
					+ placeholder.placeholderRequest(user.getOfflinePlayer(), placeholderUser, identifier));
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
