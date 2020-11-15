package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.bookgui.BookWrapper;
import com.bencodez.advancedcore.api.bookgui.Layout;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteNext extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;

	public VoteNext(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper(GUI.getInstance().getBookVoteURLBookGUITitle());

		// add colors/config options
		for (VoteSite site : plugin.getVoteSites()) {
			Layout nextLayout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			nextLayout.replaceTextComponent("[Json]",
					BookUtil.TextBuilder.of(book.colorize(site.getDisplayName()))
							.onClick(BookUtil.ClickAction.openUrl(site.getVoteURLJsonStrip()))
							.onHover(BookUtil.HoverAction.showText(user.voteCommandNextInfo(site))).build());
			book.addLayout(nextLayout);
		}
		book.addLine();

		book.open(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();

		String playerName = user.getPlayerName();

		msg.add(StringParser.getInstance().colorize(StringParser.getInstance()
				.replaceIgnoreCase(Config.getInstance().getFormatCommandsVoteNextTitle(), "%player%", playerName)));

		for (VoteSite voteSite : plugin.getVoteSites()) {

			String msgLine = Config.getInstance().getFormatCommandsVoteNextLayout();

			msgLine = StringParser.getInstance().replaceIgnoreCase(msgLine, "%info%",
					user.voteCommandNextInfo(voteSite));

			msgLine = StringParser.getInstance().replaceIgnoreCase(msgLine, "%SiteName%", voteSite.getDisplayName());
			msg.add(StringParser.getInstance().colorize(msgLine));

		}
		return msg;
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteNextName());
		inv.addPlaceholder("player", user.getPlayerName());
		for (VoteSite site : plugin.getVoteSites()) {
			inv.addButton(inv.getNextSlot(), new UpdatingBInventoryButton(site.getItem().setName(site.getDisplayName())
					.setLore(user.voteCommandNextInfo(site)).setAmountNone(1), 1000, 1000) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					return site.getItem().setName(site.getDisplayName()).setLore(user.voteCommandNextInfo(site))
							.setAmountNone(1).addPlaceholder("player", user.getPlayerName());
				}
			});
		}

		if (GUI.getInstance().getChestVoteNextBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}
		inv.openInventory(player);
	}
	
	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodNext().toUpperCase()));
	}

}
