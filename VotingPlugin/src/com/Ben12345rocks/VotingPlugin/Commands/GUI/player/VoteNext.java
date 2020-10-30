package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.UpdatingBInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.BookWrapper;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.Layout;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteNext extends GUIHandler {

	private User user;
	private Main plugin;

	public VoteNext(Main plugin, CommandSender player, User user) {
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
