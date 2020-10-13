package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.UpdatingBInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

public class VoteTotal extends GUIHandler {

	private User user;
	@SuppressWarnings("unused")
	private Main plugin;

	public VoteTotal(Main plugin, CommandSender player, User user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteTotalName());
		inv.addPlaceholder("player", user.getPlayerName());

		for (final TopVoter top : TopVoter.values()) {
			inv.addButton(new UpdatingBInventoryButton(
					new ItemBuilder(GUI.getInstance().getChestVoteTotalItem(top))
							.addPlaceholder("Total", "" + user.getTotal(top)).addPlaceholder("topvoter", top.getName()),
					1000, 1000) {

				@Override
				public void onClick(ClickEvent clickEvent) {

				}

				@Override
				public ItemBuilder onUpdate(Player arg0) {
					return new ItemBuilder(GUI.getInstance().getChestVoteTotalItem(top))
							.addPlaceholder("Total", "" + user.getTotal(top)).addPlaceholder("topvoter", top.getName());
				}
			});
		}

		if (GUI.getInstance().getChestVoteTotalBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		ArrayList<String> msg = new ArrayList<String>();

		int daily = user.getTotal(TopVoter.Daily);
		int weekly = user.getTotal(TopVoter.Weekly);
		int month = user.getTotal(TopVoter.Monthly);
		int all = user.getTotal(TopVoter.AllTime);

		for (String s : Config.getInstance().getFormatCommandsVoteTotal()) {
			String str = StringParser.getInstance().replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%player%", user.getPlayerName());
			msg.add(str);
		}

		return ArrayUtils.getInstance().colorize(msg);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodTotal().toUpperCase()));
	}

}
