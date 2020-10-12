package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteStreak extends GUIHandler {

	private User user;
	@SuppressWarnings("unused")
	private Main plugin;

	public VoteStreak(Main plugin, CommandSender player, User user) {
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
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteStreakName());
		inv.addPlaceholder("player", user.getPlayerName());

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakCurrentDayStreakItem())
				.addPlaceholder("Streak", "" + user.getDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakCurrentWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakCurrentMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {

					}
				});

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakHighestDayStreakItem())
				.addPlaceholder("Streak", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakHighestWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(
				new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteStreakHighestMonthStreakItem())
						.addPlaceholder("Streak", "" + user.getBestMonthVoteStreak())) {

					@Override
					public void onClick(ClickEvent clickEvent) {

					}
				});

		if (GUI.getInstance().getChestVoteStreakBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}

		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add(Config.getInstance().getFormatCommandsVoteStreakTitle());
		msg.addAll(Config.getInstance().getFormatCommandsVoteStreakLines());

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("DailyStreak", "" + user.getDayVoteStreak());
		placeholders.put("WeeklyStreak", "" + user.getWeekVoteStreak());
		placeholders.put("MonthlyStreak", "" + user.getMonthVoteStreak());

		placeholders.put("BestDailyStreak", "" + user.getBestDayVoteStreak());
		placeholders.put("BestWeeklyStreak", "" + user.getBestWeekVoteStreak());
		placeholders.put("BestMonthlyStreak", "" + user.getBestMonthVoteStreak());

		placeholders.put("player", user.getPlayerName());

		msg = ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders);

		return ArrayUtils.getInstance().colorize(msg);
	}
	
	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodStreak().toUpperCase()));
	}

}
