package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteStreak extends GUIHandler {

	@SuppressWarnings("unused")
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteStreak(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add(plugin.getConfigFile().getFormatCommandsVoteStreakTitle());
		msg.addAll(plugin.getConfigFile().getFormatCommandsVoteStreakLines());

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
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(plugin.getGui().getChestVoteStreakName());
		inv.addPlaceholder("player", user.getPlayerName());

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakCurrentDayStreakItem())
				.addPlaceholder("Streak", "" + user.getDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakCurrentWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakCurrentMonthStreakItem())
				.addPlaceholder("Streak", "" + user.getMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakHighestDayStreakItem())
				.addPlaceholder("Streak", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakHighestWeekStreakItem())
				.addPlaceholder("Streak", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteStreakHighestMonthStreakItem())
				.addPlaceholder("Streak", "" + user.getBestMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		if (plugin.getGui().getChestVoteStreakBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodStreak().toUpperCase()));
	}

}
