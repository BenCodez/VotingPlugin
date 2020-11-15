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
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteBest extends GUIHandler {

	private VotingPluginUser user;
	@SuppressWarnings("unused")
	private VotingPluginMain plugin;

	public VoteBest(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add(Config.getInstance().getFormatCommandsVoteBestTitle());
		msg.addAll(Config.getInstance().getFormatCommandsVoteBestLines());


		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("HighestDailyTotal", "" + user.getHighestDailyTotal());
		placeholders.put("HighestWeeklyTotal", "" + user.getHighestWeeklyTotal());
		placeholders.put("HighestMonthlyTotal", "" + user.getHighestMonthlyTotal());

		placeholders.put("player", user.getPlayerName());

		msg = ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders);

		return ArrayUtils.getInstance().colorize(msg);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteBestName());
		inv.addPlaceholder("player", user.getPlayerName());

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteBestDayBestItem())
				.addPlaceholder("Best", "" + user.getBestDayVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteBestWeekBestItem())
				.addPlaceholder("Best", "" + user.getBestWeekVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.addButton(new BInventoryButton(new ItemBuilder(GUI.getInstance().getChestVoteBestMonthBestItem())
				.addPlaceholder("Best", "" + user.getBestMonthVoteStreak())) {

			@Override
			public void onClick(ClickEvent clickEvent) {

			}
		});

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodBest().toUpperCase()));
	}

}
