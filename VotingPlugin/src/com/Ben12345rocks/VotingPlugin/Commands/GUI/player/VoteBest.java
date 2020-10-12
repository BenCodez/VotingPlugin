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
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteBest extends GUIHandler {

	private User user;
	@SuppressWarnings("unused")
	private Main plugin;

	public VoteBest(Main plugin, CommandSender player, User user) {
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
