package com.bencodez.votingplugin.commands.gui.player;

import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;

import net.md_5.bungee.api.ChatColor;

public class VoteTopVoterPreviousMonths extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;
	private int index = -1;

	public VoteTopVoterPreviousMonths(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, int index) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.index = index;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		return null;
	}

	public void next(int index) {

	}

	@Override
	public void onBook(Player player) {
		// TODO
	}

	@Override
	public void onChat(CommandSender sender) {
	}

	@Override
	public void onChest(Player player) {

		Set<YearMonth> months = plugin.getPreviousMonthsTopVoters().keySet();

		List<YearMonth> yearMonthList = months.stream().sorted(Collections.reverseOrder()).collect(Collectors.toList());
		YearMonth yearMonth = null;

		if (index < 0) {
			if (!yearMonthList.isEmpty()) {
				yearMonth = yearMonthList.get(0);
			}
			player.sendMessage(ChatColor.RED + "Can't open previous months, no data");
			return;
		}
		if (yearMonthList.size() > index) {
			yearMonth = yearMonthList.get(index);
		} else {
			player.sendMessage(ChatColor.RED + "Can't open previous months, no data for requested index");
			return;
		}

		Set<Entry<TopVoterPlayer, Integer>> users = null;
		if (!plugin.getPreviousMonthsTopVoters().containsKey(yearMonth)) {
			player.sendMessage(ChatColor.RED + "Can't open previous months, no data");
			return;
		}
		users = plugin.getPreviousMonthsTopVoters().get(yearMonth).entrySet();

		BInventory inv = new BInventory(plugin.getGui().getChestVoteTopName());
		inv.addPlaceholder("topvoter", yearMonth.toString());
		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		int pos = 1;
		for (Entry<TopVoterPlayer, Integer> entry : users) {
			ItemBuilder playerItem;

			if (plugin.getGui().isChestVoteTopUseSkull()) {
				playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
			} else {
				playerItem = new ItemBuilder(Material.valueOf(plugin.getGui().getChestVoteTopPlayerItemMaterial()));
			}

			playerItem.setLore(new ArrayList<>());

			inv.addButton(new BInventoryButton(playerItem.setName(plugin.getGui().getChestVoteTopItemName())
					.addLoreLine(plugin.getGui().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
					.addPlaceholder("player", entry.getKey().getPlayerName())
					.addPlaceholder("votes", "" + entry.getValue())) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					TopVoterPlayer user = (TopVoterPlayer) getData("User");
					new VoteGUI(plugin, player, user.getUser())
							.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
				}
			}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey()));
			pos++;
		}

		inv.getPageButtons().add(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteTopSwitchItem())
				.addPlaceholder("Top", yearMonth.toString())) {

			@Override
			public void onClick(ClickEvent clickEvent) {
				if (!clickEvent.getClick().equals(ClickType.RIGHT)) {
					new VoteTopVoterPreviousMonths(plugin, player, user, index + 1).open(GUIMethod.CHEST);
				} else {
					new VoteTopVoterPreviousMonths(plugin, player, user, index - 1).open(GUIMethod.CHEST);
				}
			}
		});

		if (plugin.getGui().isChestVoteTopBackButton()) {
			inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(1));
		}

		inv.setPages(true);
		inv.setMaxInvSize(plugin.getGui().getChestVoteTopSize());
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
