package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.Set;

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
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteTopVoter extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;
	private TopVoter top;
	private VotingPluginUser user;

	public VoteTopVoter(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, TopVoter top, int page) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.top = top;
		this.page = page;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		switch (top) {
		case AllTime:
			return ArrayUtils.getInstance().convert(plugin.getTopVoterHandler().topVoterAllTime(page));
		case Daily:
			return ArrayUtils.getInstance().convert(plugin.getTopVoterHandler().topVoterDaily(page));
		case Monthly:
			return ArrayUtils.getInstance().convert(plugin.getTopVoterHandler().topVoterMonthly(page));
		case Weekly:
			return ArrayUtils.getInstance().convert(plugin.getTopVoterHandler().topVoterWeekly(page));
		default:
			break;
		}
		return new ArrayList<String>();
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
		try {
			if (top == null) {
				top = TopVoter.getDefault();
			}
			Set<Entry<TopVoterPlayer, Integer>> users = null;

			String topVoter = top.getName();
			@SuppressWarnings("unchecked")
			LinkedHashMap<TopVoterPlayer, Integer> topVotes = (LinkedHashMap<TopVoterPlayer, Integer>) plugin
					.getTopVoter(top).clone();
			users = topVotes.entrySet();

			BInventory inv = new BInventory(plugin.getGui().getChestVoteTopName());
			inv.addPlaceholder("topvoter", topVoter);
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

				playerItem.setLore(new ArrayList<String>());

				inv.addButton(new BInventoryButton(playerItem.setName(plugin.getGui().getChestVoteTopItemName())
						.addLoreLine(plugin.getGui().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
						.addPlaceholder("player", entry.getKey().getPlayerName())
						.addPlaceholder("votes", "" + entry.getValue())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						if (plugin.getGui().getChestVoteTopOpenMainGUIOnClick()) {
							VotingPluginUser user = (VotingPluginUser) getData("User");
							new VoteGUI(plugin, player, user)
									.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
						} else {
							getInv().forceClose(clickEvent.getPlayer());
						}
					}
				}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey()));
				pos++;
			}

			final TopVoter cur = top;
			inv.getPageButtons().add(new BInventoryButton(
					new ItemBuilder(plugin.getGui().getChestVoteTopSwitchItem()).addPlaceholder("Top", topVoter)) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					if (!clickEvent.getClick().equals(ClickType.RIGHT)) {
						new VoteTopVoter(plugin, player, user, cur.next(), 0).open(GUIMethod.CHEST);
					} else {
						new VoteTopVoter(plugin, player, user, cur.prev(), 0).open(GUIMethod.CHEST);
					}
				}
			});

			if (plugin.getGui().getChestVoteTopBackButton()) {
				inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(1));
			}

			inv.setPages(true);
			inv.setMaxInvSize(plugin.getGui().getChestVoteTopSize());
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodTopVoter().toUpperCase()));
	}

}
