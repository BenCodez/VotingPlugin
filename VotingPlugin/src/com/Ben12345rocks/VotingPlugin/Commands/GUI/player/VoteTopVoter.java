package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

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
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;

public class VoteTopVoter extends GUIHandler {

	private User user;
	private Main plugin;
	private TopVoter top;
	private int page;

	public VoteTopVoter(Main plugin, CommandSender player, User user, TopVoter top, int page) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.top = top;
		this.page = page;
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
			Set<Entry<User, Integer>> users = null;

			String topVoter = top.getName();
			@SuppressWarnings("unchecked")
			LinkedHashMap<User, Integer> topVotes = (LinkedHashMap<User, Integer>) plugin.getTopVoter(top).clone();
			users = topVotes.entrySet();

			BInventory inv = new BInventory(GUI.getInstance().getChestVoteTopName());
			inv.addPlaceholder("topvoter", topVoter);
			if (!Config.getInstance().isAlwaysCloseInventory()) {
				inv.dontClose();
			}

			int pos = 1;
			for (Entry<User, Integer> entry : users) {
				ItemBuilder playerItem;

				if (GUI.getInstance().isChestVoteTopUseSkull()) {
					playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
				} else {
					playerItem = new ItemBuilder(
							Material.valueOf(GUI.getInstance().getChestVoteTopPlayerItemMaterial()));
				}

				playerItem.setLore(new ArrayList<String>());

				inv.addButton(new BInventoryButton(playerItem.setName(GUI.getInstance().getChestVoteTopItemName())
						.addLoreLine(GUI.getInstance().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
						.addPlaceholder("player", entry.getKey().getPlayerName())
						.addPlaceholder("votes", "" + entry.getValue())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						User user = (User) getData("User");
						new VoteGUI(plugin, player, user)
								.open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodGUI().toUpperCase()));
					}
				}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey()));
				pos++;
			}

			final TopVoter cur = top;
			inv.getPageButtons().add(new BInventoryButton(
					new ItemBuilder(GUI.getInstance().getChestVoteTopSwitchItem()).addPlaceholder("Top", topVoter)) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					if (!clickEvent.getClick().equals(ClickType.RIGHT)) {
						new VoteTopVoter(plugin, player, user, cur.next(), 0).open(GUIMethod.CHEST);
					} else {
						new VoteTopVoter(plugin, player, user, cur.prev(), 0).open(GUIMethod.CHEST);
					}
				}
			});

			if (GUI.getInstance().getChestVoteTopBackButton()) {
				inv.getPageButtons().add(CommandLoader.getInstance().getBackButton(user).setSlot(1));
			}

			inv.setPages(true);
			inv.setMaxInvSize(GUI.getInstance().getChestVoteTopSize());
			inv.openInventory(player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		switch (top) {
			case AllTime:
				return ArrayUtils.getInstance().convert(TopVoterHandler.getInstance().topVoterAllTime(page));
			case Daily:
				return ArrayUtils.getInstance().convert(TopVoterHandler.getInstance().topVoterDaily(page));
			case Monthly:
				return ArrayUtils.getInstance().convert(TopVoterHandler.getInstance().topVoterMonthly(page));
			case Weekly:
				return ArrayUtils.getInstance().convert(TopVoterHandler.getInstance().topVoterWeekly(page));
			default:
				break;
		}
		return new ArrayList<String>();
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodTopVoter().toUpperCase()));
	}

}
