package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
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

			ConfigurationSection customization = plugin.getGui().getChestVoteTopCustomization();
			boolean customzationEnabled = customization.getBoolean("Enabled");

			BInventory inv = new BInventory(plugin.getGui().getChestVoteTopName());
			inv.addPlaceholder("topvoter", topVoter);
			if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
				inv.dontClose();
			}

			List<Integer> customizationPlayerSlots = customization.getIntegerList("PlayerSlots");
			Queue<Integer> playerSlots = new ConcurrentLinkedQueue<Integer>();
			playerSlots.addAll(customizationPlayerSlots);

			int pos = 1;
			for (Entry<TopVoterPlayer, Integer> entry : users) {

				ItemBuilder playerItem = new ItemBuilder(Material.PAPER);

				if (plugin.getGui().isChestVoteTopUseSkull()) {
					playerItem = new ItemBuilder(entry.getKey().getPlayerHead());
				} else {
					playerItem = new ItemBuilder(Material.valueOf(plugin.getGui().getChestVoteTopPlayerItemMaterial()));
				}

				playerItem.setLore(new ArrayList<String>());

				BInventoryButton button = new BInventoryButton(playerItem
						.setName(plugin.getGui().getChestVoteTopItemName())
						.addLoreLine(plugin.getGui().getChestVoteTopItemLore()).addPlaceholder("position", "" + pos)
						.addPlaceholder("player", entry.getKey().getPlayerName())
						.addPlaceholder("votes", "" + entry.getValue())) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						if (plugin.getGui().getChestVoteTopOpenMainGUIOnClick()) {
							TopVoterPlayer user = (TopVoterPlayer) getData("User");
							new VoteGUI(plugin, player, user.getUser())
									.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
						} else {
							getInv().forceClose(clickEvent.getPlayer());
						}
					}
				}.addData("player", entry.getKey().getPlayerName()).addData("User", entry.getKey());

				if (customzationEnabled && !playerSlots.isEmpty()) {
					button.setSlot(playerSlots.remove());
				}

				inv.addButton(button);
				pos++;
			}

			TopVoter newTops = top;
			ArrayList<String> tops = plugin.getGui().getChestVoteTopSwitchItemTopVoters();
			if (!tops.isEmpty()) {
				for (String name : tops) {
					newTops.getSwitchItems().add(TopVoter.getTopVoter(name));
				}
			}

			final TopVoter cur = newTops;
			if (customzationEnabled) {
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
			} else {
				inv.addButton(new BInventoryButton(
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
			}

			if (plugin.getGui().getChestVoteTopBackButton()) {
				if (customzationEnabled) {
					inv.addButton(plugin.getCommandLoader().getBackButton(user)
							.setSlot(customization.getInt("BackButtonSlot", 0)));
				} else {
					inv.getPageButtons().add(plugin.getCommandLoader().getBackButton(user).setSlot(1));
				}
			}

			String guiPath = "VoteTop.Customization";
			for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
				inv.addButton(
						new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {

							@Override
							public void onClick(ClickEvent clickEvent) {
								new RewardBuilder(plugin.getGui().getData(),
										"CHEST." + guiPath + ".ExtraItems." + str + ".Rewards").setGiveOffline(false)
										.send(clickEvent.getPlayer());

							}
						});
			}

			if (!customization.getBoolean("RemoveBottomBar")) {
				inv.setPages(true);
			}
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
