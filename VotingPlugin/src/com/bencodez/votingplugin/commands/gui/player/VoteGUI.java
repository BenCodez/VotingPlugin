package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteGUI extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteGUI(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		return null;
	}

	private ItemBuilder getItemSlot(String slot, Player player) {
		ItemBuilder builder = new ItemBuilder(plugin.getGui().getChestVoteGUISlotSection(slot));

		String[] lore = new String[1];
		lore = ArrayUtils.getInstance().convert(plugin.getGui().getChestVoteGUISlotLore(slot));

		String str = plugin.getConfigFile().getVoteTopDefault();
		if (lore.length == 0) {
			if (slot.equalsIgnoreCase("url")) {
				lore = new String[] { "&aClick me" };
			} else if (slot.equalsIgnoreCase("next")) {
				lore = ArrayUtils.getInstance().convert(new VoteNext(plugin, player, user).getChat(player));
			} else if (slot.equalsIgnoreCase("last")) {
				lore = ArrayUtils.getInstance().convert(new VoteLast(plugin, player, user).getChat(player));
			} else if (slot.equalsIgnoreCase("total")) {
				lore = ArrayUtils.getInstance().convert(new VoteTotal(plugin, player, user).getChat(player));
			} else if (slot.equalsIgnoreCase("top")) {

				if (str.equalsIgnoreCase("monthly")) {
					lore = plugin.getTopVoterHandler().topVoterMonthly(1);
				} else if (str.equalsIgnoreCase("weekly")) {
					lore = plugin.getTopVoterHandler().topVoterWeekly(1);
				} else if (str.equalsIgnoreCase("daily")) {
					lore = plugin.getTopVoterHandler().topVoterDaily(1);
				} else {
					lore = plugin.getTopVoterHandler().topVoterAllTime(1);
				}
			} else if (slot.equalsIgnoreCase("today")) {
				String[] today = new VoteToday(plugin, player, user, 1).voteTodayGUI();
				ArrayList<String> list = new ArrayList<String>();
				if (today.length > 0) {
					for (int i = today.length - 1; i < today.length && list.size() < 10 && i >= 0; i--) {
						list.add(today[i]);
					}
				}
				lore = ArrayUtils.getInstance().convert(list);
			} else if (slot.equalsIgnoreCase("help")) {
				lore = new String[] { "Click to view help" };
			}
		}
		builder.setLore(ArrayUtils.getInstance().convert(lore));
		return builder;
	}

	@Override
	public void onBook(Player player) {
		/*
		 * BookWrapper book = new
		 * BookWrapper(plugin.getGui().getBookVoteURLBookGUITitle());
		 *
		 * // url - click to open Layout urlLayout = new Layout(new
		 * ArrayList<String>(Arrays.asList("[Json]")));
		 * urlLayout.replaceTextComponent("[Json]",
		 * BookUtil.TextBuilder.of(book.colorize("Click me")).onClick(BookUtil.
		 * ClickAction.runCommand("/vote url"))
		 * .onHover(BookUtil.HoverAction.showText("Clicking me opens url GUI")).build())
		 * ; book.addLayout(urlLayout); book.addLine();
		 *
		 * // next - click to open for (VoteSite site : plugin.getVoteSites()) { Layout
		 * nextLayout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
		 * nextLayout.replaceTextComponent("[Json]",
		 * BookUtil.TextBuilder.of(site.getDisplayName())
		 * .onClick(BookUtil.ClickAction.openUrl(site.getVoteURLJsonStrip()))
		 * .onHover(BookUtil.HoverAction.showText(user.voteCommandNextInfo(site))).build
		 * ()); book.addLayout(nextLayout); } book.addLine();
		 *
		 * // total - click to open
		 *
		 * // top = click to open
		 *
		 * // today - click to open
		 *
		 * // shop - click to open
		 *
		 * book.open(player);
		 */
	}

	@Override
	public void onChat(CommandSender sender) {
		// not available
	}

	@Override
	public void onChest(Player player) {
		if (this.user == null) {
			user = UserManager.getInstance().getVotingPluginUser(player);
		}
		BInventory inv = new BInventory(plugin.getGui().getChestVoteGUIName());
		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		if (player.getUniqueId().toString().equals(user.getUUID())) {
			inv.requirePermission("VotingPlugin.Commands.Vote.GUI");
		} else {
			inv.requirePermission("VotingPlugin.Commands.Vote.GUI.Other");
		}

		inv.addPlaceholder("points", "" + user.getPoints());
		inv.addPlaceholder("player", user.getPlayerName());
		inv.addPlaceholder("top", plugin.getConfigFile().getVoteTopDefault());

		for (String slot : plugin.getGui().getChestVoteGUISlots()) {
			ItemBuilder builder = getItemSlot(slot, player);

			inv.addButton(new UpdatingBInventoryButton(builder, 1000, 1000) {

				@Override
				public void onClick(ClickEvent event) {

					Player player = event.getWhoClicked();
					String cmd = plugin.getGui().getChestVoteGUISlotCommand(slot);
					if (cmd.equalsIgnoreCase("none")) {
						return;
					} else if (!cmd.equals("")) {
						event.runSync(new Runnable() {

							@Override
							public void run() {
								player.performCommand(cmd);
							}
						});
					} else {
						if (slot.equalsIgnoreCase("url")) {
							new VoteURL(plugin, player, user, true).open();
						} else if (slot.equalsIgnoreCase("next")) {
							new VoteNext(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("last")) {
							new VoteLast(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("total")) {
							new VoteTotal(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("top")) {
							new VoteTopVoter(plugin, player, user, TopVoter.getDefault(), 1).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("today")) {
							new VoteToday(plugin, player, user, 1).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("help")) {
							player.performCommand("vote help");
						} else if (slot.equalsIgnoreCase("shop")) {
							new VoteShop(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("lastmonth")) {
							new VoteTopVoterLastMonth(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("best")) {
							new VoteBest(plugin, player, user).open(GUIMethod.CHEST);
						} else if (slot.equalsIgnoreCase("streak")) {
							new VoteStreak(plugin, player, user).open(GUIMethod.CHEST);
						}
					}

					if (RewardHandler.getInstance().hasRewards(plugin.getGui().getData(),
							plugin.getGui().getChestVoteGUISlotRewardsPath(slot))) {
						RewardHandler.getInstance().giveReward(UserManager.getInstance().getVotingPluginUser(player),
								plugin.getGui().getData(), plugin.getGui().getChestVoteGUISlotRewardsPath(slot),
								new RewardOptions().addPlaceholder("identifier", slot));
					}
				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					ItemBuilder item = getItemSlot(slot, player);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
					item.addPlaceholder("points", "" + user.getPoints());
					item.addPlaceholder("player", user.getPlayerName());
					item.addPlaceholder("top", plugin.getConfigFile().getVoteTopDefault());
					return item;
				}

			});
		}

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
	}

}
