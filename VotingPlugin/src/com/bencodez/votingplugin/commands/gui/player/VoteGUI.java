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
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterHandler;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

public class VoteGUI extends GUIHandler {

	private VotingPluginUser user;
	private VotingPluginMain plugin;

	public VoteGUI(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public void onBook(Player player) {
		/*BookWrapper book = new BookWrapper(GUI.getInstance().getBookVoteURLBookGUITitle());

		// url - click to open
		Layout urlLayout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
		urlLayout.replaceTextComponent("[Json]",
				BookUtil.TextBuilder.of(book.colorize("Click me")).onClick(BookUtil.ClickAction.runCommand("/vote url"))
						.onHover(BookUtil.HoverAction.showText("Clicking me opens url GUI")).build());
		book.addLayout(urlLayout);
		book.addLine();

		// next - click to open
		for (VoteSite site : plugin.getVoteSites()) {
			Layout nextLayout = new Layout(new ArrayList<String>(Arrays.asList("[Json]")));
			nextLayout.replaceTextComponent("[Json]",
					BookUtil.TextBuilder.of(site.getDisplayName())
							.onClick(BookUtil.ClickAction.openUrl(site.getVoteURLJsonStrip()))
							.onHover(BookUtil.HoverAction.showText(user.voteCommandNextInfo(site))).build());
			book.addLayout(nextLayout);
		}
		book.addLine();

		// total - click to open

		// top = click to open

		// today - click to open

		// shop - click to open

		book.open(player);*/
	}

	@Override
	public void onChat(CommandSender sender) {
		// not available
	}

	private ItemBuilder getItemSlot(String slot, Player player) {
		ItemBuilder builder = new ItemBuilder(GUI.getInstance().getChestVoteGUISlotSection(slot));

		String[] lore = new String[1];
		lore = ArrayUtils.getInstance().convert(GUI.getInstance().getChestVoteGUISlotLore(slot));

		String str = Config.getInstance().getVoteTopDefault();
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
					lore = TopVoterHandler.getInstance().topVoterMonthly(1);
				} else if (str.equalsIgnoreCase("weekly")) {
					lore = TopVoterHandler.getInstance().topVoterWeekly(1);
				} else if (str.equalsIgnoreCase("daily")) {
					lore = TopVoterHandler.getInstance().topVoterDaily(1);
				} else {
					lore = TopVoterHandler.getInstance().topVoterAllTime(1);
				}
			} else if (slot.equalsIgnoreCase("today")) {
				lore = new VoteToday(plugin, player, user, 1).voteToday();
			} else if (slot.equalsIgnoreCase("help")) {
				lore = new String[] { "Click to view help" };
			}
		}
		builder.setLore(ArrayUtils.getInstance().convert(lore));
		return builder;
	}

	@Override
	public void onChest(Player player) {
		if ((!player.getName().equals(user.getPlayerName())
				&& !player.hasPermission("VotingPlugin.Commands.Vote.GUI.Other")
				&& !player.hasPermission("VotingPlugin.Mod"))
				|| (!player.hasPermission("VotingPlugin.Commands.Vote.GUI")
						&& !player.hasPermission("VotingPlugin.Player"))) {
			player.sendMessage(StringParser.getInstance().colorize(VotingPluginMain.plugin.getOptions().getFormatNoPerms()));
			return;
		}
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteGUIName());
		if (!Config.getInstance().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		inv.addPlaceholder("points", "" + user.getPoints());
		inv.addPlaceholder("player", user.getPlayerName());
		inv.addPlaceholder("top", Config.getInstance().getVoteTopDefault());

		for (String slot : GUI.getInstance().getChestVoteGUISlots()) {
			ItemBuilder builder = getItemSlot(slot, player);

			inv.addButton(new UpdatingBInventoryButton(builder, 1000, 1000) {

				@Override
				public void onClick(ClickEvent event) {

					Player player = event.getWhoClicked();
					String cmd = GUI.getInstance().getChestVoteGUISlotCommand(slot);
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
						}
					}
				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					ItemBuilder item = getItemSlot(slot, player);
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
					item.addPlaceholder("points", "" + user.getPoints());
					item.addPlaceholder("player", user.getPlayerName());
					item.addPlaceholder("top", Config.getInstance().getVoteTopDefault());
					return item;
				}

			});
		}

		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		return null;
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodGUI().toUpperCase()));
	}

}
