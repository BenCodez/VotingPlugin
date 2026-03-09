package com.bencodez.votingplugin.commands.gui.player;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.UUID;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.nms.NMSManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

public class VoteToday extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteToday(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, int page) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.page = page;
	}
	
	@Override
	public void onDialog(Player player) {
		com.bencodez.simpleapi.dialog.MultiActionDialogBuilder dialog = plugin.getDialogService().multiAction(player)
				.placeholder("player", user.getPlayerName())
				.placeholder("page", "" + page)
				.title(plugin.getGui().getChestVoteTodayName())
				.body(String.join("\n", plugin.getConfigFile().getFormatCommandsVoteTodayTitle()))
				.columns(2);

		for (TopVoterPlayer topPlayer : plugin.getVoteToday().keySet()) {
			for (VoteSite voteSite : plugin.getVoteToday().get(topPlayer).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(topPlayer).get(voteSite).format(formatter);

				final UUID uuid = topPlayer.getUuid();

				dialog.placeholder("player", topPlayer.getPlayerName())
						.placeholder("votesite", voteSite.getKey())
						.placeholder("sitename", voteSite.getDisplayName())
						.placeholder("SiteName", voteSite.getDisplayName())
						.placeholder("VoteSite", voteSite.getDisplayName())
						.placeholder("time", timeString)
						.placeholder("Time", timeString)
						.button("&e" + topPlayer.getPlayerName(),
								plugin.getConfigFile().getFormatCommandsVoteTodayLine(),
								payload -> {
									Player clicked = player.getServer().getPlayer(payload.owner());
									if (clicked != null) {
										VotingPluginUser clickedUser = plugin.getVotingPluginUserManager()
												.getVotingPluginUser(uuid);
										new VoteGUI(plugin, clicked, clickedUser)
												.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
									}
								});
			}
		}

		if (plugin.getGui().isChestVoteTodayBackButton()) {
			dialog.button("&eBack", "&7Return to previous menu", payload -> {
				Player clicked = player.getServer().getPlayer(payload.owner());
				if (clicked != null) {
					new VoteGUI(plugin, clicked, user).open();
				}
			});
		}

		dialog.open();
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		if (page < 1) {
			page = 1;
		}
		ArrayList<String> msg = new ArrayList<>();
		String[] voteToday = voteToday();

		int maxPage = voteToday.length / pagesize;
		if ((voteToday.length % pagesize) != 0) {
			maxPage++;
		}

		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("page", "" + page);
		placeholders.put("maxpage", "" + maxPage);

		for (String m : plugin.getConfigFile().getFormatCommandsVoteTodayTitle()) {
			msg.add(m);
		}

		page--;

		for (int i = pagesize * page; (i < voteToday.length) && (i < ((page + 1) * pagesize)); i++) {
			msg.add(voteToday[i]);
		}

		return ArrayUtils.colorize(PlaceholderUtils.replacePlaceHolder(msg, placeholders));
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
		BInventory inv = new BInventory(plugin.getGui().getChestVoteTodayName());
		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			inv.dontClose();
		}

		for (TopVoterPlayer user : plugin.getVoteToday().keySet()) {
			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);

				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", voteSite.getKey());
				placeholders.put("sitename", voteSite.getDisplayName());
				placeholders.put("SiteName", voteSite.getDisplayName());
				placeholders.put("VoteSite", voteSite.getDisplayName());
				placeholders.put("time", timeString);
				placeholders.put("Time", timeString);

				ItemBuilder item;
				if (plugin.getGui().isChestVoteTodayUseSkull() && !NMSManager.getInstance().isVersion("1.12")) {
					item = new ItemBuilder(user.getPlayerHead());
				} else {
					item = new ItemBuilder(plugin.getGui().getChestVoteTodayPlayerItem());
				}

				String title = PlaceholderUtils.replacePlaceHolder(plugin.getGui().getChestVoteTodayIconTitle(),
						placeholders);
				String lore = PlaceholderUtils.replacePlaceHolder(plugin.getGui().getChestVoteTodayLine(),
						placeholders);

				item.setName(title);
				item.setLore(lore);

				final UUID uuid = user.getUuid();
				inv.addButton(inv.getNextSlot(), new BInventoryButton(item) {
					@Override
					public void onClick(ClickEvent clickEvent) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
						new VoteGUI(plugin, player, user)
								.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
					}
				});
			}
		}

		String guiPath = "VoteToday";
		for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
			inv.addButton(
					new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {
						@Override
						public void onClick(ClickEvent clickEvent) {
							new RewardBuilder(plugin.getGui().getData(),
									"CHEST." + guiPath + ".ExtraItems." + str + "."
											+ clickEvent.getButton().getLastRewardsPath(player))
									.setGiveOffline(false).send(clickEvent.getPlayer());
						}
					});
		}

		if (plugin.getGui().isChestVoteTodayBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodToday().toUpperCase()));
	}

	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<>();
		for (TopVoterPlayer user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", voteSite.getKey());
				placeholders.put("sitename", voteSite.getDisplayName());
				placeholders.put("time", timeString);
				msg.add(PlaceholderUtils.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteTodayLine(),
						placeholders));
				// msg.add("&6" + user.getPlayerName() + " : " + voteSite.getKey() + " : " +
				// timeString);
			}
		}
		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	public String[] voteTodayGUI() {
		ArrayList<String> msg = new ArrayList<>();
		for (TopVoterPlayer user : plugin.getVoteToday().keySet()) {

			LocalDateTime mostRecent = null;
			VoteSite mostRecentSite = null;
			for (Entry<VoteSite, LocalDateTime> entry : plugin.getVoteToday().get(user).entrySet()) {
				if (mostRecent == null || entry.getValue().isAfter(mostRecent)) {
					mostRecent = entry.getValue();
					mostRecentSite = entry.getKey();
				}
			}
			if (mostRecent != null) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = mostRecent.format(formatter);
				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", mostRecentSite.getKey());
				placeholders.put("sitename", mostRecentSite.getDisplayName());
				placeholders.put("time", timeString);
				msg.add(PlaceholderUtils.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteTodayLine(),
						placeholders));
			}
		}
		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

}
