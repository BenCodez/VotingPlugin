package com.bencodez.votingplugin.commands.gui.player;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.nms.NMSManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteToday extends GUIHandler {

	private int page;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteToday(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, int page) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.page = page;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		if (page < 1) {
			page = 1;
		}
		ArrayList<String> msg = new ArrayList<String>();
		String[] voteToday = voteToday();

		int maxPage = voteToday.length / pagesize;
		if ((voteToday.length % pagesize) != 0) {
			maxPage++;
		}

		msg.add("&cToday's Votes " + page + "/" + maxPage);
		msg.add("&cPlayerName : VoteSite : Time");
		page--;

		for (int i = pagesize * page; (i < voteToday.length) && (i < ((page + 1) * pagesize)); i++) {
			msg.add(voteToday[i]);
		}

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
		BInventory inv = new BInventory(plugin.getGui().getChestVoteTodayName());
		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			inv.dontClose();
		}
		for (VotingPluginUser user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				String msg = plugin.getGui().getChestVoteTodayLine();
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("VoteSite", voteSite.getDisplayName());
				placeholders.put("Time", timeString);
				msg = StringParser.getInstance().replacePlaceHolder(msg, placeholders);
				ItemBuilder item = null;
				if (plugin.getGui().isChestVoteTodayUseSkull() && !NMSManager.getInstance().isVersion("1.12")) {
					item = new ItemBuilder(user.getPlayerHead());
				} else {
					item = new ItemBuilder(plugin.getGui().getChestVoteTodayPlayerItem());
				}
				item.setName(StringParser.getInstance().replacePlaceHolder(plugin.getGui().getChestVoteTodayIconTitle(),
						"player", user.getPlayerName()));
				item.setLore(msg);
				inv.addButton(inv.getNextSlot(), new BInventoryButton(item) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						VotingPluginUser user = UserManager.getInstance()
								.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
						new VoteGUI(plugin, player, user)
								.open(GUIMethod.valueOf(plugin.getGui().getGuiMethodGUI().toUpperCase()));
					}
				});
			}
		}

		if (plugin.getGui().getChestVoteTodayBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodToday().toUpperCase()));
	}

	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<String>();
		for (VotingPluginUser user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(plugin.getConfigFile().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", voteSite.getKey());
				placeholders.put("time", timeString);
				msg.add(StringParser.getInstance()
						.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteTodayLine(), placeholders));
				// msg.add("&6" + user.getPlayerName() + " : " + voteSite.getKey() + " : " +
				// timeString);
			}
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	public String[] voteTodayGUI() {
		ArrayList<String> msg = new ArrayList<String>();
		for (VotingPluginUser user : plugin.getVoteToday().keySet()) {

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
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", mostRecentSite.getKey());
				placeholders.put("time", timeString);
				msg.add(StringParser.getInstance()
						.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteTodayLine(), placeholders));
			}
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

}
