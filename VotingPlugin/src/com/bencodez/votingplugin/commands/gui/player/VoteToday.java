package com.bencodez.votingplugin.commands.gui.player;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

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
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.usermanager.UserManager;

public class VoteToday extends GUIHandler {

	private User user;
	private VotingPluginMain plugin;
	private int page;

	public VoteToday(VotingPluginMain plugin, CommandSender player, User user, int page) {
		super(player);
		this.plugin = plugin;
		this.user = user;
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
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteTodayName());
		if (!Config.getInstance().isAlwaysCloseInventory()) {
			inv.dontClose();
		}
		for (User user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				String msg = GUI.getInstance().getChestVoteTodayLine();
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("VoteSite", voteSite.getDisplayName());
				placeholders.put("Time", timeString);
				msg = StringParser.getInstance().replacePlaceHolder(msg, placeholders);
				ItemBuilder item = null;
				if (GUI.getInstance().isChestVoteTodayUseSkull() && !NMSManager.getInstance().isVersion("1.12")) {
					item = new ItemBuilder(new ItemStack(Material.PLAYER_HEAD, 1)).setSkullOwner(player);
				} else {
					item = new ItemBuilder(GUI.getInstance().getChestVoteTodayPlayerItem());
				}
				item.setName(StringParser.getInstance().replacePlaceHolder(
						GUI.getInstance().getChestVoteTodayIconTitle(), "player", user.getPlayerName()));
				item.setLore(msg);
				inv.addButton(inv.getNextSlot(), new BInventoryButton(item) {

					@Override
					public void onClick(ClickEvent clickEvent) {
						User user = UserManager.getInstance()
								.getVotingPluginUser(clickEvent.getClickedItem().getItemMeta().getDisplayName());
						new VoteGUI(plugin, player, user)
								.open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodGUI().toUpperCase()));
					}
				});
			}
		}

		if (GUI.getInstance().getChestVoteTodayBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		int pagesize = Config.getInstance().getFormatPageSize();
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

	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<String>();
		for (User user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", voteSite.getKey());
				placeholders.put("time", timeString);
				msg.add(StringParser.getInstance()
						.replacePlaceHolder(Config.getInstance().getFormatCommandsVoteTodayLine(), placeholders));
				// msg.add("&6" + user.getPlayerName() + " : " + voteSite.getKey() + " : " +
				// timeString);
			}
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}
	
	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodToday().toUpperCase()));
	}

}
