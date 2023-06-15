package com.bencodez.votingplugin.broadcast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.votingplugin.VotingPluginMain;

public class BroadcastHandler {
	private VotingPluginMain plugin;
	private ScheduledExecutorService timer;
	ConcurrentLinkedQueue<String> votedPlayers = new ConcurrentLinkedQueue<String>();

	public BroadcastHandler(VotingPluginMain plugin, int delay) {
		this.plugin = plugin;
		schedule(delay);
	}

	public void check() {
		int size = votedPlayers.size();
		if (size > 0) {
			for (int i = 0; i < size; i++) {
				votedPlayers.poll();
			}
			String bc = StringParser.getInstance()
					.colorize(plugin.getConfigFile().getFormatAlternateBroadcastBroadcast());
			String playersText = "";
			while (!votedPlayers.isEmpty()) {
				playersText += votedPlayers.remove();
				if (!votedPlayers.isEmpty()) {
					playersText += ", ";
				}
			}
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("numberofplayers", "" + size);
			placeholders.put("players", playersText);
			bc = StringParser.getInstance().replacePlaceHolder(bc, placeholders);
			ArrayList<Player> players = new ArrayList<Player>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				if (!plugin.getVotingPluginUserManager().getVotingPluginUser(p).getDisableBroadcast()) {
					players.add(p);
				}
			}

			MiscUtils.getInstance().broadcast(bc, players);
		}
	}

	public void onVote(String player) {
		votedPlayers.add(player);
	}

	public void schedule(int delay) {
		if (timer != null) {
			timer.shutdownNow();
			timer = Executors.newScheduledThreadPool(1);
		} else {
			timer = Executors.newScheduledThreadPool(1);
		}
		timer.scheduleWithFixedDelay(new Runnable() {

			@Override
			public void run() {
				check();
			}
		}, 60, 60 * delay, TimeUnit.SECONDS);
	}
}
