package com.bencodez.votingplugin.broadcast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;

public class BroadcastHandler {
	private VotingPluginMain plugin;
	private Timer timer;
	ConcurrentLinkedQueue<String> votedPlayers = new ConcurrentLinkedQueue<String>();

	public BroadcastHandler(VotingPluginMain plugin, int delay) {
		this.plugin = plugin;
		schelude(delay);
	}

	public void check() {
		int size = votedPlayers.size();
		if (size > 0) {
			for (int i = 0; i < size; i++) {
				votedPlayers.poll();
			}
			String bc = StringParser.getInstance()
					.colorize(plugin.getConfigFile().getFormatAlternateBroadcastBroadcast());
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("numberofplayers", "" + size);
			bc = StringParser.getInstance().replacePlaceHolder(bc, placeholders);
			ArrayList<Player> players = new ArrayList<Player>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				if (!UserManager.getInstance().getVotingPluginUser(p).getDisableBroadcast()) {
					players.add(p);
				}
			}

			MiscUtils.getInstance().broadcast(bc, players);
		}
	}

	public void onVote(String player) {
		votedPlayers.add(player);
	}

	public void schelude(int delay) {
		if (timer != null) {
			timer.cancel();
			timer = new Timer();
		} else {
			timer = new Timer();
		}
		timer.schedule(new TimerTask() {

			@Override
			public void run() {
				check();
			}
		}, 1000 * 60, 1000 * 60 * delay);
	}
}
