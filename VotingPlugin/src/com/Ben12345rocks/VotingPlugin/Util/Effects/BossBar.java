package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.Bukkit;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarFlag;
import org.bukkit.boss.BarStyle;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

public class BossBar {

	public org.bukkit.boss.BossBar bossBar;

	public BossBar(String msg, String barColor, String barStyle, double progress) {
		bossBar = Bukkit.createBossBar(Utils.getInstance().colorize(msg),
				BarColor.valueOf(barColor), BarStyle.valueOf(barStyle),
				BarFlag.DARKEN_SKY);
		bossBar.setProgress(progress);

	}

	public void send(Player player, int delay) {
		bossBar.addPlayer(player);
		bossBar.setVisible(true);
		Bukkit.getScheduler().runTaskLater(Main.plugin, new Runnable() {

			@Override
			public void run() {
				hide(player);
			}
		}, (long) delay);
	}

	public void hide(Player player) {
		bossBar.setVisible(false);
		bossBar.removeAll();
	}
}