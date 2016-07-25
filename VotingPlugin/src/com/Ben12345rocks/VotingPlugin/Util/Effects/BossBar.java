package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.Bukkit;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarFlag;
import org.bukkit.boss.BarStyle;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;

public class BossBar {

	public org.bukkit.boss.BossBar bossBar;

	public BossBar(String msg, String barColor, String barStyle, double progress) {
		bossBar = Bukkit.createBossBar(msg, BarColor.valueOf(barColor),
				BarStyle.valueOf(barStyle), BarFlag.values());
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
		}, delay);
	}

	public void hide(Player player) {
		bossBar.setVisible(false);
		bossBar.removeAll();
	}
}