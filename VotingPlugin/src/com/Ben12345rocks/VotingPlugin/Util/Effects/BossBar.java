package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.Bukkit;
import org.bukkit.boss.BarColor;
import org.bukkit.boss.BarFlag;
import org.bukkit.boss.BarStyle;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

/**
 * The Class BossBar.
 */
public class BossBar {

	/** The boss bar. */
	public org.bukkit.boss.BossBar bossBar;

	/**
	 * Instantiates a new boss bar.
	 *
	 * @param msg
	 *            the msg
	 * @param barColor
	 *            the bar color
	 * @param barStyle
	 *            the bar style
	 * @param progress
	 *            the progress
	 */
	public BossBar(String msg, String barColor, String barStyle, double progress) {
		bossBar = Bukkit.createBossBar(Utils.getInstance().colorize(msg),
				BarColor.valueOf(barColor), BarStyle.valueOf(barStyle),
				BarFlag.DARKEN_SKY);
		bossBar.setProgress(progress);

	}

	/**
	 * Send.
	 *
	 * @param player
	 *            the player
	 * @param delay
	 *            the delay
	 */
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

	/**
	 * Hide.
	 *
	 * @param player
	 *            the player
	 */
	public void hide(Player player) {
		bossBar.setVisible(false);
		bossBar.removeAll();
	}
}