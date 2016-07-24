package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.entity.Player;
import org.inventivetalent.bossbar.BossBarAPI;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

/**
 * The Class BossBar.
 */
public class BossBar {

	/** The instance. */
	static BossBar instance = new BossBar();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of BossBar.
	 *
	 * @return single instance of BossBar
	 */
	public static BossBar getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new boss bar.
	 */
	private BossBar() {
	}

	/**
	 * Send boss bar.
	 *
	 * @param player
	 *            the player
	 * @param msg
	 *            the msg
	 * @param barColor
	 *            the bar color
	 * @param style
	 *            the style
	 * @param progress
	 *            the progress
	 * @param timeout
	 *            the timeout
	 * @param timeoutInterval
	 *            the timeout interval
	 */
	public void sendBossBar(Player player, String msg, String barColor,
			String style, float progress, int timeout, int timeoutInterval) {
		if (player != null && msg != null && barColor != null && style != null) {
			// Create a new BossBar
			@SuppressWarnings("unused")
			org.inventivetalent.bossbar.BossBar bossBar = BossBarAPI.addBar(
					player, // The receiver of the
					// BossBar
					Utils.getInstance().stringToComp(msg), // Displayed
															// message
					BossBarAPI.Color.valueOf(barColor), // Color of the bar
					BossBarAPI.Style.valueOf(style), // Bar style
					progress, // Progress (0.0 - 1.0)
					timeout, // Timeout
					timeoutInterval); // Timeout-interval
		}
	}
}
