package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.entity.Player;
import org.inventivetalent.chat.ChatAPI;

import com.Ben12345rocks.VotingPlugin.Main;

/**
 * The Class ActionBar.
 */
public class ActionBar {

	/** The instance. */
	static ActionBar instance = new ActionBar();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ActionBar.
	 *
	 * @return single instance of ActionBar
	 */
	public static ActionBar getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new action bar.
	 */
	private ActionBar() {
	}

	/**
	 * Send action bar.
	 *
	 * @param player
	 *            the player
	 * @param msg
	 *            the msg
	 */
	public void sendActionBar(Player player, String msg) {
		if (player != null && msg != null) {
			ChatAPI.sendRawMessage(player, msg, ChatAPI.ACTION_BAR);
		}
	}

}
