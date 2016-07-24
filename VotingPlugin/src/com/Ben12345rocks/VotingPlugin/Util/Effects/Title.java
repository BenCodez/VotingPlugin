package com.Ben12345rocks.VotingPlugin.Util.Effects;

import org.bukkit.entity.Player;
import org.inventivetalent.title.TitleAPI;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

/**
 * The Class Title.
 */
public class Title {

	/** The instance. */
	static Title instance = new Title();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Title.
	 *
	 * @return single instance of Title
	 */
	public static Title getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new title.
	 */
	private Title() {
	}

	/**
	 * Send title.
	 *
	 * @param player
	 *            the player
	 * @param title
	 *            the title
	 * @param subTitle
	 *            the sub title
	 * @param fadeIn
	 *            the fade in
	 * @param showTime
	 *            the show time
	 * @param fadeOut
	 *            the fade out
	 */
	public void sendTitle(Player player, String title, String subTitle,
			int fadeIn, int showTime, int fadeOut) {

		if (title == null) {
			title = "";
		}
		if (subTitle == null) {
			subTitle = "";
		}

		TitleAPI.sendTimings(player, fadeIn, showTime, fadeOut);

		TitleAPI.sendSubTitle(player, Utils.getInstance()
				.stringToComp(subTitle));

		TitleAPI.sendTitle(player, Utils.getInstance().stringToComp(title));

	}

}
