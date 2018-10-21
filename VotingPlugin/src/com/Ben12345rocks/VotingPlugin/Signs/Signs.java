package com.Ben12345rocks.VotingPlugin.Signs;

import java.util.ArrayList;

import org.bukkit.Location;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;

// TODO: Auto-generated Javadoc
/**
 * The Class Signs.
 */
public class Signs {

	/** The instance. */
	static Signs instance = new Signs();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Signs.
	 *
	 * @return single instance of Signs
	 */
	public static Signs getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new signs.
	 */
	private Signs() {
	}

	/**
	 * Instantiates a new signs.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public Signs(Main plugin) {
		Signs.plugin = plugin;
	}

	/**
	 * Gets the sign from location.
	 *
	 * @param loc
	 *            the loc
	 * @return the sign from location
	 */
	public String getSignFromLocation(Location loc) {
		for (String sign : ServerData.getInstance().getSigns()) {
			if (ServerData.getInstance().getSignLocation(sign).equals(loc)) {
				return sign;
			}
		}

		return null;
	}

	/**
	 * Load signs.
	 */
	public void loadSigns() {
		plugin.setSigns(new ArrayList<SignHandler>());
		for (String sign : ServerData.getInstance().getSigns()) {
			// plugin.getLogger().info("Loading sign " + sign);
			plugin.getSigns().add(new SignHandler(sign, ServerData.getInstance().getSignLocation(sign),
					ServerData.getInstance().getSignData(sign), ServerData.getInstance().getSignPosition(sign)));
		}
	}

	/**
	 * Store signs.
	 */
	public void storeSigns() {
		for (SignHandler sign : plugin.getSigns()) {
			sign.storeSign();
		}
	}

	/**
	 * Update signs.
	 */
	public void updateSigns() {
		int time = 0;
		for (int i = plugin.getSigns().size() - 1; i >= 0; i--) {
			if (!plugin.getSigns().get(i).isValid()) {
				plugin.debug("Sign " + i + " invalid, removing from data.");
				plugin.getSigns().get(i).removeSign();
				plugin.getSigns().remove(i);
			} else {
				plugin.getSigns().get(i).updateLines();
				plugin.getSigns().get(i).updateSign(time);
				time += 5;
			}
		}

		plugin.debug("Signs updated");
	}

}
