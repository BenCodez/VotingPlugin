package com.Ben12345rocks.VotingPlugin.Signs;

import java.util.ArrayList;

import org.bukkit.Location;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;

// TODO: Auto-generated Javadoc
/**
 * The Class Signs.
 */
public class Signs {

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

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
	 * @param plugin the plugin
	 */
	public Signs(Main plugin) {
		Signs.plugin = plugin;
	}

	/**
	 * Gets the sign from location.
	 *
	 * @param loc the loc
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
		plugin.signs = new ArrayList<SignHandler>();
		for (String sign : ServerData.getInstance().getSigns()) {
			// plugin.getLogger().info("Loading sign " + sign);
			plugin.signs.add(new SignHandler(sign, ServerData.getInstance()
					.getSignLocation(sign), ServerData.getInstance()
					.getSignData(sign), ServerData.getInstance()
					.getSignPosition(sign)));
		}
	}

	/**
	 * Store signs.
	 */
	public void storeSigns() {
		for (SignHandler sign : plugin.signs) {
			sign.storeSign();
		}
	}

	/**
	 * Update signs.
	 */
	public void updateSigns() {
		for (int i = plugin.signs.size() - 1; i >= 0; i--) {
			if (!plugin.signs.get(i).isValid()) {
				plugin.signs.get(i).removeSign();
				plugin.signs.remove(i);
				plugin.debug("Sign " + plugin.signs.get(i).getSign()
						+ " invalid, removing from data.");
			} else {
				plugin.signs.get(i).updateLines();
				plugin.signs.get(i).updateSign(i * 3);
			}
		}

		plugin.debug("Signs updated");
	}

}
