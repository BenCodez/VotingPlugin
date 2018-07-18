package com.Ben12345rocks.VotingPlugin.Signs;

import java.util.ArrayList;

import org.bukkit.Location;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.SignHandler;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class Signs.
 */
public class Signs {

	/** The instance. */
	static Signs instance = new Signs();
	
	private Main main = ServiceLocator.getService(Main.class);

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
		main.signs = new ArrayList<SignHandler>();
		for (String sign : ServerData.getInstance().getSigns()) {
			// main.getLogger().info("Loading sign " + sign);
			main.signs.add(new SignHandler(sign, ServerData.getInstance().getSignLocation(sign),
					ServerData.getInstance().getSignData(sign), ServerData.getInstance().getSignPosition(sign)));
		}
	}

	/**
	 * Store signs.
	 */
	public void storeSigns() {
		for (SignHandler sign : main.signs) {
			sign.storeSign();
		}
	}

	/**
	 * Update signs.
	 */
	public void updateSigns() {
		for (int i = main.signs.size() - 1; i >= 0; i--) {
			if (!main.signs.get(i).isValid()) {
				main.debug("Sign " + i + " invalid, removing from data.");
				main.signs.get(i).removeSign();
				main.signs.remove(i);
			} else {
				main.signs.get(i).updateLines();
				main.signs.get(i).updateSign(i * 3);
			}
		}

		main.debug("Signs updated");
	}

}
