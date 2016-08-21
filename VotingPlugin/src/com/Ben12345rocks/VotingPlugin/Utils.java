package com.Ben12345rocks.VotingPlugin;

import java.util.ArrayList;
import java.util.Set;

import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class Utils.
 */
public class Utils {

	/** The instance. */
	static Utils instance = new Utils();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Utils.
	 *
	 * @return single instance of Utils
	 */
	public static Utils getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new utils.
	 */
	private Utils() {
	}

	/**
	 * Instantiates a new utils.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public Utils(Main plugin) {
		Utils.plugin = plugin;
	}

	/**
	 * Convert set.
	 *
	 * @param set
	 *            the set
	 * @return the array list
	 */
	public ArrayList<User> convertSet(Set<User> set) {
		if (set == null) {
			return null;
		}

		ArrayList<User> list = new ArrayList<User>();
		for (User user : set) {
			list.add(user);
		}
		return list;
	}

}
