package com.Ben12345rocks.VotingPlugin.UserManager;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class UserManager {
	/** The instance. */
	static UserManager instance = new UserManager();
	/** The plugin. */
	static Main plugin = Main.plugin;

	private HashMap<String, User> users = new HashMap<String, User>();

	/**
	 * Gets the single instance of UserManager.
	 *
	 * @return single instance of UserManager
	 */
	public static UserManager getInstance() {
		return instance;
	}

	public UserManager() {
		super();
	}

	public synchronized User getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public synchronized User getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	public synchronized User getVotingPluginUser(String playerName) {
		return getVotingPluginUser(new UUID(PlayerUtils.getInstance().getUUID(playerName)));
	}

	@SuppressWarnings("deprecation")
	public synchronized User getVotingPluginUser(UUID uuid) {
		if (AdvancedCoreHook.getInstance().isPreloadUsers()) {
			if (users.containsKey(uuid.getUUID())) {
				return users.get(uuid.getUUID());
			}
		}
		User user = new User(uuid);
		if (AdvancedCoreHook.getInstance().isPreloadUsers()) {
			users.put(uuid.getUUID(), user);
		}
		return user;
	}

	public synchronized ArrayList<String> getAllUUIDs() {
		return com.Ben12345rocks.AdvancedCore.UserManager.UserManager.getInstance().getAllUUIDs();
	}
}
