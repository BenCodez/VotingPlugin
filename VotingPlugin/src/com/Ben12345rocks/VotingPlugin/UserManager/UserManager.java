package com.Ben12345rocks.VotingPlugin.UserManager;

import java.util.ArrayList;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Data.Data;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class UserManager extends
		com.Ben12345rocks.AdvancedCore.UserManager.UserManager {
	/** The instance. */
	static UserManager instance = new UserManager();
	/** The plugin. */
	static Main plugin = Main.plugin;

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

	private ArrayList<User> users;

	public User getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public User getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	public User getVotingPluginUser(String playerName) {
		return getVotingPluginUser(new UUID(PlayerUtils.getInstance().getUUID(
				playerName)));
	}

	@SuppressWarnings("deprecation")
	public User getVotingPluginUser(UUID uuid) {
		for (User user : users) {
			if (user.getUUID().equals(uuid.getUUID())) {
				return user;
			}
		}
		User user = new User(uuid);
		user.setPlayerName();
		users.add(user);
		return user;
	}

	public ArrayList<User> getVotingPluginUsers() {
		return users;
	}

	@SuppressWarnings("deprecation")
	public void loadUsers() {
		users = new ArrayList<User>();
		for (String name : Data.getInstance().getPlayerNames()) {
			User user = new User(name);
			users.add(user);
		}
	}

}
