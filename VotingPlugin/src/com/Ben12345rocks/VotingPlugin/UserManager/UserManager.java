package com.Ben12345rocks.VotingPlugin.UserManager;

import java.util.HashMap;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Thread.Thread;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class UserManager extends com.Ben12345rocks.AdvancedCore.UserManager.UserManager {
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

	private HashMap<String, User> users = new HashMap<String, User>();

	public User getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public User getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	public User getVotingPluginUser(String playerName) {
		return getVotingPluginUser(new UUID(PlayerUtils.getInstance().getUUID(playerName)));
	}

	@SuppressWarnings("deprecation")
	public User getVotingPluginUser(UUID uuid) {
		if (users.containsKey(uuid.getUUID())) {
			return users.get(uuid.getUUID());
		}
		User user = new User(uuid);
		user.setPlayerName();
		users.put(uuid.getUUID(), user);
		return user;
	}

	public void load() {
		super.load();
		Thread.getInstance().run(new Runnable() {
			@Override
			public void run() {
				for (String uuid : getAllUUIDs()) {
					getVotingPluginUser(new UUID(uuid));
				}
			}
		});
	}

}
