package com.Ben12345rocks.VotingPlugin.UserManager;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Objects.UserStorage;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.sql.Column;
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
		if (users.containsKey(uuid.getUUID())) {
			return users.get(uuid.getUUID());
		}
		User user = new User(uuid);
		users.put(uuid.getUUID(), user);
		return user;
	}

	public synchronized ArrayList<String> getAllUUIDs() {
		if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.FLAT)) {
			File folder = new File(plugin.getDataFolder() + File.separator + "Data");
			String[] fileNames = folder.list();
			ArrayList<String> uuids = new ArrayList<String>();
			if (fileNames != null) {
				for (String playerFile : fileNames) {
					if (!playerFile.equals("null") && !playerFile.equals("")) {
						String uuid = playerFile.replace(".yml", "");
						uuids.add(uuid);
					}
				}
			}
			return uuids;
		} else if (AdvancedCoreHook.getInstance().getStorageType().equals(UserStorage.SQLITE)) {
			List<Column> cols = AdvancedCoreHook.getInstance().getSQLiteUserTable().getRows();
			ArrayList<String> uuids = new ArrayList<String>();
			for (Column col : cols) {
				uuids.add((String) col.getValue());
			}
			return uuids;
		}
		return new ArrayList<String>();
	}
}
