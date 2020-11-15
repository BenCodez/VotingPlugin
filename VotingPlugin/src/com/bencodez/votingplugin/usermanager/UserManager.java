package com.bencodez.votingplugin.usermanager;

import java.util.ArrayList;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.User;

public class UserManager {
	/** The instance. */
	static UserManager instance = new UserManager();
	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

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

	public ArrayList<String> getAllUUIDs() {
		return com.bencodez.advancedcore.api.user.UserManager.getInstance().getAllUUIDs();
	}

	public User getVotingPluginUser(com.bencodez.advancedcore.api.user.User user) {
		return getVotingPluginUser(java.util.UUID.fromString(user.getUUID()));
	}

	public User getVotingPluginUser(java.util.UUID uuid) {
		return getVotingPluginUser(new UUID(uuid.toString()));
	}

	public User getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public User getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	@SuppressWarnings("deprecation")
	public User getVotingPluginUser(String playerName) {
		return new User(com.bencodez.advancedcore.api.user.UserManager.getInstance().getProperName(playerName));
	}

	@SuppressWarnings("deprecation")
	public User getVotingPluginUser(UUID uuid) {
		return new User(uuid);
	}
}
