package com.Ben12345rocks.VotingPlugin.UserManager;

import java.util.ArrayList;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.VotingPlugin.Objects.VoteUser;

public class UserManager {
	/** The instance. */
	static UserManager instance = new UserManager();

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
		return com.Ben12345rocks.AdvancedCore.UserManager.UserManager.getInstance().getAllUUIDs();
	}

	public VoteUser getVotingPluginUser(java.util.UUID uuid) {
		return getVotingPluginUser(new UUID(uuid.toString()));
	}

	public VoteUser getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public VoteUser getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	@SuppressWarnings("deprecation")
	public VoteUser getVotingPluginUser(String playerName) {
		return new VoteUser(com.Ben12345rocks.AdvancedCore.UserManager.UserManager.getInstance().getProperName(playerName));
	}

	@SuppressWarnings("deprecation")
	public VoteUser getVotingPluginUser(UUID uuid) {
		return new VoteUser(uuid);
	}
}
