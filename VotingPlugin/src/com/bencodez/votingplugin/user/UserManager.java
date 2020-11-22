package com.bencodez.votingplugin.user;

import java.util.ArrayList;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.votingplugin.VotingPluginMain;

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

	public VotingPluginUser getVotingPluginUser(com.bencodez.advancedcore.api.user.AdvancedCoreUser user) {
		return getVotingPluginUser(java.util.UUID.fromString(user.getUUID()));
	}

	public VotingPluginUser getVotingPluginUser(java.util.UUID uuid) {
		return getVotingPluginUser(new UUID(uuid.toString()));
	}

	public VotingPluginUser getVotingPluginUser(OfflinePlayer player) {
		return getVotingPluginUser(player.getName());
	}

	public VotingPluginUser getVotingPluginUser(Player player) {
		return getVotingPluginUser(player.getName());
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(String playerName) {
		return new VotingPluginUser(plugin,
				com.bencodez.advancedcore.api.user.UserManager.getInstance().getProperName(playerName));
	}

	@SuppressWarnings("deprecation")
	public VotingPluginUser getVotingPluginUser(UUID uuid) {
		return new VotingPluginUser(plugin, uuid);
	}
}
