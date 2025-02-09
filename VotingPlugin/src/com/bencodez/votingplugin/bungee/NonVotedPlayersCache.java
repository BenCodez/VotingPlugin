package com.bencodez.votingplugin.bungee;

import java.io.File;

import com.bencodez.simpleapi.file.BungeeJsonFile;
import com.velocitypowered.api.proxy.Player;

import lombok.Getter;
import net.md_5.bungee.config.Configuration;

public class NonVotedPlayersCache extends BungeeJsonFile {
	private VotingPluginBungee plugin;
	@Getter
	private Configuration data;

	public NonVotedPlayersCache(VotingPluginBungee plugin) {
		super(new File(plugin.getDataFolder(), "nonvotedplayerscache.json"));
		this.plugin = plugin;
	}

	public void addPlayer(Player player) {
		if (!plugin.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(player.getUniqueId().toString())) {
			addPlayer(player.getUniqueId().toString(), player.getUsername());
		}
	}

	public void addPlayer(String uuid, String playerName) {
		setString("NonVotedPlayers." + playerName + ".UUID", uuid);
		setLong("NonVotedPlayers." + playerName + ".LastTime", System.currentTimeMillis());
		save();
	}

	public void addPlayerCheck(String uuid, String playerName) {
		if (!plugin.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(uuid)) {
			addPlayer(uuid, playerName);
		}
	}

	public void check() {
		for (String player : getKeys("NonVotedPlayers")) {
			long time = getLong("NonVotedPlayers." + player + ".LastTime", 0);
			if ((System.currentTimeMillis() - time) > 1000 * 60 * 60 * 24 * 5) {
				removePlayer(player);
			} else {
				String uuid = getString("NonVotedPlayers." + player + ".UUID", "");
				if (!uuid.isEmpty()) {
					if (plugin.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(uuid)) {
						removePlayer(player);
					}
				} else {
					removePlayer(player);
				}
			}
		}
		save();
	}

	public String playerExists(String playerName) {
		String uuid = getString("NonVotedPlayers." + playerName + ".UUID", "");
		if (!uuid.isEmpty()) {
			return uuid;
		}
		return "";
	}

	private void removePlayer(String player) {
		plugin.debug("Removing nonvotedplayer: " + player);
		remove("NonVotedPlayers." + player);

		save();
	}

}
