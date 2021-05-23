package com.bencodez.votingplugin.bungee;

import java.io.File;
import java.io.IOException;

import lombok.Getter;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.YamlConfiguration;

public class NonVotedPlayersCache {
	private VotingPluginBungee bungee;
	@Getter
	private Configuration data;

	public NonVotedPlayersCache(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	public void addPlayer(ProxiedPlayer proxiedPlayer) {
		if (!bungee.getMysql().containsKeyQuery(proxiedPlayer.getUniqueId().toString())) {
			addPlayer(proxiedPlayer.getUniqueId().toString(), proxiedPlayer.getName());
		}
	}

	private void addPlayer(String uuid, String playerName) {
		getData().set("NonVotedPlayers." + playerName + ".UUID", uuid);
		getData().set("NonVotedPlayers." + playerName + ".LastTime", System.currentTimeMillis());
	}

	public void check() {
		for (String player : getData().getSection("NonVotedPlayers").getKeys()) {
			long time = getData().getLong("NonVotedPlayers." + player + ".LastTime", 0);
			if ((System.currentTimeMillis() - time) > 1000 * 60 * 60 * 24 * 5) {
				remove(player);
			} else {
				String uuid = getData().getString("NonVotedPlayers." + player + ".UUID", "");
				if (!uuid.isEmpty()) {
					if (bungee.getMysql().containsKeyQuery(uuid)) {
						remove(player);
					}
				} else {
					remove(player);
				}
			}
		}
		save();
	}

	public void load() {
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}

		File file = new File(bungee.getDataFolder(), "nonvotedplayerscache.yml");

		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		try {
			data = ConfigurationProvider.getProvider(YamlConfiguration.class)
					.load(new File(bungee.getDataFolder(), "nonvotedplayerscache.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String playerExists(String playerName) {
		if (getData().getSection("NonVotedPlayers").getKeys().contains(playerName)) {
			return getData().getString("NonVotedPlayers." + playerName + ".UUID", "");
		}
		return "";
	}

	private void remove(String player) {
		bungee.debug("Removing nonvotedplayer: " + player);
		getData().set("NonVotedPlayers." + player, null);
	}

	public void save() {
		try {
			ConfigurationProvider.getProvider(YamlConfiguration.class).save(data,
					new File(bungee.getDataFolder(), "nonvotedplayerscache.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
