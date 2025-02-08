package com.bencodez.votingplugin.bungee;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.ArrayList;

import lombok.Getter;
import net.md_5.bungee.api.connection.ProxiedPlayer;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.JsonConfiguration;
import net.md_5.bungee.config.YamlConfiguration;

public class NonVotedPlayersCache {
	private VotingPluginBungee bungee;
	@Getter
	private Configuration data;

	public NonVotedPlayersCache(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	public void addPlayer(ProxiedPlayer proxiedPlayer) {
		if (!bungee.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(proxiedPlayer.getUniqueId().toString())) {
			addPlayer(proxiedPlayer.getUniqueId().toString(), proxiedPlayer.getName());
		}
	}

	public void addPlayer(String uuid, String playerName) {
		getData().set("NonVotedPlayers." + playerName + ".UUID", uuid);
		getData().set("NonVotedPlayers." + playerName + ".LastTime", System.currentTimeMillis());
		save();
	}

	public void addPlayerCheck(String uuid, String playerName) {
		if (!bungee.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(uuid)) {
			addPlayer(uuid, playerName);
		}
	}

	public void check() {
		ArrayList<String> toRemove = new ArrayList<String>();
		for (String player : getData().getSection("NonVotedPlayers").getKeys()) {
			long time = getData().getLong("NonVotedPlayers." + player + ".LastTime", 0);
			if ((System.currentTimeMillis() - time) > 1000 * 60 * 60 * 24 * 5) {
				toRemove.add(player);
			} else {
				String uuid = getData().getString("NonVotedPlayers." + player + ".UUID", "");
				if (!uuid.isEmpty()) {
					if (bungee.getVotingPluginProxy().getProxyMySQL().containsKeyQuery(uuid)) {
						toRemove.add(player);
					}
				} else {
					toRemove.add(player);
				}
			}
		}
		for (String player : toRemove) {
			remove(player);
		}
		save();
	}

	public void load() {
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}

		File yamlFile = new File(bungee.getDataFolder(), "nonvotedplayerscache.yml");
		File file = new File(bungee.getDataFolder(), "nonvotedplayerscache.json");

		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		if (yamlFile.exists()) {
			try {
				data = ConfigurationProvider.getProvider(YamlConfiguration.class)
						.load(new File(bungee.getDataFolder(), "nonvotedplayerscache.yml"));
				yamlFile.renameTo(new File(bungee.getDataFolder(), "oldnonvotedplayerscache.yml"));
				ConfigurationProvider.getProvider(YamlConfiguration.class).save(data,
						new File(bungee.getDataFolder(), "oldnonvotedplayerscache.yml"));
			} catch (IOException e) {
				e.printStackTrace();
			}

			save();
		}

		try {
			data = ConfigurationProvider.getProvider(JsonConfiguration.class)
					.load(new File(bungee.getDataFolder(), "nonvotedplayerscache.json"));
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
			ConfigurationProvider.getProvider(JsonConfiguration.class).save(data,
					new File(bungee.getDataFolder(), "nonvotedplayerscache.json"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
