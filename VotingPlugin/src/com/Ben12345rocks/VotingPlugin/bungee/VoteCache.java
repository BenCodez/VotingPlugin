package com.Ben12345rocks.VotingPlugin.bungee;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

import lombok.Getter;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.YamlConfiguration;

public class VoteCache {
	private Bungee bungee;
	@Getter
	private Configuration data;

	public VoteCache(Bungee bungee) {
		this.bungee = bungee;
	}

	public void load() {
		if (!bungee.getDataFolder().exists())
			bungee.getDataFolder().mkdir();

		File file = new File(bungee.getDataFolder(), "votecache.yml");

		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		try {
			data = ConfigurationProvider.getProvider(YamlConfiguration.class)
					.load(new File(bungee.getDataFolder(), "votecache.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void save() {
		try {
			ConfigurationProvider.getProvider(YamlConfiguration.class).save(data,
					new File(bungee.getDataFolder(), "votecache.yml"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public Collection<String> getServers() {
		return getData().getSection("VoteCache").getKeys();
	}

	public Collection<String> getPlayers() {
		return getData().getSection("OnlineCache").getKeys();
	}

	public Configuration getServerVotes(String server, String num) {
		return getData().getSection("VoteCache." + server + "." + num);
	}

	public Configuration getOnlineVotes(String name, String num) {
		return getData().getSection("OnlineCache." + name + "." + num);
	}

	public Collection<String> getServerVotes(String server) {
		return getData().getSection("VoteCache." + server).getKeys();
	}

	public Collection<String> getOnlineVotes(String name) {
		return getData().getSection("OnlineCache." + name).getKeys();
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		Configuration section = getData().getSection("VoteCache." + server + "." + num);
		section.set("Name", voteData.getPlayerName());
		section.set("Service", voteData.getService());
		section.set("UUID", voteData.getUuid());
		section.set("Time", voteData.getTime());
		save();
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		Configuration section = getData().getSection("OnlineCache." + player + "." + num);
		section.set("Name", voteData.getPlayerName());
		section.set("Service", voteData.getService());
		section.set("UUID", voteData.getUuid());
		section.set("Time", voteData.getTime());
		save();
	}

	public void clearData() {
		getData().set("VoteCache", null);
		getData().set("OnlineCache", null);
		save();
	}

}
