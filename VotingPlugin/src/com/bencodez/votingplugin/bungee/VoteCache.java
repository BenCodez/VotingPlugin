package com.bencodez.votingplugin.bungee;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

import lombok.Getter;
import net.md_5.bungee.config.Configuration;
import net.md_5.bungee.config.ConfigurationProvider;
import net.md_5.bungee.config.JsonConfiguration;
import net.md_5.bungee.config.YamlConfiguration;

public class VoteCache {
	private VotingPluginBungee bungee;
	@Getter
	private Configuration data;

	public VoteCache(VotingPluginBungee bungee) {
		this.bungee = bungee;
	}

	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		Configuration section = getData().getSection("TimedVoteCache." + num);
		section.set("Name", voteTimedQueue.getName());
		section.set("Service", voteTimedQueue.getService());
		section.set("Time", voteTimedQueue.getTime());
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		Configuration section = getData().getSection("VoteCache." + server + "." + num);
		section.set("Name", voteData.getPlayerName());
		section.set("Service", voteData.getService());
		section.set("UUID", voteData.getUuid());
		section.set("Time", voteData.getTime());
		section.set("Real", voteData.isRealVote());
		section.set("Text", voteData.getText());
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		Configuration section = getData().getSection("OnlineCache." + player + "." + num);
		section.set("Name", voteData.getPlayerName());
		section.set("Service", voteData.getService());
		section.set("UUID", voteData.getUuid());
		section.set("Time", voteData.getTime());
		section.set("Real", voteData.isRealVote());
		section.set("Text", voteData.getText());
	}

	public void clearData() {
		getData().set("VoteCache", null);
		getData().set("OnlineCache", null);
		getData().set("TimedVoteCache", null);
		save();
	}

	public Collection<String> getOnlineVotes(String name) {
		return getData().getSection("OnlineCache." + name).getKeys();
	}

	public Configuration getOnlineVotes(String name, String num) {
		return getData().getSection("OnlineCache." + name + "." + num);
	}

	public Collection<String> getPlayers() {
		return getData().getSection("OnlineCache").getKeys();
	}

	public Collection<String> getServers() {
		return getData().getSection("VoteCache").getKeys();
	}

	public Collection<String> getServerVotes(String server) {
		return getData().getSection("VoteCache." + server).getKeys();
	}

	public Configuration getServerVotes(String server, String num) {
		return getData().getSection("VoteCache." + server + "." + num);
	}

	public Collection<String> getTimedVoteCache() {
		return getData().getSection("TimedVoteCache").getKeys();
	}

	public Configuration getTimedVoteCache(String key) {
		return getData().getSection("TimedVoteCache." + key);
	}

	public int getVotePartyCache(String server) {
		return getData().getInt("VoteParty.Cache." + server, 0);
	}

	public int getVotePartyCurrentVotes() {
		return getData().getInt("VoteParty.CurrentVotes", 0);
	}

	public int getVotePartyInreaseVotesRequired() {
		return getData().getInt("VoteParty.IncreaseVotes", 0);
	}

	public void load() {
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}

		File yamlFile = new File(bungee.getDataFolder(), "votecache.yml");

		File file = new File(bungee.getDataFolder(), "votecache.json");

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
						.load(new File(bungee.getDataFolder(), "votecache.yml"));
				yamlFile.renameTo(new File(bungee.getDataFolder(), "oldvotecache.yml"));
				ConfigurationProvider.getProvider(YamlConfiguration.class).save(data,
						new File(bungee.getDataFolder(), "oldvotecache.yml"));
			} catch (IOException e) {
				e.printStackTrace();
			}

			save();
		}

		try {
			data = ConfigurationProvider.getProvider(JsonConfiguration.class)
					.load(new File(bungee.getDataFolder(), "votecache.json"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void save() {
		try {
			ConfigurationProvider.getProvider(JsonConfiguration.class).save(data,
					new File(bungee.getDataFolder(), "votecache.json"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void setVotePartyCache(String server, int amount) {
		getData().set("VoteParty.Cache." + server, amount);
	}

	public void setVotePartyCurrentVotes(int amount) {
		getData().set("VoteParty.CurrentVotes", amount);
	}

	public void setVotePartyInreaseVotesRequired(int amount) {
		getData().set("VoteParty.IncreaseVotes", amount);
	}

}
