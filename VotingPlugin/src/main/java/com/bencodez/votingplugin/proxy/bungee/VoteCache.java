package com.bencodez.votingplugin.proxy.bungee;

import java.io.File;
import java.util.Collection;

import com.bencodez.simpleapi.file.BungeeJsonFile;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.GsonDataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VoteCache extends BungeeJsonFile implements IVoteCache {
	private VotingPluginBungee bungee;

	public VoteCache(VotingPluginBungee bungee) {
		super(new File(bungee.getDataFolder(), "votecache.json"));
		this.bungee = bungee;
		initialize();
	}

	private void initialize() {
		if (!bungee.getDataFolder().exists()) {
			bungee.getDataFolder().mkdir();
		}
		// Reload the JSON file to ensure latest data
		reload();
	}

	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		String path = "TimedVoteCache." + num;
		setString(path + ".Name", voteTimedQueue.getName());
		setString(path + ".Service", voteTimedQueue.getService());
		setLong(path + ".Time", voteTimedQueue.getTime());
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		String path = "VoteCache." + server + "." + num;
		setString(path + ".Name", voteData.getPlayerName());
		setString(path + ".Service", voteData.getService());
		setString(path + ".UUID", voteData.getUuid());
		setLong(path + ".Time", voteData.getTime());
		setBoolean(path + ".Real", voteData.isRealVote());
		setString(path + ".Text", voteData.getText());
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		String path = "OnlineCache." + player + "." + num;
		setString(path + ".Name", voteData.getPlayerName());
		setString(path + ".Service", voteData.getService());
		setString(path + ".UUID", voteData.getUuid());
		setLong(path + ".Time", voteData.getTime());
		setBoolean(path + ".Real", voteData.isRealVote());
		setString(path + ".Text", voteData.getText());
	}

	public void clearData() {
		setString("VoteCache", null);
		setString("OnlineCache", null);
		setString("TimedVoteCache", null);
		save();
	}

	public Collection<String> getOnlineVotes(String name) {
		return getKeys("OnlineCache." + name);
	}

	public GsonDataNode getOnlineVotes(String name, String num) {
		return new GsonDataNode(getNode("OnlineCache." + name + "." + num));
	}

	public Collection<String> getPlayers() {
		return getKeys("OnlineCache");
	}

	public Collection<String> getServers() {
		return getKeys("VoteCache");
	}

	public Collection<String> getServerVotes(String server) {
		return getKeys("VoteCache." + server);
	}

	public GsonDataNode getServerVotes(String server, String num) {
		return new GsonDataNode(getNode("VoteCache." + server + "." + num));
	}

	public Collection<String> getTimedVoteCache() {
		return getKeys("TimedVoteCache");
	}

	public DataNode getTimedVoteCache(String key) {
		return new GsonDataNode(getNode("TimedVoteCache." + key));
	}

	public int getVotePartyCache(String server) {
		return getInt("VoteParty.Cache." + server, 0);
	}

	public int getVotePartyCurrentVotes() {
		return getInt("VoteParty.CurrentVotes", 0);
	}

	public int getVotePartyInreaseVotesRequired() {
		return getInt("VoteParty.IncreaseVotes", 0);
	}

	public void setVotePartyCache(String server, int amount) {
		setInt("VoteParty.Cache." + server, amount);
	}

	public void setVotePartyCurrentVotes(int amount) {
		setInt("VoteParty.CurrentVotes", amount);
	}

	public void setVotePartyInreaseVotesRequired(int amount) {
		setInt("VoteParty.IncreaseVotes", amount);
	}

	@Override
	public void save() {
		super.save();
	}

	@Override
	public void reload() {
		super.reload();
	}

	@Override
	public void removeOnlineVotes(String player) {
		setString("OnlineCache." + player, null);
	}

	@Override
	public void removeServerVotes(String server) {
		setString("VoteCache." + server, null);
	}

	@Override
	public void removeServerVote(String server, String uuid) {
		// search for vote with uuid and remove it
		for (String num : getServerVotes(server)) {
			if (getServerVotes(server, num).get("UUID").asString().equals(uuid)) {
				setString("VoteCache." + server + "." + num, null);
			}
		}
	}

	@Override
	public void removeVote(String server, OfflineBungeeVote vote) {
		for (String num : getServerVotes(server)) {
			GsonDataNode node = getServerVotes(server, num);
			if (node.get("UUID").asString().equals(vote.getUuid())
					&& node.get("Service").asString().equals(vote.getService())
					&& node.get("Time").asLong() == vote.getTime()) {
				setString("VoteCache." + server + "." + num, null);
			}
		}
	}

	@Override
	public void removeOnlineVote(OfflineBungeeVote vote) {
		for (String player : getPlayers()) {
			for (String num : getOnlineVotes(player)) {
				GsonDataNode node = getOnlineVotes(player, num);
				if (node.get("UUID").asString().equals(vote.getUuid())
						&& node.get("Service").asString().equals(vote.getService())
						&& node.get("Time").asLong() == vote.getTime()) {
					setString("OnlineCache." + player + "." + num, null);
				}
			}
		}
	}

}
