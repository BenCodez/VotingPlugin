package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.Collection;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.bungee.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

import ninja.leaping.configurate.ConfigurationNode;

public class VoteCache extends VelocityJSONFile {

	public VoteCache(File file) {
		super(file);
	}

	public int getVotePartyCurrentVotes() {
		return getInt(getNode("VoteParty").getNode("CurrentVotes"), 0);
	}

	public int getVotePartyInreaseVotesRequired() {
		return getInt(getNode("VoteParty").getNode("IncreaseVotes"), 0);
	}

	public void setVotePartyCurrentVotes(int amount) {
		getNode("VoteParty").getNode("CurrentVotes").setValue(amount);
	}

	public void setVotePartyInreaseVotesRequired(int amount) {
		getNode("VoteParty").getNode("IncreaseVotes").setValue(amount);
	}

	public void setVotePartyCache(String server, int amount) {
		getNode("VoteParty").getNode("Cache").getNode(server).setValue(amount);
	}

	public int getVotePartyCache(String server) {
		return getNode("VoteParty").getNode("Cache").getNode(server).getInt(0);
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		ConfigurationNode path = getNode("VoteCache", server, "" + num);

		getNode(path, "Name").setValue(voteData.getPlayerName());
		getNode(path, "Service").setValue(voteData.getService());
		getNode(path, "UUID").setValue(voteData.getUuid());
		getNode(path, "Time").setValue(voteData.getTime());
		getNode(path, "Real").setValue(voteData.isRealVote());
		getNode(path, "Text").setValue(voteData.getText());
	}

	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		ConfigurationNode path = getNode("TimedVoteCache", "" + num);
		getNode(path, "Name").setValue(voteTimedQueue.getName());
		getNode(path, "Service").setValue(voteTimedQueue.getService());
		getNode(path, "Time").setValue(voteTimedQueue.getTime());
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		ConfigurationNode path = getNode("OnlineCache", player, "" + num);

		getNode(path, "Name").setValue(voteData.getPlayerName());
		getNode(path, "Service").setValue(voteData.getService());
		getNode(path, "UUID").setValue(voteData.getUuid());
		getNode(path, "Time").setValue(voteData.getTime());
		getNode(path, "Real").setValue(voteData.isRealVote());
		getNode(path, "Text").setValue(voteData.getText());
	}

	public void clearData() {
		getNode("VoteCache").setValue(null);
		getNode("OnlineCache").setValue(null);
		save();
	}

	public Collection<String> getOnlineVotes(String name) {
		return getKeys(getNode("OnlineCache", name));
	}

	public ConfigurationNode getTimedVoteCache(String name) {
		return getNode("TimedVoteCache", name);
	}

	public ConfigurationNode getOnlineVotes(String name, String num) {
		return getNode("OnlineCache", name, num);
	}

	public Collection<String> getPlayers() {
		return getKeys(getNode("OnlineCache"));
	}

	public Collection<String> getServers() {
		return getKeys(getNode("VoteCache"));
	}

	public Collection<String> getTimedVoteCache() {
		return getKeys(getNode("TimedVoteCache"));
	}

	public Collection<String> getServerVotes(String server) {
		return getKeys(getNode("OnlineCache", server));
	}

	public ConfigurationNode getServerVotes(String server, String num) {
		return getNode("VoteCache", server, num);
	}

}
