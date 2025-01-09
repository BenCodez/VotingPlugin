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

	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		getNode("TimedVoteCache", "" + num, "Name").setValue(voteTimedQueue.getName());
		getNode("TimedVoteCache", "" + num, "Service").setValue(voteTimedQueue.getService());
		getNode("TimedVoteCache", "" + num, "Time").setValue(voteTimedQueue.getTime());
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {

		getNode("VoteCache", server, "" + num, "Name").setValue(voteData.getPlayerName());
		getNode("VoteCache", server, "" + num, "Service").setValue(voteData.getService());
		getNode("VoteCache", server, "" + num, "UUID").setValue(voteData.getUuid());
		getNode("VoteCache", server, "" + num, "Time").setValue(voteData.getTime());
		getNode("VoteCache", server, "" + num, "Real").setValue(voteData.isRealVote());
		getNode("VoteCache", server, "" + num, "Text").setValue(voteData.getText());
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {

		getNode("OnlineCache", player, "" + num, "Name").setValue(voteData.getPlayerName());
		getNode("OnlineCache", player, "" + num, "Service").setValue(voteData.getService());
		getNode("OnlineCache", player, "" + num, "UUID").setValue(voteData.getUuid());
		getNode("OnlineCache", player, "" + num, "Time").setValue(voteData.getTime());
		getNode("OnlineCache", player, "" + num, "Real").setValue(voteData.isRealVote());
		getNode("OnlineCache", player, "" + num, "Text").setValue(voteData.getText());
	}

	public void clearData() {
		getNode("VoteCache").setValue(null);
		getNode("OnlineCache").setValue(null);
		getNode("TimedVoteCache").setValue(null);
		save();
	}

	public Collection<String> getOnlineVotes(String name) {
		return getKeys(getNode("OnlineCache", name));
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

	public Collection<String> getServerVotes(String server) {
		return getKeys(getNode("VoteCache", server));
	}

	public ConfigurationNode getServerVotes(String server, String num) {
		return getNode("VoteCache", server, num);
	}

	public Collection<String> getTimedVoteCache() {
		return getKeys(getNode("TimedVoteCache"));
	}

	public ConfigurationNode getTimedVoteCache(String name) {
		return getNode("TimedVoteCache", name);
	}

	public int getVotePartyCache(String server) {
		return getNode("VoteParty").getNode("Cache").getNode(server).getInt(0);
	}

	public int getVotePartyCurrentVotes() {
		return getInt(getNode("VoteParty").getNode("CurrentVotes"), 0);
	}

	public int getVotePartyInreaseVotesRequired() {
		return getInt(getNode("VoteParty").getNode("IncreaseVotes"), 0);
	}

	public void setVotePartyCache(String server, int amount) {
		getNode("VoteParty").getNode("Cache").getNode(server).setValue(amount);
	}

	public void setVotePartyCurrentVotes(int amount) {
		getNode("VoteParty").getNode("CurrentVotes").setValue(amount);
	}

	public void setVotePartyInreaseVotesRequired(int amount) {
		getNode("VoteParty").getNode("IncreaseVotes").setValue(amount);
	}

}
