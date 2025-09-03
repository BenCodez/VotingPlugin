package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.util.Collection;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.ConfigDataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VoteCache extends VelocityJSONFile implements IVoteCache {

	public VoteCache(File file) {
		super(file);
	}

	@Override
	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		getNode("TimedVoteCache", String.valueOf(num), "Name").setValue(voteTimedQueue.getName());
		getNode("TimedVoteCache", String.valueOf(num), "Service").setValue(voteTimedQueue.getService());
		getNode("TimedVoteCache", String.valueOf(num), "Time").setValue(voteTimedQueue.getTime());
	}

	@Override
	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		getNode("VoteCache", server, String.valueOf(num), "Name").setValue(voteData.getPlayerName());
		getNode("VoteCache", server, String.valueOf(num), "Service").setValue(voteData.getService());
		getNode("VoteCache", server, String.valueOf(num), "UUID").setValue(voteData.getUuid());
		getNode("VoteCache", server, String.valueOf(num), "Time").setValue(voteData.getTime());
		getNode("VoteCache", server, String.valueOf(num), "Real").setValue(voteData.isRealVote());
		getNode("VoteCache", server, String.valueOf(num), "Text").setValue(voteData.getText());
	}

	@Override
	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		getNode("OnlineCache", player, String.valueOf(num), "Name").setValue(voteData.getPlayerName());
		getNode("OnlineCache", player, String.valueOf(num), "Service").setValue(voteData.getService());
		getNode("OnlineCache", player, String.valueOf(num), "UUID").setValue(voteData.getUuid());
		getNode("OnlineCache", player, String.valueOf(num), "Time").setValue(voteData.getTime());
		getNode("OnlineCache", player, String.valueOf(num), "Real").setValue(voteData.isRealVote());
		getNode("OnlineCache", player, String.valueOf(num), "Text").setValue(voteData.getText());
	}

	@Override
	public void clearData() {
		getNode("VoteCache").setValue(null);
		getNode("OnlineCache").setValue(null);
		getNode("TimedVoteCache").setValue(null);
		save();
	}

	@Override
	public Collection<String> getOnlineVotes(String name) {
		return getKeys(getNode("OnlineCache", name));
	}

	@Override
	public ConfigDataNode getOnlineVotes(String name, String num) {
		return new ConfigDataNode(getNode("OnlineCache", name, num));
	}

	@Override
	public Collection<String> getPlayers() {
		return getKeys(getNode("OnlineCache"));
	}

	@Override
	public Collection<String> getServers() {
		return getKeys(getNode("VoteCache"));
	}

	@Override
	public Collection<String> getServerVotes(String server) {
		return getKeys(getNode("VoteCache", server));
	}

	@Override
	public ConfigDataNode getServerVotes(String server, String num) {
		return new ConfigDataNode(getNode("VoteCache", server, num));
	}

	@Override
	public Collection<String> getTimedVoteCache() {
		return getKeys(getNode("TimedVoteCache"));
	}

	@Override
	public ConfigDataNode getTimedVoteCache(String key) {
		return new ConfigDataNode(getNode("TimedVoteCache", key));
	}

	@Override
	public int getVotePartyCache(String server) {
		return getNode("VoteParty", "Cache", server).getInt(0);
	}

	@Override
	public int getVotePartyCurrentVotes() {
		return getInt(getNode("VoteParty", "CurrentVotes"), 0);
	}

	@Override
	public int getVotePartyInreaseVotesRequired() {
		return getInt(getNode("VoteParty", "IncreaseVotes"), 0);
	}

	@Override
	public void setVotePartyCache(String server, int amount) {
		getNode("VoteParty", "Cache", server).setValue(amount);
	}

	@Override
	public void setVotePartyCurrentVotes(int amount) {
		getNode("VoteParty", "CurrentVotes").setValue(amount);
	}

	@Override
	public void setVotePartyInreaseVotesRequired(int amount) {
		getNode("VoteParty", "IncreaseVotes").setValue(amount);
	}
}
