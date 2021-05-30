package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.Collection;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;

import com.bencodez.votingplugin.bungee.OfflineBungeeVote;

public class VoteCache extends VelocityYMLFile {

	public VoteCache(File file) {
		super(file);
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		String[] path = new String[] { "VoteCache", server, "" + num };
		try {
			getNode(path, "Name").set(voteData.getPlayerName());
			getNode(path, "Service").set(voteData.getService());
			getNode(path, "UUID").set(voteData.getUuid());
			getNode(path, "Time").set(voteData.getTime());
			getNode(path, "Real").set(voteData.isRealVote());
			getNode(path, "Text").set(voteData.getText());
		} catch (SerializationException e) {
			e.printStackTrace();
		}
		save();
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		String[] path = new String[] { "OnlineCache", player, "" + num };
		try {
			getNode(path, "Name").set(voteData.getPlayerName());
			getNode(path, "Service").set(voteData.getService());
			getNode(path, "UUID").set(voteData.getUuid());
			getNode(path, "Time").set(voteData.getTime());
			getNode(path, "Real").set(voteData.isRealVote());
			getNode(path, "Text").set(voteData.getText());
		} catch (SerializationException e) {
			e.printStackTrace();
		}
		save();
	}

	public void clearData() {
		try {
			getNode("VoteCache").set(null);
			getNode("OnlineCache").set(null);
		} catch (SerializationException e) {
			e.printStackTrace();
		}
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
		return getKeys(getNode("OnlineCache", server));
	}

	public ConfigurationNode getServerVotes(String server, String num) {
		return getNode("VoteCache", server, num);
	}

}
