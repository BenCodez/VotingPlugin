package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.util.Collection;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.ConfigDataNode;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VelocityJsonVoteCache extends VelocityJSONFile implements IVoteCache {

	public VelocityJsonVoteCache(File file) {
		super(file);
	}

	@Override
	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		setPath(voteTimedQueue.getName(), "TimedVoteCache", String.valueOf(num), "Name");
		setPath(voteTimedQueue.getService(), "TimedVoteCache", String.valueOf(num), "Service");
		setPath(voteTimedQueue.getTime(), "TimedVoteCache", String.valueOf(num), "Time");
	}

	@Override
	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "VoteCache", server, String.valueOf(num), "Name");
		setPath(voteData.getService(), "VoteCache", server, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "VoteCache", server, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "VoteCache", server, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "VoteCache", server, String.valueOf(num), "Real");
		setPath(voteData.getText(), "VoteCache", server, String.valueOf(num), "Text");
		setPath(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null, "VoteCache", server, String.valueOf(num), "VoteID");
	}

	@Override
	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "OnlineCache", player, String.valueOf(num), "Name");
		setPath(voteData.getService(), "OnlineCache", player, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "OnlineCache", player, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "OnlineCache", player, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "OnlineCache", player, String.valueOf(num), "Real");
		setPath(voteData.getText(), "OnlineCache", player, String.valueOf(num), "Text");
		setPath(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null, "OnlineCache", player, String.valueOf(num), "VoteID");
	}

	@Override
	public void clearData() {
		remove("VoteCache");
		remove("OnlineCache");
		remove("TimedVoteCache");
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
		setPath(amount, "VoteParty", "Cache", server);
	}

	@Override
	public void setVotePartyCurrentVotes(int amount) {
		setPath(amount, "VoteParty", "CurrentVotes");
	}

	@Override
	public void setVotePartyInreaseVotesRequired(int amount) {
		setPath(amount, "VoteParty", "IncreaseVotes");
	}

	@Override
	public void removeOnlineVotes(String player) {
		remove("OnlineCache", player);
	}

	@Override
	public void removeServerVotes(String server) {
		remove("VoteCache", server);
	}

	@Override
	public void removeServerVote(String server, String uuid) {
		Collection<String> votes = getServerVotes(server);
		if (votes == null) {
			return;
		}
		// search for vote with uuid and remove it
		for (String num : votes) {
			ConfigDataNode node = getServerVotes(server, num);
			if (node == null) {
				continue;
			}
			DataNode uuidNode = node.get("UUID");
			if (uuidNode == null) {
				continue;
			}
			String nodeUuid = uuidNode.asString();
			if (nodeUuid != null && nodeUuid.equals(uuid)) {
				remove("VoteCache", server, num);
			}
		}

	}

	@Override
	public void removeVote(String server, OfflineBungeeVote vote) {
		Collection<String> votes = getServerVotes(server);
		if (votes == null) {
			return;
		}
		for (String num : votes) {
			ConfigDataNode node = getServerVotes(server, num);
			if (node == null) {
				continue;
			}
			DataNode uuidNode = node.get("UUID");
			DataNode serviceNode = node.get("Service");
			DataNode timeNode = node.get("Time");
			if (uuidNode == null || serviceNode == null || timeNode == null) {
				continue;
			}
			String nodeUuid = uuidNode.asString();
			String nodeService = serviceNode.asString();
			Long nodeTime = timeNode.asLong();

			if (nodeUuid != null && nodeService != null && nodeTime != null && nodeUuid.equals(vote.getUuid())
					&& nodeService.equals(vote.getService()) && nodeTime.longValue() == vote.getTime()) {
				remove("VoteCache", server, num);
			}
		}
	}

	@Override
	public void removeOnlineVote(OfflineBungeeVote vote) {
		Collection<String> players = getPlayers();
		if (players == null) {
			return;
		}
		for (String player : players) {
			Collection<String> onlineVotes = getOnlineVotes(player);
			if (onlineVotes == null) {
				continue;
			}
			for (String num : onlineVotes) {
				ConfigDataNode node = getOnlineVotes(player, num);
				if (node == null) {
					continue;
				}
				DataNode uuidNode = node.get("UUID");
				DataNode serviceNode = node.get("Service");
				DataNode timeNode = node.get("Time");
				if (uuidNode == null || serviceNode == null || timeNode == null) {
					continue;
				}
				String nodeUuid = uuidNode.asString();
				String nodeService = serviceNode.asString();
				Long nodeTime = timeNode.asLong();

				if (nodeUuid != null && nodeService != null && nodeTime != null && nodeUuid.equals(vote.getUuid())
						&& nodeService.equals(vote.getService()) && nodeTime.longValue() == vote.getTime()) {
					remove("OnlineCache", player, num);
				}
			}
		}
	}

	private void setPath(Object value, Object... path) {
		set(path, value);
	}

}