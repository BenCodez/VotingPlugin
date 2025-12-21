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
		getNode("VoteCache", server, String.valueOf(num), "VoteID")
				.setValue(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null);
	}

	@Override
	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		getNode("OnlineCache", player, String.valueOf(num), "Name").setValue(voteData.getPlayerName());
		getNode("OnlineCache", player, String.valueOf(num), "Service").setValue(voteData.getService());
		getNode("OnlineCache", player, String.valueOf(num), "UUID").setValue(voteData.getUuid());
		getNode("OnlineCache", player, String.valueOf(num), "Time").setValue(voteData.getTime());
		getNode("OnlineCache", player, String.valueOf(num), "Real").setValue(voteData.isRealVote());
		getNode("OnlineCache", player, String.valueOf(num), "Text").setValue(voteData.getText());
		getNode("OnlineCache", player, String.valueOf(num), "VoteID")
				.setValue(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null);
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

	@Override
	public void removeOnlineVotes(String player) {
		getNode("OnlineCache", player).setValue(null);
	}

	@Override
	public void removeServerVotes(String server) {
		getNode("VoteCache", server).setValue(null);
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
				getNode("VoteCache", server, num).setValue(null);
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
				getNode("VoteCache", server, num).setValue(null);
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
					getNode("OnlineCache", player, num).setValue(null);
				}
			}
		}
	}
}
