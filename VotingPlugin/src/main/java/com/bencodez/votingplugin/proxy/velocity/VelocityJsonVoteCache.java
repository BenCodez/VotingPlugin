package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.Locale;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.ConfigDataNode;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;

import org.spongepowered.configurate.gson.GsonConfigurationLoader;

/**
 * JSON-based vote cache implementation for Velocity proxy.
 */
public class VelocityJsonVoteCache extends VelocityJSONFile implements IVoteCache {

	/**
	 * Constructs a new Velocity JSON vote cache.
	 * @param file the file to store cache data
	 */
	public VelocityJsonVoteCache(File file) {
		super(file);
	}

	@Override
	public java.nio.file.Path getStoragePath() {
		return getPath();
	}

	@Override
	public synchronized void saveDurably() throws IOException {
		Path target = getStoragePath().toAbsolutePath().normalize();
		Path parent = target.getParent();
		if (parent == null) throw new IOException("Vote cache has no parent directory");
		Files.createDirectories(parent);
		Path staged = Files.createTempFile(parent, target.getFileName().toString(), ".tmp");
		try {
			GsonConfigurationLoader.builder().path(staged).build().save(getConf());
			DurableFiles.publishStagedFile(staged, target);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	@Override
	public void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
		setPath(voteTimedQueue.getName(), "TimedVoteCache", String.valueOf(num), "Name");
		setPath(voteTimedQueue.getService(), "TimedVoteCache", String.valueOf(num), "Service");
		setPath(voteTimedQueue.getTime(), "TimedVoteCache", String.valueOf(num), "Time");
		setPath(voteTimedQueue.getVoteId() == null ? null : voteTimedQueue.getVoteId().toString(), "TimedVoteCache",
				String.valueOf(num), "VoteId");
		setPath(voteTimedQueue.getUuid(), "TimedVoteCache", String.valueOf(num), "UUID");
		setPath(voteTimedQueue.isProxyBroadcastHandled(), "TimedVoteCache", String.valueOf(num),
				"ProxyBroadcastHandled");
		setPath(voteTimedQueue.getTotals(), "TimedVoteCache", String.valueOf(num), "Totals");
		setPath(voteTimedQueue.isProcessed(), "TimedVoteCache", String.valueOf(num), "Processed");
		setPath(voteTimedQueue.encodeBroadcastTargets(), "TimedVoteCache", String.valueOf(num), "BroadcastTargets");
		setPath(voteTimedQueue.encodeBroadcastForwardedServers(), "TimedVoteCache", String.valueOf(num),
				"BroadcastForwardedServers");
		setPath(voteTimedQueue.encodeHttpBroadcastDeliveryIds(), "TimedVoteCache", String.valueOf(num),
				"HttpBroadcastDeliveryIds");
	}

	@Override
	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "VoteCache", server, String.valueOf(num), "Name");
		setPath(voteData.getService(), "VoteCache", server, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "VoteCache", server, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "VoteCache", server, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "VoteCache", server, String.valueOf(num), "Real");
		setPath(voteData.getText(), "VoteCache", server, String.valueOf(num), "Text");
		setPath(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null, "VoteCache", server, String.valueOf(num), "VoteId");
		setPath(voteData.isBroadcastForwarded(), "VoteCache", server, String.valueOf(num), "BroadcastForwarded");
		setPath(voteData.isProxyBroadcastHandled(), "VoteCache", server, String.valueOf(num),
				"ProxyBroadcastHandled");
		setPath(voteData.encodeBroadcastTargets(), "VoteCache", server, String.valueOf(num), "BroadcastTargets");
		setPath(voteData.encodeBroadcastForwardedServers(), "VoteCache", server, String.valueOf(num),
				"BroadcastForwardedServers");
		setPath(voteData.isRewardDelivered(), "VoteCache", server, String.valueOf(num), "RewardDelivered");
		setPath(voteData.encodeHttpDeliveryIds(), "VoteCache", server, String.valueOf(num), "HttpDeliveryIds");
		setPath(voteData.encodeHttpBroadcastDeliveryIds(), "VoteCache", server, String.valueOf(num), "HttpBroadcastDeliveryIds");
	}

	@Override
	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "OnlineCache", player, String.valueOf(num), "Name");
		setPath(voteData.getService(), "OnlineCache", player, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "OnlineCache", player, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "OnlineCache", player, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "OnlineCache", player, String.valueOf(num), "Real");
		setPath(voteData.getText(), "OnlineCache", player, String.valueOf(num), "Text");
		setPath(voteData.getVoteId() != null ? voteData.getVoteId().toString() : null, "OnlineCache", player, String.valueOf(num), "VoteId");
		setPath(voteData.isBroadcastForwarded(), "OnlineCache", player, String.valueOf(num), "BroadcastForwarded");
		setPath(voteData.isProxyBroadcastHandled(), "OnlineCache", player, String.valueOf(num),
				"ProxyBroadcastHandled");
		setPath(voteData.encodeBroadcastTargets(), "OnlineCache", player, String.valueOf(num), "BroadcastTargets");
		setPath(voteData.encodeBroadcastForwardedServers(), "OnlineCache", player, String.valueOf(num),
				"BroadcastForwardedServers");
		setPath(voteData.isRewardDelivered(), "OnlineCache", player, String.valueOf(num), "RewardDelivered");
		setPath(voteData.encodeHttpDeliveryIds(), "OnlineCache", player, String.valueOf(num), "HttpDeliveryIds");
		setPath(voteData.encodeHttpBroadcastDeliveryIds(), "OnlineCache", player, String.valueOf(num), "HttpBroadcastDeliveryIds");
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
	public void removeTimedVotes() {
		remove("TimedVoteCache");
	}

	@Override
	public int getVotePartyCache(String server) {
		return getNode("VoteParty", "Cache", server).getInt(0);
	}

	@Override
	public Collection<String> getPendingVotePartyRewardServers() {
		Collection<String> encoded = getKeys(getNode("VoteParty", "PendingRewards"));
		Collection<String> servers = new ArrayList<>();
		if (encoded != null) for (String key : encoded) servers.add(decodeServerKey(key));
		return servers;
	}

	@Override
	public Collection<String> getPendingVotePartyRewardIds(String server) {
		return getKeys(getNode("VoteParty", "PendingRewards", encodeServerKey(server)));
	}

	@Override
	public PendingVotePartyProxyEffects getPendingVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(
				getString(getNode("VoteParty", "PendingProxyEffects", "Broadcast"), ""),
				getStringList(getNode("VoteParty", "PendingProxyEffects", "Commands"), java.util.List.of()));
	}

	@Override
	public PendingVotePartyProxyEffects getQuarantinedVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(
				getString(getNode("VoteParty", "QuarantinedProxyEffects", "Broadcast"), ""),
				getStringList(getNode("VoteParty", "QuarantinedProxyEffects", "Commands"), java.util.List.of()));
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
	public void setPendingVotePartyReward(String server, String deliveryId, boolean pending) {
		String serverKey = encodeServerKey(server);
		if (pending) setPath(true, "VoteParty", "PendingRewards", serverKey, deliveryId);
		else {
			remove("VoteParty", "PendingRewards", serverKey, deliveryId);
			Collection<String> remaining = getKeys(getNode("VoteParty", "PendingRewards", serverKey));
			if (remaining == null || remaining.isEmpty()) remove("VoteParty", "PendingRewards", serverKey);
		}
	}

	@Override
	public void setPendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		if (effects.isEmpty()) {
			remove("VoteParty", "PendingProxyEffects");
			return;
		}
		set(new Object[] { "VoteParty", "PendingProxyEffects", "Broadcast" }, effects.broadcast());
		set(new Object[] { "VoteParty", "PendingProxyEffects", "Commands" }, effects.commands());
	}

	@Override
	public void setQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		if (effects.isEmpty()) {
			remove("VoteParty", "QuarantinedProxyEffects");
			return;
		}
		set(new Object[] { "VoteParty", "QuarantinedProxyEffects", "Broadcast" }, effects.broadcast());
		set(new Object[] { "VoteParty", "QuarantinedProxyEffects", "Commands" }, effects.commands());
	}

	private static String encodeServerKey(String server) {
		return Base64.getUrlEncoder().withoutPadding()
				.encodeToString(server.toLowerCase(Locale.ROOT).getBytes(StandardCharsets.UTF_8));
	}

	private static String decodeServerKey(String server) {
		return new String(Base64.getUrlDecoder().decode(server), StandardCharsets.UTF_8);
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
			DataNode voteIdNode = node.has("VoteId") ? node.get("VoteId")
					: node.has("VoteID") ? node.get("VoteID") : null;
			if (vote.getVoteId() != null && voteIdNode != null) {
				if (vote.getVoteId().toString().equals(voteIdNode.asString())) {
					remove("VoteCache", server, num);
				}
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
				DataNode voteIdNode = node.has("VoteId") ? node.get("VoteId")
						: node.has("VoteID") ? node.get("VoteID") : null;
				if (vote.getVoteId() != null && voteIdNode != null) {
					if (vote.getVoteId().toString().equals(voteIdNode.asString())) {
						remove("OnlineCache", player, num);
					}
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

	/**
	 * Sets a value at a specific path.
	 * @param value the value to set
	 * @param path the path elements
	 */
	private void setPath(Object value, Object... path) {
		set(path, value);
	}

}
