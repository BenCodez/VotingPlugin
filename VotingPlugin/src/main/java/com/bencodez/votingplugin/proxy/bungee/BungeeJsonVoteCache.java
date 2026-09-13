package com.bencodez.votingplugin.proxy.bungee;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.Locale;

import com.bencodez.simpleapi.file.BungeeJsonFile;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.GsonDataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.GsonBuilder;

/**
 * JSON file-based vote cache for Bungee.
 */
public class BungeeJsonVoteCache extends BungeeJsonFile implements IVoteCache {
	private VotingPluginBungee bungee;

	/**
	 * Constructs a new BungeeJsonVoteCache.
	 *
	 * @param bungee the bungee plugin instance
	 */
	public BungeeJsonVoteCache(VotingPluginBungee bungee) {
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
		setString(path + ".VoteId", voteTimedQueue.getVoteId() == null ? null : voteTimedQueue.getVoteId().toString());
		setString(path + ".UUID", voteTimedQueue.getUuid());
		setBoolean(path + ".ProxyBroadcastHandled", voteTimedQueue.isProxyBroadcastHandled());
		setString(path + ".Totals", voteTimedQueue.getTotals());
		setBoolean(path + ".Processed", voteTimedQueue.isProcessed());
		setBoolean(path + ".MultiProxyForwardingHandled", voteTimedQueue.isMultiProxyForwardingHandled());
		setBoolean(path + ".MultiProxyForwardingRequired", voteTimedQueue.isMultiProxyForwardingRequired());
		setBoolean(path + ".RealVote", voteTimedQueue.isRealVote());
		setString(path + ".MultiProxyOrigin", voteTimedQueue.getMultiProxyOrigin());
		setBoolean(path + ".MultiProxyCompletionPending", voteTimedQueue.isMultiProxyCompletionPending());
		setString(path + ".MultiProxyRecipients", voteTimedQueue.encodeMultiProxyRecipients());
		setString(path + ".MultiProxyAcknowledgedServers", voteTimedQueue.encodeMultiProxyAcknowledgedServers());
		setString(path + ".MultiProxyLegacyPendingRecipients",
				voteTimedQueue.encodeMultiProxyLegacyPendingRecipients());
		setString(path + ".BroadcastTargets", voteTimedQueue.encodeBroadcastTargets());
		setString(path + ".BroadcastForwardedServers", voteTimedQueue.encodeBroadcastForwardedServers());
		setString(path + ".HttpBroadcastDeliveryIds", voteTimedQueue.encodeHttpBroadcastDeliveryIds());
	}

	@Override
	public java.nio.file.Path getStoragePath() {
		return getFile().toPath();
	}

	@Override
	public synchronized void saveDurably() throws IOException {
		Path target = getStoragePath().toAbsolutePath().normalize();
		Path parent = target.getParent();
		if (parent == null) throw new IOException("Vote cache has no parent directory");
		Files.createDirectories(parent);
		Path staged = Files.createTempFile(parent, target.getFileName().toString(), ".tmp");
		try {
			Files.writeString(staged, new GsonBuilder().setPrettyPrinting().create().toJson(getConf()),
					StandardCharsets.UTF_8);
			DurableFiles.publishStagedFile(staged, target);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	public void addVote(String server, int num, OfflineBungeeVote voteData) {
		String path = "VoteCache." + server + "." + num;
		setString(path + ".Name", voteData.getPlayerName());
		setString(path + ".Service", voteData.getService());
		setString(path + ".UUID", voteData.getUuid());
		setLong(path + ".Time", voteData.getTime());
		setBoolean(path + ".Real", voteData.isRealVote());
		setString(path + ".Text", voteData.getText());
		setString(path + ".VoteId", voteData.getVoteId() != null ? voteData.getVoteId().toString() : null);
		setBoolean(path + ".BroadcastForwarded", voteData.isBroadcastForwarded());
		setBoolean(path + ".ProxyBroadcastHandled", voteData.isProxyBroadcastHandled());
		setString(path + ".BroadcastTargets", voteData.encodeBroadcastTargets());
		setString(path + ".BroadcastForwardedServers", voteData.encodeBroadcastForwardedServers());
		setBoolean(path + ".RewardDelivered", voteData.isRewardDelivered());
		setString(path + ".HttpDeliveryIds", voteData.encodeHttpDeliveryIds());
		setString(path + ".HttpBroadcastDeliveryIds", voteData.encodeHttpBroadcastDeliveryIds());
	}

	public void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		String path = "OnlineCache." + player + "." + num;
		setString(path + ".Name", voteData.getPlayerName());
		setString(path + ".Service", voteData.getService());
		setString(path + ".UUID", voteData.getUuid());
		setLong(path + ".Time", voteData.getTime());
		setBoolean(path + ".Real", voteData.isRealVote());
		setString(path + ".Text", voteData.getText());
		setString(path + ".VoteId", voteData.getVoteId() != null ? voteData.getVoteId().toString() : null);
		setBoolean(path + ".BroadcastForwarded", voteData.isBroadcastForwarded());
		setBoolean(path + ".ProxyBroadcastHandled", voteData.isProxyBroadcastHandled());
		setString(path + ".BroadcastTargets", voteData.encodeBroadcastTargets());
		setString(path + ".BroadcastForwardedServers", voteData.encodeBroadcastForwardedServers());
		setBoolean(path + ".RewardDelivered", voteData.isRewardDelivered());
		setString(path + ".HttpDeliveryIds", voteData.encodeHttpDeliveryIds());
		setString(path + ".HttpBroadcastDeliveryIds", voteData.encodeHttpBroadcastDeliveryIds());
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

	@Override
	public void removeTimedVotes() {
		setString("TimedVoteCache", null);
	}

	public int getVotePartyCache(String server) {
		return getInt("VoteParty.Cache." + server, 0);
	}

	@Override
	public Collection<String> getPendingVotePartyRewardServers() {
		Collection<String> encoded = getKeys("VoteParty.PendingRewards");
		Collection<String> servers = new ArrayList<>();
		if (encoded != null) for (String key : encoded) servers.add(decodeServerKey(key));
		return servers;
	}

	@Override
	public Collection<String> getPendingVotePartyRewardIds(String server) {
		return getKeys("VoteParty.PendingRewards." + encodeServerKey(server));
	}

	@Override
	public PendingVotePartyProxyEffects getPendingVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(getString("VoteParty.PendingProxyEffects.Broadcast", ""),
				getStringList("VoteParty.PendingProxyEffects.Commands", java.util.List.of()));
	}

	@Override
	public PendingVotePartyProxyEffects getQuarantinedVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(getString("VoteParty.QuarantinedProxyEffects.Broadcast", ""),
				getStringList("VoteParty.QuarantinedProxyEffects.Commands", java.util.List.of()));
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

	@Override
	public void setPendingVotePartyReward(String server, String deliveryId, boolean pending) {
		String serverPath = "VoteParty.PendingRewards." + encodeServerKey(server);
		String path = serverPath + "." + deliveryId;
		if (pending) setBoolean(path, true);
		else {
			setString(path, null);
			Collection<String> remaining = getKeys(serverPath);
			if (remaining == null || remaining.isEmpty()) setString(serverPath, null);
		}
	}

	@Override
	public void setPendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		if (effects.isEmpty()) {
			remove("VoteParty.PendingProxyEffects");
			return;
		}
		setString("VoteParty.PendingProxyEffects.Broadcast", effects.broadcast());
		setStringList("VoteParty.PendingProxyEffects.Commands", effects.commands());
	}

	@Override
	public void setQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		if (effects.isEmpty()) {
			remove("VoteParty.QuarantinedProxyEffects");
			return;
		}
		setString("VoteParty.QuarantinedProxyEffects.Broadcast", effects.broadcast());
		setStringList("VoteParty.QuarantinedProxyEffects.Commands", effects.commands());
	}

	private static String encodeServerKey(String server) {
		return Base64.getUrlEncoder().withoutPadding()
				.encodeToString(server.toLowerCase(Locale.ROOT).getBytes(StandardCharsets.UTF_8));
	}

	private static String decodeServerKey(String server) {
		return new String(Base64.getUrlDecoder().decode(server), StandardCharsets.UTF_8);
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
		Collection<String> votes = getServerVotes(server);
		if (votes == null) {
			return;
		}
		// search for vote with uuid and remove it
		for (String num : votes) {
			GsonDataNode node = getServerVotes(server, num);
			if (node == null) {
				continue;
			}
			DataNode uuidNode = node.get("UUID");
			if (uuidNode == null) {
				continue;
			}
			String nodeUuid = uuidNode.asString();
			if (nodeUuid != null && nodeUuid.equals(uuid)) {
				setString("VoteCache." + server + "." + num, null);
			}
		}
	}

	@Override
	public void removeVote(String server, OfflineBungeeVote vote) {
		if (vote.getServerVoteCacheJsonKey() != null) {
			setString("VoteCache." + server + "." + vote.getServerVoteCacheJsonKey(), null);
			return;
		}
		Collection<String> votes = getServerVotes(server);
		if (votes == null) {
			return;
		}
		for (String num : votes) {
			GsonDataNode node = getServerVotes(server, num);
			if (node == null) {
				continue;
			}
			DataNode voteIdNode = node.has("VoteId") ? node.get("VoteId")
					: node.has("VoteID") ? node.get("VoteID") : null;
			if (vote.getVoteId() != null && voteIdNode != null) {
				if (vote.getVoteId().toString().equals(voteIdNode.asString())) {
					setString("VoteCache." + server + "." + num, null);
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
				setString("VoteCache." + server + "." + num, null);
			}
		}
	}

	@Override
	public void removeOnlineVote(OfflineBungeeVote vote) {
		if (vote.getOnlineVoteCacheJsonKey() != null && vote.getUuid() != null) {
			setString("OnlineCache." + vote.getUuid() + "." + vote.getOnlineVoteCacheJsonKey(), null);
			return;
		}
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
				GsonDataNode node = getOnlineVotes(player, num);
				if (node == null) {
					continue;
				}
				DataNode voteIdNode = node.has("VoteId") ? node.get("VoteId")
						: node.has("VoteID") ? node.get("VoteID") : null;
				if (vote.getVoteId() != null && voteIdNode != null) {
					if (vote.getVoteId().toString().equals(voteIdNode.asString())) {
						setString("OnlineCache." + player + "." + num, null);
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
					setString("OnlineCache." + player + "." + num, null);
				}
			}
		}
	}

}
