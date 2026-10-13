package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.io.UncheckedIOException;
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
import com.bencodez.votingplugin.proxy.cache.VoteCacheDurability;
import com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects;
import com.bencodez.votingplugin.proxy.cache.PendingPluginMessageVotePartyRewards;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;

import org.spongepowered.configurate.gson.GsonConfigurationLoader;

import com.google.gson.JsonObject;

/**
 * JSON-based vote cache implementation for Velocity proxy.
 */
public class VelocityJsonVoteCache extends VelocityJSONFile implements IVoteCache {
	private boolean writesPaused;
	private boolean journalLoaded;

	/**
	 * Constructs a new Velocity JSON vote cache.
	 * @param file the file to store cache data
	 */
	public VelocityJsonVoteCache(File file) {
		super(file);
		reload();
	}

	@Override
	public synchronized java.nio.file.Path getStoragePath() {
		return getPath();
	}

	/** Routine saves use the same atomic publication and reload failure fence as journal writes. */
	@Override
	public synchronized void save() {
		if (writesPaused) return;
		try {
			saveDurably();
		} catch (IOException failure) {
			java.util.logging.Logger.getLogger(getClass().getName())
					.log(java.util.logging.Level.WARNING, "Unable to save the vote cache", failure);
		}
	}

	@Override
	public synchronized void reload() {
		try {
			var document = VoteCacheDurability.readJournal(getStoragePath());
			PendingPluginMessageVotePartyRewards.fromCacheJson(document);
			var loaded = GsonConfigurationLoader.builder()
					.source(() -> new java.io.BufferedReader(new java.io.StringReader(document.toString())))
					.build().load();
			// Timed outbox rows have no opaque-row preservation during save. Never
			// accept a raw timed entry that Configurate would drop or reclassify.
			if (document.has("TimedVoteCache")) {
				var timed = document.get("TimedVoteCache");
				if (!timed.isJsonObject()) throw new IOException("Invalid timed vote cache structure");
				for (var entry : timed.getAsJsonObject().entrySet()) {
					if (!entry.getValue().isJsonObject() || (!loaded.node("TimedVoteCache", entry.getKey()).isMap()
							|| loaded.node("TimedVoteCache", entry.getKey()).childrenMap().isEmpty()))
						throw new IOException("Timed vote cache entry requires recovery");
				}
			}
			// readJournal already verifies the raw object. Configurate can normalize
			// an object containing only null cache entries to an empty root.
			super.setConf(loaded);
			writesPaused = false;
			journalLoaded = true;
		} catch (IOException failure) {
			writesPaused = true;
			throw new UncheckedIOException("Unable to reload the vote cache", failure);
		} catch (RuntimeException failure) {
			writesPaused = true;
			throw failure;
		}
	}

	@Override
	public synchronized org.spongepowered.configurate.ConfigurationNode getData() { return super.getData(); }
	@Override
	public synchronized org.spongepowered.configurate.ConfigurationNode getConf() { return super.getConf(); }
	@Override
	public synchronized org.spongepowered.configurate.ConfigurationNode getNode(Object... path) { return super.getNode(path); }
	@Override
	public synchronized void setConf(org.spongepowered.configurate.ConfigurationNode conf) { super.setConf(conf); }
	@Override
	public synchronized void set(Object[] path, Object value) { super.set(path, value); }
	@Override
	public synchronized void remove(Object... path) { super.remove(path); }

	@Override
	public synchronized void saveDurably() throws IOException {
		if (writesPaused) throw new IOException("Vote cache writes require a successful reload");
		Path target = getStoragePath().toAbsolutePath().normalize();
		JsonObject previous = null;
		if (journalLoaded) {
			try {
				previous = VoteCacheDurability.readJournal(target);
				PendingPluginMessageVotePartyRewards.fromCacheJson(previous);
			}
			catch (IOException | RuntimeException failure) {
				writesPaused = true;
				throw new IOException("Vote cache is unreadable; reload is required before saving", failure);
			}
		}
		Path parent = target.getParent();
		if (parent == null) throw new IOException("Vote cache has no parent directory");
		Files.createDirectories(parent);
		Path staged = Files.createTempFile(parent, target.getFileName().toString(), ".tmp");
		try {
			Object root = getConf().raw();
			if (root == null || root instanceof java.util.Map<?, ?> map && map.isEmpty()) {
				// Configurate's empty-root writer can emit null or consecutive objects.
				// An empty cache always remains one complete JSON object on disk.
				Files.writeString(staged, "{}", StandardCharsets.UTF_8);
			} else {
				GsonConfigurationLoader.builder().path(staged).build().save(getConf());
			}
			JsonObject candidate = VoteCacheDurability.readJournal(staged);
			if (previous != null && hasEmergencyJournalMarker()
					&& VoteCacheDurability.preserveUnresolvedCacheRows(previous, candidate)) {
				Files.writeString(staged, candidate.toString(), StandardCharsets.UTF_8);
			}
			PendingPluginMessageVotePartyRewards.fromCacheJson(VoteCacheDurability.readJournal(staged));
			DurableFiles.publishStagedFile(staged, target);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	@Override
	public synchronized void addTimedVote(int num, VoteTimeQueue voteTimedQueue) {
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
		setPath(voteTimedQueue.isVotePartyApplied(), "TimedVoteCache", String.valueOf(num), "VotePartyApplied");
		setPath(voteTimedQueue.isTotalsApplied(), "TimedVoteCache", String.valueOf(num), "TotalsApplied");
		setPath(voteTimedQueue.isMultiProxyForwardingHandled(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyForwardingHandled");
		setPath(voteTimedQueue.isMultiProxyForwardingRequired(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyForwardingRequired");
		setPath(voteTimedQueue.isMultiProxyCapabilityDiscoveryPending(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyCapabilityDiscoveryPending");
		setPath(voteTimedQueue.isRealVote(), "TimedVoteCache", String.valueOf(num), "RealVote");
		setPath(voteTimedQueue.isWasOnline(), "TimedVoteCache", String.valueOf(num), "WasOnline");
		setPath(voteTimedQueue.isWasOnlineKnown(), "TimedVoteCache", String.valueOf(num), "WasOnlineKnown");
		setPath(voteTimedQueue.isDelayValidated(), "TimedVoteCache", String.valueOf(num), "DelayValidated");
		setPath(voteTimedQueue.isQueueClassificationKnown(), "TimedVoteCache", String.valueOf(num),
				"DelayValidationKnown");
		setPath(voteTimedQueue.getMultiProxyOrigin(), "TimedVoteCache", String.valueOf(num), "MultiProxyOrigin");
		setPath(voteTimedQueue.isMultiProxyCompletionPending(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyCompletionPending");
		setPath(voteTimedQueue.encodeMultiProxyRecipients(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyRecipients");
		setPath(voteTimedQueue.encodeMultiProxyAcknowledgedServers(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyAcknowledgedServers");
		setPath(voteTimedQueue.encodeMultiProxyLegacyPendingRecipients(), "TimedVoteCache", String.valueOf(num),
				"MultiProxyLegacyPendingRecipients");
		setPath(voteTimedQueue.encodeBroadcastTargets(), "TimedVoteCache", String.valueOf(num), "BroadcastTargets");
		setPath(voteTimedQueue.encodeBroadcastForwardedServers(), "TimedVoteCache", String.valueOf(num),
				"BroadcastForwardedServers");
		setPath(voteTimedQueue.encodeHttpBroadcastDeliveryIds(), "TimedVoteCache", String.valueOf(num),
				"HttpBroadcastDeliveryIds");
	}

	@Override
	public synchronized void addVote(String server, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "VoteCache", server, String.valueOf(num), "Name");
		setPath(voteData.getService(), "VoteCache", server, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "VoteCache", server, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "VoteCache", server, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "VoteCache", server, String.valueOf(num), "Real");
		setPath(voteData.isWasOnline(), "VoteCache", server, String.valueOf(num), "WasOnline");
		setPath(voteData.isWasOnlineKnown(), "VoteCache", server, String.valueOf(num), "WasOnlineKnown");
		setPath(voteData.isDelayValidated(), "VoteCache", server, String.valueOf(num), "DelayValidated");
		setPath(voteData.isQueueClassificationKnown(), "VoteCache", server, String.valueOf(num), "DelayValidationKnown");
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
		setPath(voteData.getCachedBatchNumber(), "VoteCache", server, String.valueOf(num), "CachedBatchNumber");
		setPath(voteData.getCachedBatchSize(), "VoteCache", server, String.valueOf(num), "CachedBatchSize");
		setPath(voteData.getCachedBatchSourceRowId(), "VoteCache", server, String.valueOf(num), "CachedBatchSourceRowId");
		setPath(voteData.encodeHttpBroadcastDeliveryIds(), "VoteCache", server, String.valueOf(num), "HttpBroadcastDeliveryIds");
	}

	@Override
	public synchronized void addVoteOnline(String player, int num, OfflineBungeeVote voteData) {
		setPath(voteData.getPlayerName(), "OnlineCache", player, String.valueOf(num), "Name");
		setPath(voteData.getService(), "OnlineCache", player, String.valueOf(num), "Service");
		setPath(voteData.getUuid(), "OnlineCache", player, String.valueOf(num), "UUID");
		setPath(voteData.getTime(), "OnlineCache", player, String.valueOf(num), "Time");
		setPath(voteData.isRealVote(), "OnlineCache", player, String.valueOf(num), "Real");
		setPath(voteData.isWasOnline(), "OnlineCache", player, String.valueOf(num), "WasOnline");
		setPath(voteData.isWasOnlineKnown(), "OnlineCache", player, String.valueOf(num), "WasOnlineKnown");
		setPath(voteData.isDelayValidated(), "OnlineCache", player, String.valueOf(num), "DelayValidated");
		setPath(voteData.isQueueClassificationKnown(), "OnlineCache", player, String.valueOf(num), "DelayValidationKnown");
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
		setPath(voteData.getCachedBatchNumber(), "OnlineCache", player, String.valueOf(num), "CachedBatchNumber");
		setPath(voteData.getCachedBatchSize(), "OnlineCache", player, String.valueOf(num), "CachedBatchSize");
		setPath(voteData.getCachedBatchSourceRowId(), "OnlineCache", player, String.valueOf(num), "CachedBatchSourceRowId");
		setPath(voteData.encodeHttpBroadcastDeliveryIds(), "OnlineCache", player, String.valueOf(num), "HttpBroadcastDeliveryIds");
	}

	@Override
	public synchronized void clearData() {
		remove("VoteCache");
		remove("OnlineCache");
		remove("TimedVoteCache");
		save();
	}

	@Override
	public synchronized Collection<String> getOnlineVotes(String name) {
		return getKeys(getNode("OnlineCache", name));
	}

	@Override
	public ConfigDataNode getOnlineVotes(String name, String num) {
		return new ConfigDataNode(getNode("OnlineCache", name, num));
	}

	@Override
	public synchronized Collection<String> getPlayers() {
		return getKeys(getNode("OnlineCache"));
	}

	@Override
	public synchronized Collection<String> getServers() {
		return getKeys(getNode("VoteCache"));
	}

	@Override
	public synchronized Collection<String> getServerVotes(String server) {
		return getKeys(getNode("VoteCache", server));
	}

	@Override
	public ConfigDataNode getServerVotes(String server, String num) {
		return new ConfigDataNode(getNode("VoteCache", server, num));
	}

	@Override
	public synchronized Collection<String> getTimedVoteCache() {
		return getKeys(getNode("TimedVoteCache"));
	}

	@Override
	public ConfigDataNode getTimedVoteCache(String key) {
		return new ConfigDataNode(getNode("TimedVoteCache", key));
	}

	@Override
	public synchronized void removeTimedVotes() {
		remove("TimedVoteCache");
	}

	@Override
	public synchronized int getVotePartyCache(String server) {
		return getNode("VoteParty", "Cache", server).getInt(0);
	}

	@Override
	public synchronized PendingPluginMessageVotePartyRewards getPluginMessageVotePartyRewards() {
		return PendingPluginMessageVotePartyRewards.fromStorageValue(
				getNode("VoteParty", "PluginMessageRewards").raw());
	}

	@Override
	public synchronized void setPluginMessageVotePartyRewards(PendingPluginMessageVotePartyRewards rewards) {
		if (rewards == null) throw new IllegalArgumentException("Missing plugin-message vote-party backlog");
		if (rewards.deliveries().isEmpty()) remove("VoteParty", "PluginMessageRewards");
		else set(new Object[] { "VoteParty", "PluginMessageRewards" }, rewards.toStorageValue());
	}

	@Override
	public synchronized Collection<String> getPendingVotePartyRewardServers() {
		Collection<String> encoded = getKeys(getNode("VoteParty", "PendingRewards"));
		Collection<String> servers = new ArrayList<>();
		if (encoded != null) for (String key : encoded) try {
			servers.add(decodeServerKey(key));
		} catch (IllegalArgumentException malformedKey) {
			// Preserve later valid deliveries when one persisted key is malformed.
		}
		return servers;
	}

	@Override
	public synchronized Collection<String> getPendingVotePartyRewardIds(String server) {
		return getKeys(getNode("VoteParty", "PendingRewards", encodeServerKey(server)));
	}

	@Override
	public synchronized PendingVotePartyProxyEffects getPendingVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(
				getString(getNode("VoteParty", "PendingProxyEffects", "Broadcast"), ""),
				getStringList(getNode("VoteParty", "PendingProxyEffects", "Commands"), java.util.List.of()));
	}

	@Override
	public synchronized PendingVotePartyProxyEffects getQuarantinedVotePartyProxyEffects() {
		return new PendingVotePartyProxyEffects(
				getString(getNode("VoteParty", "QuarantinedProxyEffects", "Broadcast"), ""),
				getStringList(getNode("VoteParty", "QuarantinedProxyEffects", "Commands"), java.util.List.of()));
	}

	@Override
	public synchronized int getVotePartyCurrentVotes() {
		return getInt(getNode("VoteParty", "CurrentVotes"), 0);
	}

	@Override
	public synchronized int getVotePartyInreaseVotesRequired() {
		return getInt(getNode("VoteParty", "IncreaseVotes"), 0);
	}

	@Override
	public synchronized void setVotePartyCache(String server, int amount) {
		setPath(amount, "VoteParty", "Cache", server);
	}

	@Override
	public synchronized void setPendingVotePartyReward(String server, String deliveryId, boolean pending) {
		String serverKey = encodeServerKey(server);
		if (pending) setPath(true, "VoteParty", "PendingRewards", serverKey, deliveryId);
		else {
			remove("VoteParty", "PendingRewards", serverKey, deliveryId);
			Collection<String> remaining = getKeys(getNode("VoteParty", "PendingRewards", serverKey));
			if (remaining == null || remaining.isEmpty()) remove("VoteParty", "PendingRewards", serverKey);
		}
	}

	@Override
	public synchronized void setPendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
		if (effects.isEmpty()) {
			remove("VoteParty", "PendingProxyEffects");
			return;
		}
		set(new Object[] { "VoteParty", "PendingProxyEffects", "Broadcast" }, effects.broadcast());
		set(new Object[] { "VoteParty", "PendingProxyEffects", "Commands" }, effects.commands());
	}

	@Override
	public synchronized void setQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects) {
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
	public synchronized void setVotePartyCurrentVotes(int amount) {
		setPath(amount, "VoteParty", "CurrentVotes");
	}

	@Override
	public synchronized void setVotePartyInreaseVotesRequired(int amount) {
		setPath(amount, "VoteParty", "IncreaseVotes");
	}

	@Override
	public synchronized void removeOnlineVotes(String player) {
		remove("OnlineCache", player);
	}

	@Override
	public synchronized void removeServerVotes(String server) {
		remove("VoteCache", server);
	}

	@Override
	public synchronized void removeServerVote(String server, String uuid) {
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
	public synchronized void removeVote(String server, OfflineBungeeVote vote) {
		if (vote.getServerVoteCacheJsonKey() != null) {
			remove("VoteCache", server, vote.getServerVoteCacheJsonKey());
			return;
		}
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
	public synchronized void removeOnlineVote(OfflineBungeeVote vote) {
		if (vote.getOnlineVoteCacheJsonKey() != null && vote.getUuid() != null) {
			remove("OnlineCache", vote.getUuid(), vote.getOnlineVoteCacheJsonKey());
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
