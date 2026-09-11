package com.bencodez.votingplugin.proxy.cache;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.util.DurableFiles;

import lombok.Getter;

/**
 * Handles caching of votes for proxy servers.
 */
public abstract class VoteCacheHandler {
	private static final int MAX_PENDING_PERSISTENCE_VOTES = 1024;

	/**
	 * Queue of timed votes for time change processing.
	 */
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	// uuid based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	// server based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new ConcurrentHashMap<>();

	// Entries whose durable insert failed remain isolated from delivery until a
	// retry succeeds. This prevents both silent loss and dispatch from memory
	// before the cache/outbox record is durable.
	private final ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> pendingOnlineVotePersistence =
			new ConcurrentHashMap<>();
	private final ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> pendingServerVotePersistence =
			new ConcurrentHashMap<>();
	private int pendingPersistenceVoteCount;
	private boolean jsonStorageQuarantined;

	/**
	 * Checks if a server has cached votes.
	 * @param server the server name
	 * @return true if the server has cached votes
	 */
	public boolean hasVotes(String server) {
		return cachedVotes.containsKey(server);
	}

	/**
	 * Gets cached votes for a server.
	 * @param server the server name
	 * @return list of cached votes
	 */
	public ArrayList<OfflineBungeeVote> getVotes(String server) {
		return cachedVotes.getOrDefault(server, new ArrayList<>());
	}

	/**
	 * Get total cached votes for a UUID across: - UUID-based online vote cache -
	 * Server-based vote cache
	 *
	 * @param uuid player UUID (string form)
	 * @return total cached votes across all proxy caches
	 */
	public int getProxyCachedTotal(String uuid) {
		if (uuid == null || uuid.isEmpty()) {
			return 0;
		}
		int total = 0;

		// 1) UUID-based cache (fast lookup)
		ArrayList<OfflineBungeeVote> onlineVotes = cachedOnlineVotes.get(uuid);
		if (onlineVotes != null) {
			total += onlineVotes.size();
		}

		// 2) Server-based caches (scan)
		for (ArrayList<OfflineBungeeVote> serverVotes : cachedVotes.values()) {
			for (OfflineBungeeVote vote : serverVotes) {
				if (vote != null && vote.getUuid() != null && vote.getUuid().equalsIgnoreCase(uuid)) {
					total++;
				}
			}
		}

		return total;
	}

	/**
	 * Adds a vote to the server cache.
	 * @param server the server name
	 * @param vote the vote to add
	 */
	public synchronized void addServerVote(String server, OfflineBungeeVote vote) {
		addServerVoteDurably(server, vote);
	}

	/**
	 * Adds a server vote only when it has been durably stored. MySQL failures use
	 * the JSON cache as an emergency journal when it is available.
	 *
	 * @param server the server name
	 * @param vote the vote to add
	 * @return true when the vote is durable (including an already cached vote)
	 */
	public synchronized boolean addServerVoteDurably(String server, OfflineBungeeVote vote) {
		if (server == null || vote == null) {
			return false;
		}
		if (containsServerVote(server, vote)) {
			debug1("Not caching duplicate vote " + vote.getVoteId() + " for server " + server);
			return true;
		}

		boolean stored;
		if (useMySQL) {
			stored = voteCacheTable.tryInsertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText(), vote.isBroadcastForwarded(),
					vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.isRewardDelivered(), vote.encodeHttpDeliveryIds(),
					vote.encodeHttpBroadcastDeliveryIds(), server);
			if (!stored) {
				stored = persistServerVoteToJson(server, vote);
			}
		} else {
			stored = persistServerVoteToJson(server, vote);
		}
		if (stored) {
			cachedVotes.putIfAbsent(server, new ArrayList<>());
			cachedVotes.get(server).add(vote);
		}
		return stored;
	}

	/** Retains a failed server-cache insert for a later durability retry. */
	public synchronized boolean retainServerVoteForPersistenceRetry(String server, OfflineBungeeVote vote) {
		if (server == null || vote == null) return false;
		ArrayList<OfflineBungeeVote> pending = pendingServerVotePersistence.get(server);
		if (containsVoteId(pending, vote.getVoteId())) return true;
		if (pendingPersistenceVoteCount >= MAX_PENDING_PERSISTENCE_VOTES) return false;
		pendingServerVotePersistence.putIfAbsent(server, new ArrayList<>());
		pending = pendingServerVotePersistence.get(server);
		pending.add(vote);
		pendingPersistenceVoteCount++;
		return true;
	}

	/**
	 * Persists updated delivery state for an existing server-cached vote.
	 *
	 * @param server backend server owning the cached reward
	 * @param vote cached vote with updated delivery state
	 */
	public synchronized boolean updateServerVote(String server, OfflineBungeeVote vote) {
		if (useMySQL) {
			if (voteCacheTable.updateProxyBroadcastState(vote, server)) {
				return true;
			}
			return updateServerVoteJson(server, vote);
		}
		if (jsonStorage == null || jsonStorageQuarantined) return false;

		Collection<String> keys = jsonStorage.getServerVotes(server);
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getServerVotes(server, key);
			if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service")
					|| !data.has("Time")) {
				continue;
			}
			if (matchesStoredVote(data, vote)) {
				try {
					int index = Integer.parseInt(key);
					jsonStorage.addVote(server, index, vote);
					return verifyJsonServerVote(server, index, vote);
				} catch (RuntimeException e) {
					debug1(e);
					return false;
				}
			}
		}
		return false;
	}

	/**
	 * Removes a vote for a specific player from a server cache.
	 * @param server the server name
	 * @param uuid the player UUID
	 */
	public void removeVote(String server, String uuid) {
		if (cachedVotes.containsKey(server)) {
			ArrayList<OfflineBungeeVote> votes = cachedVotes.get(server);
			votes.removeIf(vote -> vote.getUuid().equals(uuid));
		}
		if (useMySQL) {
			voteCacheTable.removeVotesByServerAndUUID(server, uuid);
		}
		if (jsonStorage != null && !jsonStorageQuarantined) {
			jsonStorage.removeServerVote(server, uuid);
			jsonStorage.save();
		}
	}

	/**
	 * Removes all cached votes for a server.
	 * @param server the server name
	 */
	public void removeVotes(String server) {
		cachedVotes.remove(server);
		if (useMySQL) {
			voteCacheTable.removeVotesByServer(server);
		}
		if (jsonStorage != null && !jsonStorageQuarantined) {
			jsonStorage.removeServerVotes(server);
			jsonStorage.save();
		}
	}

	/**
	 * Checks if a player has cached online votes.
	 * @param uuid the player UUID
	 * @return true if the player has cached online votes
	 */
	public boolean hasOnlineVotes(String uuid) {
		return cachedOnlineVotes.containsKey(uuid);
	}

	/**
	 * Gets cached online votes for a player.
	 * @param uuid the player UUID
	 * @return list of cached online votes
	 */
	public ArrayList<OfflineBungeeVote> getOnlineVotes(String uuid) {
		return cachedOnlineVotes.getOrDefault(uuid, new ArrayList<>());
	}

	/**
	 * Gets a snapshot of player UUID keys that have voter-keyed cached votes.
	 *
	 * @return cached player UUIDs
	 */
	public Set<String> getOnlineVoteUUIDs() {
		return new LinkedHashSet<>(cachedOnlineVotes.keySet());
	}

	/**
	 * Adds a vote to the online vote cache for a player.
	 * @param uuid the player UUID
	 * @param vote the vote to add
	 */
	public synchronized void addOnlineVote(String uuid, OfflineBungeeVote vote) {
		addOnlineVoteDurably(uuid, vote);
	}

	/**
	 * Adds an online vote only when it has been durably stored. MySQL failures use
	 * the JSON cache as an emergency journal when it is available.
	 *
	 * @param uuid voter UUID
	 * @param vote the vote to add
	 * @return true when the vote is durable (including an already cached vote)
	 */
	public synchronized boolean addOnlineVoteDurably(String uuid, OfflineBungeeVote vote) {
		if (uuid == null || vote == null) {
			return false;
		}
		if (containsOnlineVote(uuid, vote)) {
			debug1("Not caching duplicate online vote " + vote.getVoteId() + " for " + uuid);
			return true;
		}

		boolean stored;
		if (useMySQL) {
			stored = onlineVoteCacheTable.tryInsertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText(), vote.isBroadcastForwarded(),
					vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.isRewardDelivered(), vote.encodeHttpDeliveryIds(),
					vote.encodeHttpBroadcastDeliveryIds());
			if (!stored) {
				stored = persistOnlineVoteToJson(uuid, vote);
			}
		} else {
			stored = persistOnlineVoteToJson(uuid, vote);
		}
		if (stored) {
			cachedOnlineVotes.putIfAbsent(uuid, new ArrayList<>());
			cachedOnlineVotes.get(uuid).add(vote);
		}
		return stored;
	}

	/** Retains a failed online-cache insert for a later durability retry. */
	public synchronized boolean retainOnlineVoteForPersistenceRetry(String uuid, OfflineBungeeVote vote) {
		if (uuid == null || vote == null) return false;
		ArrayList<OfflineBungeeVote> pending = pendingOnlineVotePersistence.get(uuid);
		if (containsVoteId(pending, vote.getVoteId())) return true;
		if (pendingPersistenceVoteCount >= MAX_PENDING_PERSISTENCE_VOTES) return false;
		pendingOnlineVotePersistence.putIfAbsent(uuid, new ArrayList<>());
		pending = pendingOnlineVotePersistence.get(uuid);
		pending.add(vote);
		pendingPersistenceVoteCount++;
		return true;
	}

	/**
	 * Retries failed cache inserts. Entries become visible to normal delivery only
	 * after their ordinary durable insert succeeds.
	 */
	public synchronized boolean retryPendingVotePersistence() {
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry
				: new ArrayList<>(pendingServerVotePersistence.entrySet())) {
			for (OfflineBungeeVote vote : new ArrayList<>(entry.getValue())) {
				if (addServerVoteDurably(entry.getKey(), vote) && entry.getValue().remove(vote)) {
					pendingPersistenceVoteCount--;
				}
			}
			if (entry.getValue().isEmpty()) pendingServerVotePersistence.remove(entry.getKey());
		}
		for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry
				: new ArrayList<>(pendingOnlineVotePersistence.entrySet())) {
			for (OfflineBungeeVote vote : new ArrayList<>(entry.getValue())) {
				if (addOnlineVoteDurably(entry.getKey(), vote) && entry.getValue().remove(vote)) {
					pendingPersistenceVoteCount--;
				}
			}
			if (entry.getValue().isEmpty()) pendingOnlineVotePersistence.remove(entry.getKey());
		}
		return pendingServerVotePersistence.isEmpty() && pendingOnlineVotePersistence.isEmpty();
	}

	private boolean containsVoteId(Collection<OfflineBungeeVote> votes, UUID voteId) {
		if (votes == null || voteId == null) return false;
		for (OfflineBungeeVote candidate : votes) {
			if (candidate != null && voteId.equals(candidate.getVoteId())) return true;
		}
		return false;
	}

	/**
	 * Persists updated delivery state for an existing voter-keyed cached vote.
	 *
	 * @param uuid voter cache key
	 * @param vote cached vote with updated delivery state
	 */
	public synchronized boolean updateOnlineVote(String uuid, OfflineBungeeVote vote) {
		if (useMySQL) {
			if (onlineVoteCacheTable.updateProxyBroadcastState(vote)) {
				return true;
			}
			return updateOnlineVoteJson(uuid, vote);
		}
		if (jsonStorage == null || jsonStorageQuarantined) return false;

		Collection<String> keys = jsonStorage.getOnlineVotes(uuid);
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getOnlineVotes(uuid, key);
			if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service")
					|| !data.has("Time")) {
				continue;
			}
			if (matchesStoredVote(data, vote)) {
				try {
					int index = Integer.parseInt(key);
					jsonStorage.addVoteOnline(uuid, index, vote);
					return verifyJsonOnlineVote(uuid, index, vote);
				} catch (RuntimeException e) {
					debug1(e);
					return false;
				}
			}
		}
		return false;
	}

	/**
	 * Clears voter-keyed reward eligibility while retaining entries that still have
	 * standalone proxy broadcast targets to deliver.
	 *
	 * @param uuid player UUID whose global reward was delivered by another proxy
	 */
	public synchronized void clearOnlineVoteRewards(String uuid) {
		ArrayList<OfflineBungeeVote> votes = cachedOnlineVotes.get(uuid);
		if (votes == null || votes.isEmpty()) {
			return;
		}

		for (OfflineBungeeVote vote : new ArrayList<>(votes)) {
			if (vote.isProxyBroadcastHandled() && !vote.isProxyBroadcastComplete()) {
				if (!vote.isRewardDelivered()) {
					vote.setRewardDelivered(true);
					vote.setDeliveryStateDirty(true);
				}
				if (vote.isDeliveryStateDirty() && updateOnlineVote(uuid, vote)) vote.setDeliveryStateDirty(false);
				continue;
			}
			removeOnlineVote(uuid, vote);
		}
	}

	/**
	 * Removes one voter-keyed cached vote by its stable vote identity.
	 *
	 * @param uuid voter cache key
	 * @param removedVote vote to remove
	 */
	public synchronized void removeOnlineVote(String uuid, OfflineBungeeVote removedVote) {
		tryRemoveOnlineVote(uuid, removedVote);
	}

	/** Removes one voter-keyed cached vote after every configured store confirms deletion. */
	public synchronized boolean tryRemoveOnlineVote(String uuid, OfflineBungeeVote removedVote) {
		ArrayList<OfflineBungeeVote> votes = cachedOnlineVotes.get(uuid);
		if (votes == null || votes.isEmpty()) {
			return true;
		}
		boolean mysqlRemoved = !useMySQL || onlineVoteCacheTable.tryRemoveVote(removedVote);
		boolean jsonRemoved = jsonStorage == null;
		if (jsonStorage != null && !jsonStorageQuarantined) {
			jsonRemoved = removeJsonOnlineVoteDurably(uuid, removedVote);
		}
		boolean removedDurably = mysqlRemoved && jsonRemoved;
		if (!removedDurably) return false;
		votes.removeIf(vote -> sameVoteIdentity(vote, removedVote));
		if (votes.isEmpty()) cachedOnlineVotes.remove(uuid);
		return true;
	}

	protected boolean removeJsonOnlineVoteDurably(String uuid, OfflineBungeeVote removedVote) {
		jsonStorage.removeOnlineVote(removedVote);
		try {
			return VoteCacheDurability.saveAndVerifyRemoval(jsonStorage,
					() -> !containsStoredOnlineVote(uuid, removedVote));
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		}
	}

	private boolean containsStoredOnlineVote(String uuid, OfflineBungeeVote expected) {
		Collection<String> keys = jsonStorage.getOnlineVotes(uuid);
		if (keys == null) return false;
		for (String key : keys) {
			DataNode data = jsonStorage.getOnlineVotes(uuid, key);
			if (data != null && matchesStoredVote(data, expected)) return true;
		}
		return false;
	}

	/**
	 * Removes all cached online votes for a player.
	 * @param uuid the player UUID
	 */
	public void removeOnlineVotes(String uuid) {
		cachedOnlineVotes.remove(uuid);
		if (useMySQL) {
			onlineVoteCacheTable.removeVotesByUuid(uuid);
		}
		if (jsonStorage != null && !jsonStorageQuarantined) {
			jsonStorage.removeOnlineVotes(uuid);
			jsonStorage.save();
		}
	}

	/**
	 * Checks and removes expired votes from cache.
	 * @param voteCacheTime cache time in days
	 */
	public void checkVoteCacheTime(int voteCacheTime) {
		long cTime = LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();

		// Collect expired online votes
		ArrayList<OfflineBungeeVote> expiredOnlineVotes = new ArrayList<>();
		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				if (vote.getTime() + (voteCacheTime * 24 * 60 * 60 * 1000L) < cTime) {
					debug1("Removing vote from cache: " + vote.toString());
					expiredOnlineVotes.add(vote);
				}
			}
		}
		removeOnlineVotes(expiredOnlineVotes);

		// Collect expired server votes
		ArrayList<OfflineBungeeVote> expiredServerVotes = new ArrayList<>();
		for (Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
			ArrayList<OfflineBungeeVote> votes = entry.getValue();
			for (OfflineBungeeVote vote : votes) {
				if (vote.getTime() + (voteCacheTime * 24 * 60 * 60 * 1000L) < cTime) {
					debug1("Removing vote from cache: " + vote.toString());
					expiredServerVotes.add(vote);
				}
			}
		}

		for (String server : cachedVotes.keySet()) {
			removeServerVotes(server, expiredServerVotes);
		}
	}

	private boolean containsServerVote(String server, OfflineBungeeVote candidate) {
		for (OfflineBungeeVote vote : getVotes(server)) {
			if (sameVoteIdentity(vote, candidate)) {
				return true;
			}
		}
		return false;
	}

	private boolean containsOnlineVote(String uuid, OfflineBungeeVote candidate) {
		for (OfflineBungeeVote vote : getOnlineVotes(uuid)) {
			if (sameVoteIdentity(vote, candidate)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Reads a vote identifier using the current key and the legacy key.
	 *
	 * @param data cached vote data
	 * @return stored vote identifier or an empty string
	 */
	private String readVoteId(DataNode data) {
		if (data.has("VoteId")) {
			return data.get("VoteId").asString();
		}
		if (data.has("VoteID")) {
			return data.get("VoteID").asString();
		}
		return "";
	}

	/**
	 * Reads an optional UUID from cached data.
	 *
	 * @param data cached data
	 * @param key value key
	 * @return parsed UUID or null
	 */
	private UUID readUuid(DataNode data, String key) {
		if (!data.has(key)) {
			return null;
		}
		String value = data.get(key).asString();
		if (value == null || value.isEmpty()) {
			return null;
		}
		try {
			return UUID.fromString(value);
		} catch (IllegalArgumentException ignored) {
			return null;
		}
	}

	private boolean matchesStoredVote(DataNode data, OfflineBungeeVote vote) {
		String storedVoteId = readVoteId(data);
		if (vote.getVoteId() != null && storedVoteId != null && !storedVoteId.isEmpty()) {
			return vote.getVoteId().toString().equals(storedVoteId);
		}
		return vote.getUuid().equals(data.get("UUID").asString())
				&& vote.getService().equals(data.get("Service").asString())
				&& vote.getTime() == data.get("Time").asLong();
	}

	/** Persists a server vote in the JSON emergency journal. */
	private boolean persistServerVoteToJson(String server, OfflineBungeeVote vote) {
		if (jsonStorage == null || jsonStorageQuarantined) {
			return false;
		}
		try {
			Collection<String> keys = jsonStorage.getServerVotes(server);
			int index = nextCacheIndex(keys);
			jsonStorage.addVote(server, index, vote);
			return verifyJsonServerVote(server, index, vote);
		} catch (RuntimeException e) {
			debug1(e);
			return false;
		}
	}

	/** Persists an online vote in the JSON emergency journal. */
	private boolean persistOnlineVoteToJson(String uuid, OfflineBungeeVote vote) {
		if (jsonStorage == null || jsonStorageQuarantined) {
			return false;
		}
		try {
			Collection<String> keys = jsonStorage.getOnlineVotes(uuid);
			int index = nextCacheIndex(keys);
			jsonStorage.addVoteOnline(uuid, index, vote);
			return verifyJsonOnlineVote(uuid, index, vote);
		} catch (RuntimeException e) {
			debug1(e);
			return false;
		}
	}

	protected boolean verifyJsonServerVote(String server, int index, OfflineBungeeVote vote) {
		try {
			return VoteCacheDurability.saveAndVerifyServerVote(jsonStorage, server, index, vote);
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		}
	}

	protected boolean verifyJsonOnlineVote(String uuid, int index, OfflineBungeeVote vote) {
		try {
			return VoteCacheDurability.saveAndVerifyOnlineVote(jsonStorage, uuid, index, vote);
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		}
	}

	protected boolean verifyJsonTimeVote(int index, VoteTimeQueue vote) {
		try {
			return VoteCacheDurability.saveAndVerifyTimeVote(jsonStorage, index, vote);
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		}
	}

	private int nextCacheIndex(Collection<String> keys) {
		int index = 0;
		while (keys != null && keys.contains(String.valueOf(index))) {
			index++;
		}
		return index;
	}

	private boolean updateServerVoteJson(String server, OfflineBungeeVote vote) {
		if (jsonStorage == null || jsonStorageQuarantined) {
			return false;
		}
		Collection<String> keys = jsonStorage.getServerVotes(server);
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getServerVotes(server, key);
			if (data != null && data.isObject() && matchesStoredVote(data, vote)) {
				try {
					int index = Integer.parseInt(key);
					jsonStorage.addVote(server, index, vote);
					return verifyJsonServerVote(server, index, vote);
				} catch (RuntimeException e) {
					debug1(e);
					return false;
				}
			}
		}
		return false;
	}

	private boolean updateOnlineVoteJson(String uuid, OfflineBungeeVote vote) {
		if (jsonStorage == null || jsonStorageQuarantined) {
			return false;
		}
		Collection<String> keys = jsonStorage.getOnlineVotes(uuid);
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getOnlineVotes(uuid, key);
			if (data != null && data.isObject() && matchesStoredVote(data, vote)) {
				try {
					int index = Integer.parseInt(key);
					jsonStorage.addVoteOnline(uuid, index, vote);
					return verifyJsonOnlineVote(uuid, index, vote);
				} catch (RuntimeException e) {
					debug1(e);
					return false;
				}
			}
		}
		return false;
	}

	private boolean sameVoteIdentity(OfflineBungeeVote first, OfflineBungeeVote second) {
		if (first.getVoteId() != null && second.getVoteId() != null) {
			return first.getVoteId().equals(second.getVoteId());
		}
		return first.getUuid().equals(second.getUuid()) && first.getService().equals(second.getService())
				&& first.getTime() == second.getTime();
	}

	private boolean matchesStoredTimeVote(DataNode data, VoteTimeQueue vote) {
		String storedVoteId = readVoteId(data);
		if (vote.getVoteId() != null && storedVoteId != null && !storedVoteId.isEmpty()) {
			return vote.getVoteId().toString().equals(storedVoteId);
		}
		return data.has("Name") && data.has("Service") && data.has("Time")
				&& vote.getName().equals(data.get("Name").asString())
				&& vote.getService().equals(data.get("Service").asString())
				&& vote.getTime() == data.get("Time").asLong();
	}

	/**
	 * Saves the vote cache to storage.
	 */
	public void saveVoteCache() {
		if (jsonStorage != null && !jsonStorageQuarantined) {
			jsonStorage.save();
		}
	}

	/**
	 * Adds a timed vote to the cache queue.
	 * @param vote the timed vote to add
	 * @return true when the vote was durably stored
	 */
	public synchronized boolean addTimeVoteToCache(VoteTimeQueue vote) {
		if (vote == null) {
			return false;
		}
		timeChangeQueue.add(vote);
		if (useMySQL) {
			boolean stored = timedVoteCacheTable.insertTimedVote(vote.getVoteId(), vote.getUuid(), vote.getName(), vote.getService(),
					vote.getTime(), vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.getTotals(), vote.isProcessed(),
					vote.encodeHttpBroadcastDeliveryIds());
			if (!stored) {
				timeChangeQueue.remove(vote);
			}
			return stored;
		}
		if (jsonStorage == null || jsonStorageQuarantined) {
			timeChangeQueue.remove(vote);
			return false;
		}

		try {
			Collection<String> keys = jsonStorage.getTimedVoteCache();
			int index = 0;
			while (keys != null && keys.contains(String.valueOf(index))) {
				index++;
			}
			jsonStorage.addTimedVote(index, vote);
			if (verifyJsonTimeVote(index, vote)) return true;
			timeChangeQueue.remove(vote);
			return false;
		} catch (RuntimeException e) {
			timeChangeQueue.remove(vote);
			debug1(e);
			return false;
		}
	}

	/**
	 * Persists changed delivery state for a queued rollover vote.
	 *
	 * @param vote queued vote to update
	 * @return true when the durable state update completed
	 */
	public synchronized boolean updateTimeVote(VoteTimeQueue vote) {
		if (useMySQL) {
			return timedVoteCacheTable.updateTimedVote(vote);
		}
		if (jsonStorage == null || jsonStorageQuarantined) return false;

		Collection<String> keys = jsonStorage.getTimedVoteCache();
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getTimedVoteCache(key);
			if (data != null && data.isObject() && matchesStoredTimeVote(data, vote)) {
				try {
					int index = Integer.parseInt(key);
					jsonStorage.addTimedVote(index, vote);
					return verifyJsonTimeVote(index, vote);
				} catch (NumberFormatException e) {
					debug1(e);
					return false;
				} catch (RuntimeException e) {
					debug1(e);
					return false;
				}
			}
		}
		return false;
	}

	/**
	 * Removes a queued rollover vote after its normal processing completes.
	 *
	 * @param vote processed queued vote
	 * @return true when durable storage and the in-memory queue were updated
	 */
	public synchronized boolean removeTimeVote(VoteTimeQueue vote) {
		if (useMySQL) {
			if (!timedVoteCacheTable.removeVote(vote)) {
				return false;
			}
			timeChangeQueue.remove(vote);
			return true;
		}

		ArrayList<VoteTimeQueue> remaining = new ArrayList<>(timeChangeQueue);
		remaining.remove(vote);
		if (jsonStorage == null || jsonStorageQuarantined) return false;
		try {
			jsonStorage.removeTimedVotes();
			int index = 0;
			for (VoteTimeQueue queued : remaining) {
				jsonStorage.addTimedVote(index++, queued);
			}
			boolean removedDurably = VoteCacheDurability.saveAndVerifyRemoval(jsonStorage, () -> {
				Collection<String> keys = jsonStorage.getTimedVoteCache();
				if (keys == null) return true;
				for (String key : keys) {
					DataNode data = jsonStorage.getTimedVoteCache(key);
					if (data != null && data.isObject() && matchesStoredTimeVote(data, vote)) return false;
				}
				return true;
			});
			if (!removedDurably) return false;
			timeChangeQueue.remove(vote);
			return true;
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		} catch (RuntimeException e) {
			debug1(e);
			return false;
		}
	}

	/**
	 * Records completed timed-vote side effects outside the mutable queue file.
	 * This tombstone closes the crash window where both updating and deleting the
	 * queue row fail after those side effects have already committed.
	 */
	public synchronized boolean markTimeVoteCompletedDurably(VoteTimeQueue vote) {
		Path target = timeVoteCompletionPath(vote);
		if (target == null) return false;
		Path staged = null;
		try {
			Files.createDirectories(target.getParent());
			staged = Files.createTempFile(target.getParent(), ".completion-", ".tmp");
			Files.writeString(staged, timeVoteCompletionIdentity(vote), StandardCharsets.UTF_8);
			try {
				DurableFiles.publishStagedFile(staged, target);
			} catch (DurableFiles.PublishedException published) {
				// The tombstone is already visible; the read-back below is authoritative.
			}
			return hasTimeVoteCompletion(vote);
		} catch (IOException | RuntimeException failure) {
			debug1(failure);
			return false;
		} finally {
			if (staged != null) {
				try { Files.deleteIfExists(staged); } catch (IOException ignored) { }
			}
		}
	}

	public synchronized boolean hasTimeVoteCompletion(VoteTimeQueue vote) {
		Path target = timeVoteCompletionPath(vote);
		if (target == null || !Files.isRegularFile(target)) return false;
		try {
			return timeVoteCompletionIdentity(vote).equals(Files.readString(target, StandardCharsets.UTF_8));
		} catch (IOException | RuntimeException failure) {
			debug1(failure);
			return false;
		}
	}

	public synchronized void clearTimeVoteCompletion(VoteTimeQueue vote) {
		Path target = timeVoteCompletionPath(vote);
		if (target == null) return;
		try {
			DurableFiles.deleteIfExists(target);
		} catch (IOException | RuntimeException failure) {
			debug1(failure);
		}
	}

	private Path timeVoteCompletionPath(VoteTimeQueue vote) {
		if (vote == null || jsonStorage == null || jsonStorage.getStoragePath() == null) return null;
		Path storage = jsonStorage.getStoragePath().toAbsolutePath().normalize();
		Path name = storage.getFileName();
		if (name == null) return null;
		UUID key = vote.getVoteId() != null ? vote.getVoteId()
				: UUID.nameUUIDFromBytes(timeVoteCompletionIdentity(vote).getBytes(StandardCharsets.UTF_8));
		return storage.resolveSibling(name + ".completed-timed-votes").resolve(key.toString());
	}

	private String timeVoteCompletionIdentity(VoteTimeQueue vote) {
		return (vote.getVoteId() == null ? "" : vote.getVoteId()) + "\n" + vote.getUuid() + "\n"
				+ vote.getName() + "\n" + vote.getService() + "\n" + vote.getTime();
	}

	/**
	 * Loads vote cache from storage.
	 */
	public void load() {
		if (useMySQL) {
			// Load votes from MySQL
			voteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getVoteId(), voteRow.getPlayerName(),
						voteRow.getUuid(), voteRow.getService(), voteRow.getTime(), voteRow.isRealVote(),
						voteRow.getText(), voteRow.isBroadcastForwarded(), voteRow.isProxyBroadcastHandled(),
						VoteTimeQueue.decodeBroadcastForwardedServers(voteRow.getBroadcastTargets()),
						VoteTimeQueue.decodeBroadcastForwardedServers(voteRow.getBroadcastForwardedServers()),
						voteRow.isRewardDelivered(), OfflineBungeeVote.decodeHttpDeliveryIds(voteRow.getHttpDeliveryIds()),
						OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(voteRow.getHttpBroadcastDeliveryIds()));
				String server = voteRow.getServer();
				cachedVotes.putIfAbsent(server, new ArrayList<>());
				cachedVotes.get(server).add(vote);
			});

			// Load online votes from MySQL
			onlineVoteCacheTable.getAllVotes().forEach(voteRow -> {
				OfflineBungeeVote vote = new OfflineBungeeVote(voteRow.getVoteId(), voteRow.getPlayerName(),
						voteRow.getUuid(), voteRow.getService(), voteRow.getTime(), voteRow.isRealVote(),
						voteRow.getText(), voteRow.isBroadcastForwarded(), voteRow.isProxyBroadcastHandled(),
						VoteTimeQueue.decodeBroadcastForwardedServers(voteRow.getBroadcastTargets()),
						VoteTimeQueue.decodeBroadcastForwardedServers(voteRow.getBroadcastForwardedServers()),
						voteRow.isRewardDelivered(), OfflineBungeeVote.decodeHttpDeliveryIds(voteRow.getHttpDeliveryIds()),
						OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(voteRow.getHttpBroadcastDeliveryIds()));
				String player = vote.getUuid();
				cachedOnlineVotes.putIfAbsent(player, new ArrayList<>());
				cachedOnlineVotes.get(player).add(vote);
			});

			// Load timed votes from MySQL
			ArrayList<VoteTimeQueue> timedVotes = new ArrayList<>();
			timedVoteCacheTable.getAllVotes().forEach(timedVoteRow -> {
				VoteTimeQueue voteTimeQueue = new VoteTimeQueue(timedVoteRow.getVoteId(), timedVoteRow.getPlayerName(),
						timedVoteRow.getService(), timedVoteRow.getTime(), timedVoteRow.isProxyBroadcastHandled(),
						VoteTimeQueue.decodeBroadcastForwardedServers(timedVoteRow.getBroadcastTargets()),
						VoteTimeQueue.decodeBroadcastForwardedServers(timedVoteRow.getBroadcastForwardedServers()),
						timedVoteRow.getTotals(), timedVoteRow.isProcessed(), timedVoteRow.getUuid(),
						VoteTimeQueue.decodeHttpBroadcastDeliveryIds(timedVoteRow.getHttpBroadcastDeliveryIds()));
				timedVotes.add(voteTimeQueue);
			});
			timeChangeQueue.addAll(timedVotes);
			loadJsonEmergencyVotes();

		} else {
			try {
				for (String key : jsonStorage.getTimedVoteCache()) {
					DataNode data = jsonStorage.getTimedVoteCache(key);

					if (data != null && data.isObject()) {
						String name = data.has("Name") ? data.get("Name").asString() : "";
						String service = data.has("Service") ? data.get("Service").asString() : "";
						long time = data.has("Time") ? data.get("Time").asLong() : 0L;
						UUID voteId = readUuid(data, "VoteId");
						String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
						boolean proxyBroadcastHandled = data.has("ProxyBroadcastHandled")
								&& data.get("ProxyBroadcastHandled").asBoolean();
						String forwardedServers = data.has("BroadcastForwardedServers")
								? data.get("BroadcastForwardedServers").asString()
								: "";
						String broadcastTargets = data.has("BroadcastTargets")
								? data.get("BroadcastTargets").asString()
								: "";
						String totals = data.has("Totals") ? data.get("Totals").asString() : "";
						boolean processed = data.has("Processed") && data.get("Processed").asBoolean();
						String httpBroadcastDeliveryIds = data.has("HttpBroadcastDeliveryIds")
							? data.get("HttpBroadcastDeliveryIds").asString() : "";

						getTimeChangeQueue().add(new VoteTimeQueue(voteId, name, service, time, proxyBroadcastHandled,
								VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
								VoteTimeQueue.decodeBroadcastForwardedServers(forwardedServers), totals, processed, uuid,
								VoteTimeQueue.decodeHttpBroadcastDeliveryIds(httpBroadcastDeliveryIds)));
					}
				}

			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String server : jsonStorage.getServers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : jsonStorage.getServerVotes(server)) {
						DataNode data = jsonStorage.getServerVotes(server, num);

						if (data != null && data.isObject()) {

							String name = data.has("Name") ? data.get("Name").asString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
							String service = data.has("Service") ? data.get("Service").asString() : "";
							long time = data.has("Time") ? data.get("Time").asLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").asBoolean();
							String text = data.has("Text") ? data.get("Text").asString() : "";
							String voteId = readVoteId(data);
							boolean broadcastForwarded = data.has("BroadcastForwarded")
									&& data.get("BroadcastForwarded").asBoolean();
							boolean proxyBroadcastHandled = data.has("ProxyBroadcastHandled")
									&& data.get("ProxyBroadcastHandled").asBoolean();
							String broadcastTargets = data.has("BroadcastTargets")
									? data.get("BroadcastTargets").asString()
									: "";
							String broadcastForwardedServers = data.has("BroadcastForwardedServers")
									? data.get("BroadcastForwardedServers").asString()
									: "";
							String httpDeliveryIds = data.has("HttpDeliveryIds")
									? data.get("HttpDeliveryIds").asString() : "";
							String httpBroadcastDeliveryIds = data.has("HttpBroadcastDeliveryIds")
									? data.get("HttpBroadcastDeliveryIds").asString() : "";
							boolean rewardDelivered = data.has("RewardDelivered")
									&& data.get("RewardDelivered").asBoolean();

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text,
									broadcastForwarded, proxyBroadcastHandled,
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastForwardedServers),
								rewardDelivered, OfflineBungeeVote.decodeHttpDeliveryIds(httpDeliveryIds),
								OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(httpBroadcastDeliveryIds)));
						}
					}
					cachedVotes.put(server, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			try {
				for (String player : jsonStorage.getPlayers()) {
					ArrayList<OfflineBungeeVote> votes = new ArrayList<>();
					for (String num : jsonStorage.getOnlineVotes(player)) {
						DataNode data = jsonStorage.getOnlineVotes(player, num);

						if (data != null && data.isObject()) {

							String name = data.has("Name") ? data.get("Name").asString() : "";
							String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
							String service = data.has("Service") ? data.get("Service").asString() : "";
							long time = data.has("Time") ? data.get("Time").asLong() : 0L;
							boolean real = data.has("Real") && data.get("Real").asBoolean();
							String text = data.has("Text") ? data.get("Text").asString() : "";
							String voteId = readVoteId(data);
							boolean broadcastForwarded = data.has("BroadcastForwarded")
									&& data.get("BroadcastForwarded").asBoolean();
							boolean proxyBroadcastHandled = data.has("ProxyBroadcastHandled")
									&& data.get("ProxyBroadcastHandled").asBoolean();
							String broadcastTargets = data.has("BroadcastTargets")
									? data.get("BroadcastTargets").asString()
									: "";
							String broadcastForwardedServers = data.has("BroadcastForwardedServers")
									? data.get("BroadcastForwardedServers").asString()
									: "";
							String httpDeliveryIds = data.has("HttpDeliveryIds")
									? data.get("HttpDeliveryIds").asString() : "";
							String httpBroadcastDeliveryIds = data.has("HttpBroadcastDeliveryIds")
									? data.get("HttpBroadcastDeliveryIds").asString() : "";
							boolean rewardDelivered = data.has("RewardDelivered")
									&& data.get("RewardDelivered").asBoolean();

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text,
									broadcastForwarded, proxyBroadcastHandled,
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastForwardedServers),
								rewardDelivered, OfflineBungeeVote.decodeHttpDeliveryIds(httpDeliveryIds),
								OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(httpBroadcastDeliveryIds)));
						}
					}
					cachedOnlineVotes.put(player, votes);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

		}

		// log vote cache load summary

		debug1("Loaded " + cachedVotes.size() + " server vote caches.");
		int totalServerVotes = cachedVotes.values().stream().mapToInt(ArrayList::size).sum();
		debug1("Loaded " + totalServerVotes + " total server votes.");

		debug1("Loaded " + cachedOnlineVotes.size() + " online vote caches.");
		int totalOnlineVotes = cachedOnlineVotes.values().stream().mapToInt(ArrayList::size).sum();
		debug1("Loaded " + totalOnlineVotes + " total online votes.");

		debug1("Loaded " + timeChangeQueue.size() + " timed votes.");

	}

	/**
	 * Loads entries written to the JSON emergency journal while MySQL was
	 * unavailable. Entries already present in SQL are de-duplicated by vote ID (or
	 * the legacy UUID/service/time identity).
	 */
	private void loadJsonEmergencyVotes() {
		if (jsonStorage == null) {
			return;
		}
		try {
			Collection<String> servers = jsonStorage.getServers();
			if (servers != null) {
				for (String server : servers) {
					Collection<String> keys = jsonStorage.getServerVotes(server);
					if (keys == null) {
						continue;
					}
					for (String key : keys) {
						OfflineBungeeVote vote = decodeJsonVote(jsonStorage.getServerVotes(server, key));
						if (vote != null && !containsServerVote(server, vote)) {
							cachedVotes.computeIfAbsent(server, ignored -> new ArrayList<>()).add(vote);
						}
					}
				}
			}
			Collection<String> players = jsonStorage.getPlayers();
			if (players != null) {
				for (String player : players) {
					Collection<String> keys = jsonStorage.getOnlineVotes(player);
					if (keys == null) {
						continue;
					}
					for (String key : keys) {
						OfflineBungeeVote vote = decodeJsonVote(jsonStorage.getOnlineVotes(player, key));
						if (vote != null && !containsOnlineVote(player, vote)) {
							cachedOnlineVotes.computeIfAbsent(player, ignored -> new ArrayList<>()).add(vote);
						}
					}
				}
			}
		} catch (RuntimeException e) {
			debug1(e);
		}
	}

	private OfflineBungeeVote decodeJsonVote(DataNode data) {
		if (data == null || !data.isObject()) {
			return null;
		}
		String name = data.has("Name") ? data.get("Name").asString() : "";
		String uuid = data.has("UUID") ? data.get("UUID").asString() : "";
		String service = data.has("Service") ? data.get("Service").asString() : "";
		long time = data.has("Time") ? data.get("Time").asLong() : 0L;
		boolean real = data.has("Real") && data.get("Real").asBoolean();
		String text = data.has("Text") ? data.get("Text").asString() : "";
		String voteId = readVoteId(data);
		boolean broadcastForwarded = data.has("BroadcastForwarded") && data.get("BroadcastForwarded").asBoolean();
		boolean proxyBroadcastHandled = data.has("ProxyBroadcastHandled")
				&& data.get("ProxyBroadcastHandled").asBoolean();
		String broadcastTargets = data.has("BroadcastTargets") ? data.get("BroadcastTargets").asString() : "";
		String broadcastForwardedServers = data.has("BroadcastForwardedServers")
				? data.get("BroadcastForwardedServers").asString() : "";
		String httpDeliveryIds = data.has("HttpDeliveryIds") ? data.get("HttpDeliveryIds").asString() : "";
		String httpBroadcastDeliveryIds = data.has("HttpBroadcastDeliveryIds")
				? data.get("HttpBroadcastDeliveryIds").asString() : "";
		boolean rewardDelivered = data.has("RewardDelivered") && data.get("RewardDelivered").asBoolean();
		return new OfflineBungeeVote(voteId, name, uuid, service, time, real, text, broadcastForwarded,
				proxyBroadcastHandled, VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
				VoteTimeQueue.decodeBroadcastForwardedServers(broadcastForwardedServers), rewardDelivered,
				OfflineBungeeVote.decodeHttpDeliveryIds(httpDeliveryIds),
				OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(httpBroadcastDeliveryIds));
	}

	private final boolean useMySQL;
	private ProxyVoteCacheTable voteCacheTable;
	private ProxyTimedVoteCacheTable timedVoteCacheTable;
	private ProxyOnlineVoteCacheTable onlineVoteCacheTable;

	private IVoteCache jsonStorage;

	/**
	 * Logs an info message.
	 * @param msg the message to log
	 */
	public abstract void logInfo1(String msg);

	/**
	 * Logs a severe message.
	 * @param msg the message to log
	 */
	public abstract void logSevere1(String msg);

	/**
	 * Logs a debug exception.
	 * @param e the exception to log
	 */
	public abstract void debug1(Exception e);

	/**
	 * Logs a debug throwable.
	 * @param e the throwable to log
	 */
	public abstract void debug1(Throwable e);

	/**
	 * Logs a debug message.
	 * @param msg the message to log
	 */
	public abstract void debug1(String msg);

	/**
	 * Constructs a new vote cache handler.
	 * @param mysqlConfig MySQL configuration
	 * @param useMySQL whether to use MySQL
	 * @param useExistingConnection whether to use an existing connection
	 * @param mysql existing MySQL connection
	 * @param debug whether debug mode is enabled
	 * @param jsonStorage JSON storage implementation
	 */
	public VoteCacheHandler(MysqlConfig mysqlConfig, boolean useMySQL, boolean useExistingConnection, MySQL mysql,
			boolean debug, IVoteCache jsonStorage) {
		this.useMySQL = useMySQL;
		// Keep the JSON store available as a durable emergency journal even when
		// MySQL is the primary cache backend.
		this.jsonStorage = jsonStorage;

		if (useMySQL) {
			if (useExistingConnection) {
				voteCacheTable = new ProxyVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};

				timedVoteCacheTable = new ProxyTimedVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};

				onlineVoteCacheTable = new ProxyOnlineVoteCacheTable(mysql, mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};
			} else {
				voteCacheTable = new ProxyVoteCacheTable(mysqlConfig, debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};

				timedVoteCacheTable = new ProxyTimedVoteCacheTable(voteCacheTable.getMysql(),
						mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};

				onlineVoteCacheTable = new ProxyOnlineVoteCacheTable(voteCacheTable.getMysql(),
						mysqlConfig.getTablePrefix(), debug) {
					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Throwable t) {
						if (debug)
							debug1(t);
					}

					@Override
					public void debug(String text) {
						debug1(text);
					}
				};
			}
		}
	}

	/**
	 * Gets all servers with cached votes.
	 * @return array of server names
	 */
	public String[] getCachedVotesServers() {
		return cachedVotes.keySet().toArray(new String[0]);
	}

	/**
	 * Removes specific votes from a server cache.
	 * @param server the server name
	 * @param removed list of votes to remove
	 */
	public synchronized void removeServerVotes(String server, ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : removed) {
			boolean mysqlRemoved = !useMySQL || voteCacheTable.tryRemoveVote(vote, server);
			boolean jsonRemoved = jsonStorage == null;
			if (jsonStorage != null && !jsonStorageQuarantined) {
				jsonRemoved = removeJsonServerVoteDurably(server, vote);
			}
			boolean removedDurably = mysqlRemoved && jsonRemoved;
			if (!removedDurably) continue;
			ArrayList<OfflineBungeeVote> serverVotes = cachedVotes.get(server);
			if (serverVotes != null) {
				serverVotes.removeIf(candidate -> sameVoteIdentity(candidate, vote));
				if (serverVotes.isEmpty()) cachedVotes.remove(server);
			}
		}
	}

	protected boolean removeJsonServerVoteDurably(String server, OfflineBungeeVote vote) {
		jsonStorage.removeVote(server, vote);
		try {
			return VoteCacheDurability.saveAndVerifyRemoval(jsonStorage,
					() -> !containsStoredServerVote(server, vote));
		} catch (VoteCacheDurability.ReloadFailedException failure) {
			jsonStorageQuarantined = true;
			debug1(failure);
			return false;
		}
	}

	private boolean containsStoredServerVote(String server, OfflineBungeeVote expected) {
		Collection<String> keys = jsonStorage.getServerVotes(server);
		if (keys == null) return false;
		for (String key : keys) {
			DataNode data = jsonStorage.getServerVotes(server, key);
			if (data != null && matchesStoredVote(data, expected)) return true;
		}
		return false;
	}

	/**
	 * Removes specific votes from the online vote cache.
	 * @param removed list of votes to remove
	 */
	public void removeOnlineVotes(ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : new ArrayList<>(removed)) {
			tryRemoveOnlineVote(vote.getUuid(), vote);
		}
	}

}
