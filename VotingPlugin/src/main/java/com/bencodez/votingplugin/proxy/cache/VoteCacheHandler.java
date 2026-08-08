package com.bencodez.votingplugin.proxy.cache;

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

import lombok.Getter;

/**
 * Handles caching of votes for proxy servers.
 */
public abstract class VoteCacheHandler {

	/**
	 * Queue of timed votes for time change processing.
	 */
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	// uuid based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedOnlineVotes = new ConcurrentHashMap<>();

	// server based
	private ConcurrentHashMap<String, ArrayList<OfflineBungeeVote>> cachedVotes = new ConcurrentHashMap<>();

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
		if (containsServerVote(server, vote.getVoteId())) {
			debug1("Not caching duplicate vote " + vote.getVoteId() + " for server " + server);
			return;
		}

		cachedVotes.putIfAbsent(server, new ArrayList<>());
		cachedVotes.get(server).add(vote);

		if (useMySQL) {
			voteCacheTable.insertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText(), vote.isBroadcastForwarded(),
					vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.isRewardDelivered(), server);
		} else {
			// IMPORTANT: index must come from JSON, not from cachedVotes (cache can be out
			// of sync with JSON)
			int idx = jsonStorage.getServerVotes(server).size();
			jsonStorage.addVote(server, idx, vote);
			jsonStorage.save();
		}
	}

	/**
	 * Persists updated delivery state for an existing server-cached vote.
	 *
	 * @param server backend server owning the cached reward
	 * @param vote cached vote with updated delivery state
	 */
	public synchronized void updateServerVote(String server, OfflineBungeeVote vote) {
		if (useMySQL) {
			voteCacheTable.updateProxyBroadcastState(vote, server);
			return;
		}

		Collection<String> keys = jsonStorage.getServerVotes(server);
		if (keys == null) {
			return;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getServerVotes(server, key);
			if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service")
					|| !data.has("Time")) {
				continue;
			}
			if (matchesStoredVote(data, vote)) {
				try {
					jsonStorage.addVote(server, Integer.parseInt(key), vote);
					jsonStorage.save();
				} catch (NumberFormatException e) {
					debug1(e);
				}
				return;
			}
		}
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

			if (useMySQL) {
				voteCacheTable.removeVotesByServerAndUUID(server, uuid);
			} else {
				jsonStorage.removeServerVote(server, uuid);
				jsonStorage.save();
			}
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
		} else {
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
		if (containsOnlineVote(uuid, vote.getVoteId())) {
			debug1("Not caching duplicate online vote " + vote.getVoteId() + " for " + uuid);
			return;
		}

		cachedOnlineVotes.putIfAbsent(uuid, new ArrayList<>());
		cachedOnlineVotes.get(uuid).add(vote);

		if (useMySQL) {
			onlineVoteCacheTable.insertVote(vote.getVoteId(), vote.getUuid(), vote.getPlayerName(), vote.getService(),
					vote.getTime(), vote.isRealVote(), vote.getText(), vote.isBroadcastForwarded(),
					vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.isRewardDelivered());
		} else {
			// IMPORTANT: index must come from JSON, not from cachedOnlineVotes (cache can
			// be out of sync with JSON)
			int idx = jsonStorage.getOnlineVotes(uuid).size();
			jsonStorage.addVoteOnline(uuid, idx, vote);
			jsonStorage.save();
		}
	}

	/**
	 * Persists updated delivery state for an existing voter-keyed cached vote.
	 *
	 * @param uuid voter cache key
	 * @param vote cached vote with updated delivery state
	 */
	public synchronized void updateOnlineVote(String uuid, OfflineBungeeVote vote) {
		if (useMySQL) {
			onlineVoteCacheTable.updateProxyBroadcastState(vote);
			return;
		}

		Collection<String> keys = jsonStorage.getOnlineVotes(uuid);
		if (keys == null) {
			return;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getOnlineVotes(uuid, key);
			if (data == null || !data.isObject() || !data.has("UUID") || !data.has("Service")
					|| !data.has("Time")) {
				continue;
			}
			if (matchesStoredVote(data, vote)) {
				try {
					jsonStorage.addVoteOnline(uuid, Integer.parseInt(key), vote);
					jsonStorage.save();
				} catch (NumberFormatException e) {
					debug1(e);
				}
				return;
			}
		}
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

		ArrayList<OfflineBungeeVote> retained = new ArrayList<>();
		for (OfflineBungeeVote vote : votes) {
			if (vote.isProxyBroadcastHandled() && !vote.isProxyBroadcastComplete()) {
				vote.setRewardDelivered(true);
				retained.add(vote);
			}
		}

		removeOnlineVotes(uuid);
		for (OfflineBungeeVote vote : retained) {
			addOnlineVote(uuid, vote);
		}
	}

	/**
	 * Removes one voter-keyed cached vote by its stable vote identity.
	 *
	 * @param uuid voter cache key
	 * @param removedVote vote to remove
	 */
	public synchronized void removeOnlineVote(String uuid, OfflineBungeeVote removedVote) {
		ArrayList<OfflineBungeeVote> votes = cachedOnlineVotes.get(uuid);
		if (votes == null || votes.isEmpty()) {
			return;
		}

		ArrayList<OfflineBungeeVote> retained = new ArrayList<>();
		for (OfflineBungeeVote vote : votes) {
			if (!sameVoteIdentity(vote, removedVote)) {
				retained.add(vote);
			}
		}

		removeOnlineVotes(uuid);
		for (OfflineBungeeVote vote : retained) {
			addOnlineVote(uuid, vote);
		}
	}

	/**
	 * Removes all cached online votes for a player.
	 * @param uuid the player UUID
	 */
	public void removeOnlineVotes(String uuid) {
		cachedOnlineVotes.remove(uuid);
		if (useMySQL) {
			onlineVoteCacheTable.removeVotesByUuid(uuid);
		} else {
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

	/**
	 * Checks whether a vote is already cached for a server.
	 *
	 * @param server target server
	 * @param voteId unique vote identifier
	 * @return true if the vote is already cached for the server
	 */
	private boolean containsServerVote(String server, UUID voteId) {
		if (voteId == null) {
			return false;
		}
		for (OfflineBungeeVote vote : getVotes(server)) {
			if (voteId.equals(vote.getVoteId())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Checks whether a vote is already cached for an online player.
	 *
	 * @param uuid player UUID
	 * @param voteId unique vote identifier
	 * @return true if the vote is already cached for the player
	 */
	private boolean containsOnlineVote(String uuid, UUID voteId) {
		if (voteId == null) {
			return false;
		}
		for (OfflineBungeeVote vote : getOnlineVotes(uuid)) {
			if (voteId.equals(vote.getVoteId())) {
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
		if (!useMySQL) {
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
			boolean stored = timedVoteCacheTable.insertTimedVote(vote.getVoteId(), vote.getName(), vote.getService(),
					vote.getTime(), vote.isProxyBroadcastHandled(), vote.encodeBroadcastTargets(),
					vote.encodeBroadcastForwardedServers(), vote.getTotals(), vote.isProcessed());
			if (!stored) {
				timeChangeQueue.remove(vote);
			}
			return stored;
		}

		try {
			Collection<String> keys = jsonStorage.getTimedVoteCache();
			int index = 0;
			while (keys != null && keys.contains(String.valueOf(index))) {
				index++;
			}
			jsonStorage.addTimedVote(index, vote);
			jsonStorage.save();
			return true;
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

		Collection<String> keys = jsonStorage.getTimedVoteCache();
		if (keys == null) {
			return false;
		}
		for (String key : keys) {
			DataNode data = jsonStorage.getTimedVoteCache(key);
			if (data != null && data.isObject() && matchesStoredTimeVote(data, vote)) {
				try {
					jsonStorage.addTimedVote(Integer.parseInt(key), vote);
					jsonStorage.save();
					return true;
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
		try {
			jsonStorage.removeTimedVotes();
			int index = 0;
			for (VoteTimeQueue queued : remaining) {
				jsonStorage.addTimedVote(index++, queued);
			}
			jsonStorage.save();
			timeChangeQueue.remove(vote);
			return true;
		} catch (RuntimeException e) {
			debug1(e);
			return false;
		}
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
						voteRow.isRewardDelivered());
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
						voteRow.isRewardDelivered());
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
						timedVoteRow.getTotals(), timedVoteRow.isProcessed());
				timedVotes.add(voteTimeQueue);
			});
			timeChangeQueue.addAll(timedVotes);

		} else {
			try {
				for (String key : jsonStorage.getTimedVoteCache()) {
					DataNode data = jsonStorage.getTimedVoteCache(key);

					if (data != null && data.isObject()) {
						String name = data.has("Name") ? data.get("Name").asString() : "";
						String service = data.has("Service") ? data.get("Service").asString() : "";
						long time = data.has("Time") ? data.get("Time").asLong() : 0L;
						UUID voteId = readUuid(data, "VoteId");
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

						getTimeChangeQueue().add(new VoteTimeQueue(voteId, name, service, time, proxyBroadcastHandled,
								VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
								VoteTimeQueue.decodeBroadcastForwardedServers(forwardedServers), totals, processed));
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
							boolean rewardDelivered = data.has("RewardDelivered")
									&& data.get("RewardDelivered").asBoolean();

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text,
									broadcastForwarded, proxyBroadcastHandled,
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastForwardedServers),
									rewardDelivered));
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
							boolean rewardDelivered = data.has("RewardDelivered")
									&& data.get("RewardDelivered").asBoolean();

							votes.add(new OfflineBungeeVote(voteId, name, uuid, service, time, real, text,
									broadcastForwarded, proxyBroadcastHandled,
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastTargets),
									VoteTimeQueue.decodeBroadcastForwardedServers(broadcastForwardedServers),
									rewardDelivered));
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
		} else {
			this.jsonStorage = jsonStorage;
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
	public void removeServerVotes(String server, ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : removed) {
			for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedVotes.entrySet()) {
				if (entry.getKey().equals(server)) {
					entry.getValue().removeIf(v -> v.getUuid().equals(vote.getUuid())
							&& v.getService().equals(vote.getService()) && v.getTime() == vote.getTime());
				}
			}
			if (useMySQL) {
				voteCacheTable.removeVote(vote, server);
			} else {
				jsonStorage.removeVote(server, vote);
				jsonStorage.save();
			}
		}
	}

	/**
	 * Removes specific votes from the online vote cache.
	 * @param removed list of votes to remove
	 */
	public void removeOnlineVotes(ArrayList<OfflineBungeeVote> removed) {
		for (OfflineBungeeVote vote : removed) {
			for (Map.Entry<String, ArrayList<OfflineBungeeVote>> entry : cachedOnlineVotes.entrySet()) {
				entry.getValue().removeIf(v -> v.getUuid().equals(vote.getUuid())
						&& v.getService().equals(vote.getService()) && v.getTime() == vote.getTime());
			}
			if (useMySQL) {
				onlineVoteCacheTable.removeVote(vote);
			} else {
				jsonStorage.removeOnlineVote(vote);
				jsonStorage.save();
			}
		}
	}

}
