package com.bencodez.votingplugin.proxy.cache;

import java.util.Collection;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import com.bencodez.votingplugin.util.DurableFiles;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/**
 * Interface for vote caching operations.
 */
public interface IVoteCache {

	static final String EMERGENCY_JOURNAL_MARKER_FILE_SUFFIX = ".vote-emergency-journal";
	static final String EMERGENCY_JOURNAL_MARKER_VERSION = "1";

	/**
	 * Returns the marker file used for emergency-journal recovery coordination.
	 *
	 * @return marker file path, or {@code null} when no file-backed cache exists
	 */
	default Path getEmergencyJournalMarkerPath() {
		Path storagePath = getStoragePath();
		if (storagePath == null) {
			return null;
		}
		return storagePath.resolveSibling(storagePath.getFileName() + EMERGENCY_JOURNAL_MARKER_FILE_SUFFIX);
	}

	/**
	 * Returns whether this JSON cache has a marker that indicates emergency journal
	 * compatibility for MySQL recovery. Legacy files without this marker are not
	 * treated as emergency recoverables.
	 *
	 * @return true when the marker file exists
	 */
	default boolean hasEmergencyJournalMarker() {
		Path markerPath = getEmergencyJournalMarkerPath();
		if (markerPath == null) {
			return false;
		}
		try {
			return Files.isRegularFile(markerPath, LinkOption.NOFOLLOW_LINKS) && !Files.isSymbolicLink(markerPath)
					&& Files.size(markerPath) <= 8
					&& EMERGENCY_JOURNAL_MARKER_VERSION.equals(Files.readString(markerPath, StandardCharsets.UTF_8).trim());
		} catch (IOException ignored) {
			return false;
		}
	}

	/**
	 * Writes the emergency-journal marker file used to indicate that this cache has
	 * participated in emergency journal recovery flow.
	 *
	 * @throws IOException when marker publication fails
	 */
	default void markEmergencyJournalUsed() throws IOException {
		Path markerPath = getEmergencyJournalMarkerPath();
		if (markerPath == null) {
			throw new IOException("Vote cache has no emergency journal marker path");
		}
		Path normalizedMarker = markerPath.toAbsolutePath().normalize();
		Path parent = normalizedMarker.getParent();
		if (parent != null && Files.isSymbolicLink(parent)) {
			throw new IOException("Emergency journal marker parent is a symbolic link");
		}
		if (Files.isSymbolicLink(normalizedMarker)) {
			throw new IOException("Emergency journal marker is a symbolic link");
		}
		if (parent != null) {
			Files.createDirectories(parent);
		}
		Path staged = Files.createTempFile(parent, normalizedMarker.getFileName().toString(), ".journal");
		try {
			Files.writeString(staged, EMERGENCY_JOURNAL_MARKER_VERSION, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.publishStagedFile(staged, normalizedMarker);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	/**
	 * Returns the file backing this JSON cache, when one exists.
	 *
	 * @return backing file path, or {@code null} for non-file implementations
	 */
	default Path getStoragePath() {
		return null;
	}

	/** Persists the complete cache and forces it to stable storage. */
	default void saveDurably() throws IOException {
		Path path = getStoragePath();
		if (path == null) throw new IOException("Vote cache has no durable storage path");
		save();
		DurableFiles.forceFile(path);
		try {
			DurableFiles.forceDirectory(path.toAbsolutePath().normalize().getParent());
		} catch (IOException failure) {
			throw new DurableFiles.PublishedException(failure);
		}
	}

	/**
	 * Adds a timed vote to the cache.
	 *
	 * @param num the vote number
	 * @param voteTimedQueue the vote time queue
	 */
	void addTimedVote(int num, VoteTimeQueue voteTimedQueue);

	/**
	 * Adds a vote for a specific server.
	 *
	 * @param server the server name
	 * @param num the vote number
	 * @param voteData the vote data
	 */
	void addVote(String server, int num, OfflineBungeeVote voteData);

	/**
	 * Adds a vote for an online player.
	 *
	 * @param player the player name
	 * @param num the vote number
	 * @param voteData the vote data
	 */
	void addVoteOnline(String player, int num, OfflineBungeeVote voteData);

	/**
	 * Clears all cached data.
	 */
	void clearData();

	/**
	 * Gets all online votes for a player.
	 *
	 * @param name the player name
	 * @return collection of vote identifiers
	 */
	Collection<String> getOnlineVotes(String name);

	/**
	 * Gets a specific online vote for a player.
	 *
	 * @param name the player name
	 * @param num the vote number
	 * @return the vote data node
	 */
	DataNode getOnlineVotes(String name, String num);

	/**
	 * Gets all cached players.
	 *
	 * @return collection of player names
	 */
	Collection<String> getPlayers();

	/**
	 * Gets all cached servers.
	 *
	 * @return collection of server names
	 */
	Collection<String> getServers();

	/**
	 * Gets all votes for a server.
	 *
	 * @param server the server name
	 * @return collection of vote identifiers
	 */
	Collection<String> getServerVotes(String server);

	/**
	 * Gets a specific vote for a server.
	 *
	 * @param server the server name
	 * @param num the vote number
	 * @return the vote data node
	 */
	DataNode getServerVotes(String server, String num);

	/**
	 * Gets all timed vote cache entries.
	 *
	 * @return collection of cache keys
	 */
	Collection<String> getTimedVoteCache();

	/**
	 * Gets a specific timed vote cache entry.
	 *
	 * @param key the cache key
	 * @return the vote data node
	 */
	DataNode getTimedVoteCache(String key);

	/**
	 * Removes all persisted timed vote entries.
	 */
	void removeTimedVotes();

	/**
	 * Gets the vote party cache for a server.
	 *
	 * @param server the server name
	 * @return the cached vote count
	 */
	int getVotePartyCache(String server);

	/** Returns backend IDs with persisted, undelivered vote-party rewards. */
	Collection<String> getPendingVotePartyRewardServers();

	Collection<String> getPendingVotePartyRewardIds(String server);

	PendingVotePartyProxyEffects getPendingVotePartyProxyEffects();

	PendingVotePartyProxyEffects getQuarantinedVotePartyProxyEffects();

	/**
	 * Gets the current vote party votes.
	 *
	 * @return the current vote count
	 */
	int getVotePartyCurrentVotes();

	/**
	 * Gets the vote party increase votes required.
	 *
	 * @return the increase amount
	 */
	int getVotePartyInreaseVotesRequired();

	/**
	 * Sets the vote party cache for a server.
	 *
	 * @param server the server name
	 * @param amount the vote amount
	 */
	void setVotePartyCache(String server, int amount);

	void setPendingVotePartyReward(String server, String deliveryId, boolean pending);

	void setPendingVotePartyProxyEffects(PendingVotePartyProxyEffects effects);

	void setQuarantinedVotePartyProxyEffects(PendingVotePartyProxyEffects effects);

	/**
	 * Sets the current vote party votes.
	 *
	 * @param amount the vote amount
	 */
	void setVotePartyCurrentVotes(int amount);

	/**
	 * Sets the vote party increase votes required.
	 *
	 * @param amount the increase amount
	 */
	void setVotePartyInreaseVotesRequired(int amount);

	/**
	 * Saves the cache data.
	 */
	void save();

	/**
	 * Reloads the cache data.
	 */
	void reload();

	/**
	 * Removes all online votes for a player.
	 *
	 * @param player the player name
	 */
	void removeOnlineVotes(String player);

	/**
	 * Removes all votes for a server.
	 *
	 * @param server the server name
	 */
	void removeServerVotes(String server);

	/**
	 * Removes a specific vote for a server.
	 *
	 * @param server the server name
	 * @param uuid the vote UUID
	 */
	void removeServerVote(String server, String uuid);

	/**
	 * Removes a vote from a server.
	 *
	 * @param server the server name
	 * @param vote the vote to remove
	 */
	void removeVote(String server, OfflineBungeeVote vote);

	/**
	 * Removes an online vote.
	 *
	 * @param vote the vote to remove
	 */
	void removeOnlineVote(OfflineBungeeVote vote);
}
