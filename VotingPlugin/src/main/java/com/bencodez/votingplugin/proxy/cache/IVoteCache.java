package com.bencodez.votingplugin.proxy.cache;

import java.util.Collection;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/**
 * Interface for vote caching operations.
 */
public interface IVoteCache {

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
	 * Gets the vote party cache for a server.
	 *
	 * @param server the server name
	 * @return the cached vote count
	 */
	int getVotePartyCache(String server);

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
