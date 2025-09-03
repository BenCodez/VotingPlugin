package com.bencodez.votingplugin.proxy.cache;

import java.util.Collection;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public interface IVoteCache {

	void addTimedVote(int num, VoteTimeQueue voteTimedQueue);

	void addVote(String server, int num, OfflineBungeeVote voteData);

	void addVoteOnline(String player, int num, OfflineBungeeVote voteData);

	void clearData();

	Collection<String> getOnlineVotes(String name);

	DataNode getOnlineVotes(String name, String num);

	Collection<String> getPlayers();

	Collection<String> getServers();

	Collection<String> getServerVotes(String server);

	DataNode getServerVotes(String server, String num);

	Collection<String> getTimedVoteCache();

	DataNode getTimedVoteCache(String key);

	int getVotePartyCache(String server);

	int getVotePartyCurrentVotes();

	int getVotePartyInreaseVotesRequired();

	void setVotePartyCache(String server, int amount);

	void setVotePartyCurrentVotes(int amount);

	void setVotePartyInreaseVotesRequired(int amount);

	void save();

	void reload();
}
