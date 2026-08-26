package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.List;

import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;

/** Owns persistent VoteParty totals, participant state, and per-user counters. */
public final class VotePartyState {

	private static final String TOTAL_PATH = "VoteParty.Total";
	private static final String VOTED_PATH = "VoteParty.Voted";
	private static final String USER_COUNT_KEY = "VotePartyVotes";

	private final VotingPluginMain plugin;

	public VotePartyState(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public int getTotalVotes() {
		return plugin.getServerData().getData().getInt(TOTAL_PATH);
	}

	public void setTotalVotes(int value) {
		plugin.getServerData().getData().set(TOTAL_PATH, value);
		plugin.getServerData().saveData();
	}

	public List<String> getVotedUsers() {
		List<String> users = plugin.getServerData().getData().getStringList(VOTED_PATH);
		return users == null ? new ArrayList<>() : users;
	}

	public void setVotedUsers(List<String> users) {
		plugin.getServerData().getData().set(VOTED_PATH, users);
		plugin.getServerData().saveData();
	}

	public int getVotesRequired() {
		int required = plugin.getSpecialRewardsConfig().getVotePartyVotesRequired();
		int extra = plugin.getServerData().getVotePartyExtraRequired();
		return extra > 0 ? required + extra : required;
	}

	public int getNeededVotes() {
		return getVotesRequired() - getTotalVotes();
	}

	public void reset(boolean resetTotal) {
		if (resetTotal) {
			setTotalVotes(0);
		}
		setVotedUsers(new ArrayList<>());
		resetUserCounts();
	}

	public void resetUserCounts() {
		plugin.getUserManager().removeAllKeyValues(USER_COUNT_KEY, DataType.INTEGER);
	}
}
