package com.bencodez.votingplugin.topvoter;

import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.HashMap;
import java.util.LinkedHashMap;

import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * Owns the mutable ranking state used by the top-voter subsystem.
 */
public class TopVoterState {

	private LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> topVoters;
	private LinkedHashMap<TopVoterPlayer, Integer> lastMonthTopVoters;
	private LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previousMonthsTopVoters;
	private LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday;

	public TopVoterState() {
		reset();
	}

	public void reset() {
		topVoters = new LinkedHashMap<>();
		for (TopVoter topVoter : TopVoter.values()) {
			topVoters.put(topVoter, new LinkedHashMap<>());
		}
		lastMonthTopVoters = new LinkedHashMap<>();
		previousMonthsTopVoters = new LinkedHashMap<>();
		voteToday = new LinkedHashMap<>();
	}

	public LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> getTopVoters() {
		return topVoters;
	}

	public void setTopVoters(LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> topVoters) {
		this.topVoters = topVoters != null ? topVoters : new LinkedHashMap<>();
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getTopVoters(TopVoter topVoter) {
		LinkedHashMap<TopVoterPlayer, Integer> result = topVoters.get(topVoter);
		return result != null ? result : new LinkedHashMap<>();
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getLastMonthTopVoters() {
		return lastMonthTopVoters;
	}

	public void setLastMonthTopVoters(LinkedHashMap<TopVoterPlayer, Integer> lastMonthTopVoters) {
		this.lastMonthTopVoters = lastMonthTopVoters != null ? lastMonthTopVoters : new LinkedHashMap<>();
	}

	public LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> getPreviousMonthsTopVoters() {
		return previousMonthsTopVoters;
	}

	public void setPreviousMonthsTopVoters(
			LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previousMonthsTopVoters) {
		this.previousMonthsTopVoters = previousMonthsTopVoters != null ? previousMonthsTopVoters : new LinkedHashMap<>();
	}

	public LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> getVoteToday() {
		return voteToday;
	}

	public void setVoteToday(LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday) {
		this.voteToday = voteToday != null ? voteToday : new LinkedHashMap<>();
	}
}
