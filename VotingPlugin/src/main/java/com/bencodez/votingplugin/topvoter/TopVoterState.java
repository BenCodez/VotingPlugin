package com.bencodez.votingplugin.topvoter;

import java.time.YearMonth;
import java.util.LinkedHashMap;

/**
 * Owns the mutable ranking state used by the top-voter subsystem.
 */
public class TopVoterState {

	private LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> topVoters = new LinkedHashMap<>();
	private LinkedHashMap<TopVoterPlayer, Integer> lastMonthTopVoters = new LinkedHashMap<>();
	private LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previousMonthsTopVoters = new LinkedHashMap<>();

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
}
