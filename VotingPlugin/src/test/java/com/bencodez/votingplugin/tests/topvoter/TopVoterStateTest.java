package com.bencodez.votingplugin.tests.topvoter;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.YearMonth;
import java.util.LinkedHashMap;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.topvoter.TopVoterState;

public class TopVoterStateTest {

	@Test
	public void testDefaultsAreUsable() {
		TopVoterState state = new TopVoterState();

		assertNotNull(state.getTopVoters());
		assertNotNull(state.getLastMonthTopVoters());
		assertNotNull(state.getPreviousMonthsTopVoters());
		assertTrue(state.getTopVoters(TopVoter.Monthly).isEmpty());
	}

	@Test
	public void testStoresRankingMapsWithoutCopying() {
		TopVoterState state = new TopVoterState();
		LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> rankings = new LinkedHashMap<>();
		LinkedHashMap<TopVoterPlayer, Integer> lastMonth = new LinkedHashMap<>();
		LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previous = new LinkedHashMap<>();

		state.setTopVoters(rankings);
		state.setLastMonthTopVoters(lastMonth);
		state.setPreviousMonthsTopVoters(previous);

		assertSame(rankings, state.getTopVoters());
		assertSame(lastMonth, state.getLastMonthTopVoters());
		assertSame(previous, state.getPreviousMonthsTopVoters());
	}

	@Test
	public void testNullAssignmentsResetToEmptyMaps() {
		TopVoterState state = new TopVoterState();

		state.setTopVoters(null);
		state.setLastMonthTopVoters(null);
		state.setPreviousMonthsTopVoters(null);

		assertTrue(state.getTopVoters().isEmpty());
		assertTrue(state.getLastMonthTopVoters().isEmpty());
		assertTrue(state.getPreviousMonthsTopVoters().isEmpty());
	}
}
