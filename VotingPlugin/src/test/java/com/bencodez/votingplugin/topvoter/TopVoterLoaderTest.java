package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.YearMonth;
import java.util.ArrayList;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class TopVoterLoaderTest {

	@Test
	public void parsesStoredMonthColumns() {
		assertEquals(YearMonth.of(2025, 1), TopVoterLoader.parseMonthColumn("MonthTotal-JANUARY-2025"));
		assertEquals(YearMonth.of(2026, 12), TopVoterLoader.parseMonthColumn("MonthTotal-DECEMBER-2026"));
	}

	@Test
	public void rejectsMalformedMonthColumns() {
		assertNull(TopVoterLoader.parseMonthColumn("MonthTotal-BAD-2025"));
		assertNull(TopVoterLoader.parseMonthColumn("MonthTotal-JANUARY-nope"));
		assertNull(TopVoterLoader.parseMonthColumn("Other-JANUARY-2025"));
		assertNull(TopVoterLoader.parseMonthColumn(null));
	}

	@Test
	@SuppressWarnings("unchecked")
	void boundaryRankingUsesCopiedTotalInsteadOfCurrentPeriodTotal() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		com.bencodez.votingplugin.user.UserManager votingUserManager =
				mock(com.bencodez.votingplugin.user.UserManager.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		TopVoterPlayer player = new TopVoterPlayer(uuid, "first", 1L);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUserManager);
		when(votingUserManager.getVotingPluginUser(uuid, false)).thenReturn(user);
		when(user.getLastDailyTotal()).thenReturn(20);
		when(user.getTotal(TopVoter.Daily)).thenReturn(99);
		when(user.getTopVoterPlayer()).thenReturn(player);
		doAnswer(invocation -> {
			BiConsumer<UUID, ArrayList<Column>> perUser = invocation.getArgument(0);
			Consumer<Integer> finished = invocation.getArgument(1);
			perUser.accept(uuid, new ArrayList<>());
			finished.accept(1);
			return null;
		}).when(userManager).forEachUserKeys(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.any());

		assertEquals(20, new TopVoterLoader(plugin).getBoundaryTopVoters(TopVoter.Daily).get(player));
	}
}
