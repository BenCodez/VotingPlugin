package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.YearMonth;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
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
		Config config = mock(Config.class);
		UUID uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		TopVoterPlayer player = new TopVoterPlayer(uuid, "first", 1L);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUserManager);
		when(plugin.getConfigFile()).thenReturn(config);
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

	@Test
	@SuppressWarnings("unchecked")
	void boundaryRankingKeepsOnlyTheConfiguredBestPlayers() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		com.bencodez.votingplugin.user.UserManager votingUserManager =
				mock(com.bencodez.votingplugin.user.UserManager.class);
		Config config = mock(Config.class);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUserManager);
		when(plugin.getConfigFile()).thenReturn(config);
		when(config.getMaxiumNumberOfTopVotersToLoad()).thenReturn(2);
		UUID first = UUID.fromString("00000000-0000-0000-0000-000000000001");
		UUID second = UUID.fromString("00000000-0000-0000-0000-000000000002");
		UUID third = UUID.fromString("00000000-0000-0000-0000-000000000003");
		VotingPluginUser firstUser = boundaryUser(first, "first", 10, votingUserManager);
		VotingPluginUser secondUser = boundaryUser(second, "second", 30, votingUserManager);
		VotingPluginUser thirdUser = boundaryUser(third, "third", 20, votingUserManager);
		doAnswer(invocation -> {
			BiConsumer<UUID, ArrayList<Column>> perUser = invocation.getArgument(0);
			Consumer<Integer> finished = invocation.getArgument(1);
			perUser.accept(first, new ArrayList<>());
			perUser.accept(second, new ArrayList<>());
			perUser.accept(third, new ArrayList<>());
			finished.accept(3);
			return null;
		}).when(userManager).forEachUserKeys(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.any());

		TopVoterLoader.BoundaryRanking boundary =
				new TopVoterLoader(plugin).getBoundaryRanking(TopVoter.Daily, null);
		LinkedHashMap<TopVoterPlayer, Integer> ranking = boundary.players();

		assertEquals(java.util.List.of(secondUser.getTopVoterPlayer(), thirdUser.getTopVoterPlayer()),
				new ArrayList<>(ranking.keySet()));
		assertEquals(java.util.List.of(30, 20), new ArrayList<>(ranking.values()));
		assertEquals(60, boundary.combinedTotal());
	}

	@Test
	@SuppressWarnings("unchecked")
	void boundaryRankingExcludesBannedBlacklistedAndIgnoredPlayers() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		com.bencodez.votingplugin.user.UserManager votingUserManager =
				mock(com.bencodez.votingplugin.user.UserManager.class);
		Config config = mock(Config.class);
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUserManager);
		when(plugin.getConfigFile()).thenReturn(config);
		UUID banned = UUID.fromString("00000000-0000-0000-0000-000000000001");
		UUID blacklisted = UUID.fromString("00000000-0000-0000-0000-000000000002");
		UUID ignored = UUID.fromString("00000000-0000-0000-0000-000000000003");
		VotingPluginUser bannedUser = boundaryUser(banned, "banned", 30, votingUserManager);
		VotingPluginUser blacklistedUser = boundaryUser(blacklisted, "blacklisted", 20, votingUserManager);
		VotingPluginUser ignoredUser = boundaryUser(ignored, "ignored", 10, votingUserManager);
		when(bannedUser.isBanned()).thenReturn(true);
		when(blacklistedUser.getPlayerName()).thenReturn("blacklisted");
		when(ignoredUser.isTopVoterIgnore()).thenReturn(true);
		doAnswer(invocation -> {
			BiConsumer<UUID, ArrayList<Column>> perUser = invocation.getArgument(0);
			perUser.accept(banned, new ArrayList<>());
			perUser.accept(blacklisted, new ArrayList<>());
			perUser.accept(ignored, new ArrayList<>());
			return null;
		}).when(userManager).forEachUserKeys(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.any());

		TopVoterLoader.BoundaryRanking ranking = new TopVoterLoader(plugin).getBoundaryRanking(
				TopVoter.Daily, null, true, java.util.List.of("blacklisted"));

		assertEquals(0, ranking.players().size());
		assertEquals(0, ranking.combinedTotal());
	}

	private VotingPluginUser boundaryUser(UUID uuid, String name, int total,
			com.bencodez.votingplugin.user.UserManager users) {
		VotingPluginUser user = mock(VotingPluginUser.class);
		TopVoterPlayer player = new TopVoterPlayer(uuid, name, (long) total);
		when(users.getVotingPluginUser(uuid, false)).thenReturn(user);
		when(user.getLastDailyTotal()).thenReturn(total);
		when(user.getTopVoterPlayer()).thenReturn(player);
		return user;
	}
}
