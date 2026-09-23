package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardTarget;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardState;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

class TopVoterTimeChangeRecoveryTest {
	@Test
	void retryUsesPersistedAbsoluteTargetAndDoesNotDuplicateCompletedStreakReward() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		SpecialRewards specialRewards = mock(SpecialRewards.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getSpecialRewards()).thenReturn(specialRewards);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 9, true))
				.thenReturn(new TimeChangeUserProgress(uuid, 8, true, true));
		when(user.getWeekVoteStreak()).thenReturn(8);

		new TopVoterHandler(plugin).applyRecoverableStreak(user, transition, uuid, TopVoter.Weekly, 9, true);

		verify(user, never()).setWeekVoteStreak(org.mockito.ArgumentMatchers.anyInt());
		verify(specialRewards, never()).checkVoteStreak(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.eq(user), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyBoolean());
		verify(serverData, never()).completeTimeChangeUserStreakReward(transition, uuid);
	}

	@Test
	void ambiguousStreakRewardClaimStopsAutomaticReplay() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		SpecialRewards specialRewards = mock(SpecialRewards.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getSpecialRewards()).thenReturn(specialRewards);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 9, true))
				.thenReturn(new TimeChangeUserProgress(uuid, 8, true, false));
		when(serverData.getTimeChangeUserStreakRewardState(transition, uuid))
				.thenReturn(TimeChangeRewardState.CLAIMED);
		when(user.getWeekVoteStreak()).thenReturn(8);

		assertThrows(IllegalStateException.class,
				() -> new TopVoterHandler(plugin).applyRecoverableStreak(
						user, transition, uuid, TopVoter.Weekly, 9, true));

		verify(specialRewards, never()).checkVoteStreak(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.eq(user), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyBoolean());
	}

	@Test
	void userRecoveryWaitsForTheStorageIterationCompletionCallback() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		com.bencodez.advancedcore.api.user.UserManager userManager =
				mock(com.bencodez.advancedcore.api.user.UserManager.class);
		ServerData serverData = mock(ServerData.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		AtomicReference<Consumer<Integer>> completion = new AtomicReference<>();
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseHighestTotals()).thenReturn(true);
		doAnswer(invocation -> {
			completion.set(invocation.getArgument(1));
			return null;
		}).when(userManager).forEachUserKeys(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.any());

		CompletableFuture<Void> recovery = CompletableFuture.runAsync(
				() -> new TopVoterHandler(plugin).processRecoverableUsers(TopVoter.Daily, transition));
		for (int attempt = 0; attempt < 100 && completion.get() == null; attempt++) Thread.sleep(5L);
		assertNotNull(completion.get());
		assertFalse(recovery.isDone());
		completion.get().accept(0);
		recovery.get(2, TimeUnit.SECONDS);
	}

	@Test
	void userRecoveryFlushesUserChangesBeforeAdvancingTheCursor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		com.bencodez.advancedcore.api.user.UserManager userManager =
				mock(com.bencodez.advancedcore.api.user.UserManager.class);
		com.bencodez.votingplugin.user.UserManager votingUsers =
				mock(com.bencodez.votingplugin.user.UserManager.class);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		UserDataCache cache = mock(UserDataCache.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		UUID firstUuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
		UUID secondUuid = UUID.fromString("00000000-0000-0000-0000-000000000002");
		when(plugin.getUserManager()).thenReturn(userManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseHighestTotals()).thenReturn(true);
		when(serverData.getTimeChangeCursor(transition)).thenReturn("");
		when(votingUsers.getVotingPluginUser(org.mockito.ArgumentMatchers.any(UUID.class),
				org.mockito.ArgumentMatchers.eq(false))).thenReturn(user);
		when(user.getCache()).thenReturn(cache);
		doAnswer(invocation -> {
			java.util.function.BiConsumer<UUID, ArrayList<Column>> perUser = invocation.getArgument(0);
			Consumer<Integer> finished = invocation.getArgument(1);
			perUser.accept(secondUuid, new ArrayList<>());
			perUser.accept(firstUuid, new ArrayList<>());
			finished.accept(2);
			return null;
		}).when(userManager).forEachUserKeys(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.any());

		new TopVoterHandler(plugin).processRecoverableUsers(TopVoter.Daily, transition);

		org.mockito.InOrder order = inOrder(cache, serverData);
		order.verify(cache).clearChanges();
		order.verify(serverData).completeTimeChangeUser(transition, firstUuid.toString());
		order.verify(cache).clearChanges();
		order.verify(serverData).completeTimeChangeUser(transition, secondUuid.toString());
	}

	@Test
	void weeklyStreakUsesBoundaryTotalInsteadOfANewWeekVote() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseVoteStreaks()).thenReturn(true);
		when(user.getLastWeeklyTotal()).thenReturn(0);
		when(user.getTotal(TopVoter.Weekly)).thenReturn(1);
		when(user.getWeekVoteStreak()).thenReturn(4);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 0, false))
				.thenReturn(new TimeChangeUserProgress(uuid, 0, false, false));

		new TopVoterHandler(plugin).processWeeklyUser(user, transition, uuid);

		verify(serverData).prepareTimeChangeUserStreak(transition, uuid, 0, false);
		verify(user).setWeekVoteStreak(0);
		verify(user, never()).hasPercentageTotal(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyDouble(), org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyInt());
	}

	@Test
	void delayedDailyRecoveryPreservesAStreakStartedAfterTheBoundary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		long oldUpdate = LocalDateTime.of(2026, 9, 19, 12, 0).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		long newUpdate = LocalDateTime.of(2026, 9, 21, 12, 0).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseVoteStreaks()).thenReturn(true);
		when(transition.getPeriodKey()).thenReturn("2026-09-21");
		when(user.getLastDayVoteStreak()).thenReturn(4);
		when(user.getLastDayVoteStreakLastUpdate()).thenReturn(oldUpdate);
		when(user.getDayVoteStreakLastUpdate()).thenReturn(newUpdate);
		when(user.getDayVoteStreak()).thenReturn(5);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 1, false))
				.thenReturn(new TimeChangeUserProgress(uuid, 1, false, false));

		new TopVoterHandler(plugin).processDailyUser(user, transition, uuid);

		verify(serverData).prepareTimeChangeUserStreak(transition, uuid, 1, false);
		verify(user).setDayVoteStreak(1);
	}

	@Test
	void copiedDailyBoundaryStillResetsStreakAfterSettingIsDisabled() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		long oldUpdate = LocalDateTime.of(2026, 9, 19, 12, 0).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseVoteStreaks()).thenReturn(false);
		when(transition.getPeriodKey()).thenReturn("2026-09-21");
		when(user.getLastDayVoteStreak()).thenReturn(4);
		when(user.getLastDayVoteStreakLastUpdate()).thenReturn(oldUpdate);
		when(user.getDayVoteStreakLastUpdate()).thenReturn(oldUpdate);
		when(user.getDayVoteStreak()).thenReturn(4);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 0, false))
				.thenReturn(new TimeChangeUserProgress(uuid, 0, false, false));

		new TopVoterHandler(plugin).processDailyUser(user, transition, uuid, true);

		verify(serverData).prepareTimeChangeUserStreak(transition, uuid, 0, false);
		verify(user).setDayVoteStreak(0);
	}

	@Test
	void dailyRecoveryClearsAnIneligibleBoundaryStreakWithoutANewVote() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		long oldUpdate = LocalDateTime.of(2026, 9, 19, 12, 0).atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseVoteStreaks()).thenReturn(true);
		when(transition.getPeriodKey()).thenReturn("2026-09-21");
		when(user.getLastDayVoteStreak()).thenReturn(4);
		when(user.getLastDayVoteStreakLastUpdate()).thenReturn(oldUpdate);
		when(user.getDayVoteStreakLastUpdate()).thenReturn(oldUpdate);
		when(user.getDayVoteStreak()).thenReturn(4);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 0, false))
				.thenReturn(new TimeChangeUserProgress(uuid, 0, false, false));

		new TopVoterHandler(plugin).processDailyUser(user, transition, uuid);

		verify(serverData).prepareTimeChangeUserStreak(transition, uuid, 0, false);
		verify(user).setDayVoteStreak(0);
	}

	@Test
	void recoveredHighestTotalsUseCopiedDailyAndWeeklyBoundaries() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(plugin.getConfigFile().isUseHighestTotals()).thenReturn(true);
		when(user.getLastDailyTotal()).thenReturn(5);
		when(user.getTotal(TopVoter.Daily)).thenReturn(8);
		when(user.getHighestDailyTotal()).thenReturn(2);
		when(user.getLastWeeklyTotal()).thenReturn(6);
		when(user.getTotal(TopVoter.Weekly)).thenReturn(9);
		when(user.getHighestWeeklyTotal()).thenReturn(3);

		TopVoterHandler handler = new TopVoterHandler(plugin);
		handler.processDailyUser(user, transition, "player");
		handler.processWeeklyUser(user, transition, "player");

		verify(user).setHighestDailyTotal(5);
		verify(user).setHighestWeeklyTotal(6);
		verify(user, never()).setHighestDailyTotal(8);
		verify(user, never()).setHighestWeeklyTotal(9);
	}

	@Test
	void monthlyRecoveryUsesCopiedBoundaryWhenUndatedTotalsArePrimary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		LocalDateTime previousMonth = LocalDateTime.of(2026, 8, 15, 0, 0);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getConfigFile().isUseVoteStreaks()).thenReturn(true);
		when(plugin.getConfigFile().isUseHighestTotals()).thenReturn(true);
		when(plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()).thenReturn(false);
		when(user.getLastMonthTotal()).thenReturn(0);
		when(user.getTotal(TopVoter.Monthly, previousMonth)).thenReturn(7);
		when(user.getMonthVoteStreak()).thenReturn(4);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 0, false))
				.thenReturn(new TimeChangeUserProgress(uuid, 0, false, false));

		new TopVoterHandler(plugin).processMonthlyUser(user, previousMonth, transition, uuid);

		verify(serverData).prepareTimeChangeUserStreak(transition, uuid, 0, false);
		verify(user).setMonthVoteStreak(0);
		verify(user, never()).setHighestMonthlyTotal(7);
		verify(user, never()).hasPercentageTotal(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyDouble(), org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.anyInt());
	}

	@Test
	void monthlyRecoveryUsesDatedBoundaryWhenDateTotalsArePrimary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		LocalDateTime previousMonth = LocalDateTime.of(2026, 8, 15, 0, 0);
		when(plugin.getConfigFile().isUseHighestTotals()).thenReturn(true);
		when(plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()).thenReturn(true);
		when(user.getTotal(TopVoter.Monthly, previousMonth)).thenReturn(5);
		when(user.getLastMonthTotal()).thenReturn(9);
		when(user.getHighestMonthlyTotal()).thenReturn(2);

		new TopVoterHandler(plugin).processMonthlyUser(user, previousMonth, transition, "player");

		verify(user).setHighestMonthlyTotal(5);
		verify(user, never()).setHighestMonthlyTotal(9);
	}

	@Test
	void dailyRecoveryUsesTheDayBeforeTheStableTransitionDate() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(transition.getPeriodKey()).thenReturn("2026-09-21");

		assertEquals(LocalDateTime.of(2026, 9, 20, 0, 0),
				new TopVoterHandler(plugin).previousDayTime(transition));
	}

	@Test
	void rewardSnapshotFixesRecipientsPlacesAndVoteTotalsAtThePeriodBoundary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		TopVoterPlayer first = new TopVoterPlayer(
				UUID.fromString("00000000-0000-0000-0000-000000000001"), "first", 1L);
		TopVoterPlayer second = new TopVoterPlayer(
				UUID.fromString("00000000-0000-0000-0000-000000000002"), "second", 2L);
		LinkedHashMap<TopVoterPlayer, Integer> ranking = new LinkedHashMap<>();
		ranking.put(first, 20);
		ranking.put(second, 10);
		when(plugin.getSpecialRewardsConfig().isEnableDailyRewards()).thenReturn(true);
		when(plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces()).thenReturn(Set.of("1", "2"));
		TopVoterHandler handler = spy(new TopVoterHandler(plugin));
		doReturn(ranking).when(handler).boundaryTopVotersFor(TopVoter.Daily, transition);

		List<TimeChangeRewardTarget> targets = handler.buildTopRewardSnapshot(TopVoter.Daily, transition);

		assertEquals(List.of(
				new TimeChangeRewardTarget(first.getUuid().toString(), "first", 1, "1", 20),
				new TimeChangeRewardTarget(second.getUuid().toString(), "second", 2, "2", 10)), targets);
	}

	@Test
	void ambiguousTopRewardClaimStopsAutomaticReplay() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		TimeChangeRewardTarget target = new TimeChangeRewardTarget(uuid, "first", 1, "1", 20);
		when(plugin.getServerData()).thenReturn(serverData);
		when(serverData.getTimeChangeRewardTargets(transition)).thenReturn(List.of(target));
		when(serverData.getTimeChangeRewardState(transition, uuid)).thenReturn(TimeChangeRewardState.CLAIMED);

		assertThrows(IllegalStateException.class,
				() -> new TopVoterHandler(plugin).processRecoverableTopRewards(TopVoter.Daily, transition));

		verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(
				org.mockito.ArgumentMatchers.any(UUID.class), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void archiveRetryUsesOneStableTransitionFile() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(transition.getPeriodKey()).thenReturn("2026-W38");
		TopVoterHandler handler = new TopVoterHandler(plugin);

		String first = handler.timeChangeArchiveFileName(TopVoter.Weekly, transition);
		String retry = handler.timeChangeArchiveFileName(TopVoter.Weekly, transition);

		assertEquals("TopVoter" + File.separator + "Weekly" + File.separator + "Weekly_2026-W38.yml", first);
		assertEquals(first, retry);
	}

	@Test
	void sqliteTotalResetRetryPreservesVotesAcceptedAfterTheFirstReset() throws Exception {
		try (Connection connection = DriverManager.getConnection("jdbc:sqlite::memory:");
				Statement statement = connection.createStatement()) {
			statement.executeUpdate("CREATE TABLE users (uuid TEXT PRIMARY KEY, DailyTotal INTEGER, LastDailyTotal INTEGER)");
			statement.executeUpdate("INSERT INTO users VALUES ('player', 20, 0)");

			TimeChangeTotalReset.copyBoundarySqlite(connection, "users", "DailyTotal", "LastDailyTotal",
					"time-copy:DAY:2026-09-21");
			statement.executeUpdate("UPDATE users SET DailyTotal = DailyTotal + 3 WHERE uuid = 'player'");
			TimeChangeTotalReset.copyBoundarySqlite(connection, "users", "DailyTotal", "LastDailyTotal",
					"time-copy:DAY:2026-09-21");
			TimeChangeTotalReset.resetSqlite(connection, "users", "DailyTotal", "LastDailyTotal",
					"time-total:DAY:2026-09-21");
			statement.executeUpdate("UPDATE users SET DailyTotal = DailyTotal + 2 WHERE uuid = 'player'");
			TimeChangeTotalReset.resetSqlite(connection, "users", "DailyTotal", "LastDailyTotal",
					"time-total:DAY:2026-09-21");

			try (ResultSet result = statement.executeQuery("SELECT DailyTotal FROM users WHERE uuid = 'player'")) {
				assertEquals(5, result.getInt(1));
			}
		}
	}

	@Test
	void sqliteVotePartyBoundaryResetPreservesVotesAcceptedAfterTheBoundary() throws Exception {
		try (Connection connection = DriverManager.getConnection("jdbc:sqlite::memory:");
				Statement statement = connection.createStatement()) {
			statement.executeUpdate("CREATE TABLE users (uuid TEXT PRIMARY KEY, VotePartyVotes INTEGER, "
					+ "LastVotePartyVotes INTEGER)");
			statement.executeUpdate("INSERT INTO users VALUES ('player', 4, 0)");
			TimeChangeTotalReset.copyBoundarySqlite(connection, "users", "VotePartyVotes", "LastVotePartyVotes",
					"vote-party-copy:DAY");
			statement.executeUpdate("UPDATE users SET VotePartyVotes = VotePartyVotes + 3 WHERE uuid = 'player'");
			TimeChangeTotalReset.resetSqlite(connection, "users", "VotePartyVotes", "LastVotePartyVotes",
					"vote-party-reset:DAY");
			statement.executeUpdate("UPDATE users SET VotePartyVotes = VotePartyVotes + 2 WHERE uuid = 'player'");
			TimeChangeTotalReset.resetSqlite(connection, "users", "VotePartyVotes", "LastVotePartyVotes",
					"vote-party-reset:DAY");
			try (ResultSet result = statement.executeQuery("SELECT VotePartyVotes FROM users WHERE uuid = 'player'")) {
				assertEquals(5, result.getInt(1));
			}
		}
	}

	@Test
	void sqliteDailyStreakBoundaryKeepsValueAndTimestampTogether() throws Exception {
		try (Connection connection = DriverManager.getConnection("jdbc:sqlite::memory:");
				Statement statement = connection.createStatement()) {
			statement.executeUpdate("CREATE TABLE users (uuid TEXT PRIMARY KEY, DayVoteStreak INTEGER, "
					+ "LastDayVoteStreak INTEGER, DayVoteStreakLastUpdate TEXT, "
					+ "LastDayVoteStreakLastUpdate TEXT)");
			statement.executeUpdate("INSERT INTO users VALUES ('player', 4, 0, '100', '')");

			TimeChangeTotalReset.copyDailyStreakBoundarySqlite(connection, "users", "time-streak-copy:DAY");
			statement.executeUpdate("UPDATE users SET DayVoteStreak = 5, DayVoteStreakLastUpdate = '200'");
			TimeChangeTotalReset.copyDailyStreakBoundarySqlite(connection, "users", "time-streak-copy:DAY");

			try (ResultSet result = statement.executeQuery(
					"SELECT LastDayVoteStreak, LastDayVoteStreakLastUpdate FROM users WHERE uuid = 'player'")) {
				assertEquals(4, result.getInt(1));
				assertEquals("100", result.getString(2));
			}
		}
	}

	@Test
	void sqliteVoteShopResetsRemainIndependentAndDoNotRepeat() throws Exception {
		try (Connection connection = DriverManager.getConnection("jdbc:sqlite::memory:");
				Statement statement = connection.createStatement()) {
			statement.executeUpdate("CREATE TABLE users (uuid TEXT PRIMARY KEY, VoteShopLimitdaily INTEGER, "
					+ "VoteShopLimitweekly INTEGER)");
			statement.executeUpdate("INSERT INTO users VALUES ('player', 4, 6)");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VoteShopLimitdaily",
					"time-shop:DAY:2026-09-21:VoteShopLimitdaily");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VoteShopLimitweekly",
					"time-shop:DAY:2026-09-21:VoteShopLimitweekly");
			statement.executeUpdate("UPDATE users SET VoteShopLimitdaily = 1, VoteShopLimitweekly = 2");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VoteShopLimitdaily",
					"time-shop:DAY:2026-09-21:VoteShopLimitdaily");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VoteShopLimitweekly",
					"time-shop:DAY:2026-09-21:VoteShopLimitweekly");
			try (ResultSet result = statement.executeQuery(
					"SELECT VoteShopLimitdaily, VoteShopLimitweekly FROM users WHERE uuid = 'player'")) {
				assertEquals(1, result.getInt(1));
				assertEquals(2, result.getInt(2));
			}
		}
	}

	@Test
	void failedMysqlVoteShopResetLeavesPhasePending() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getShopFile().getShopIdentifiers()).thenReturn(Set.of("daily"));
		when(plugin.getShopFile().getVoteShopResetDaily("daily")).thenReturn(true);
		when(transition.getType()).thenReturn(TimeType.DAY);
		when(transition.getPeriodKey()).thenReturn("2026-09-21");
		try (MockedStatic<VoteShopPurchaseService> purchaseService = mockStatic(VoteShopPurchaseService.class)) {
			purchaseService.when(() -> VoteShopPurchaseService.limitGenerationIdForTransition(transition))
					.thenReturn("time-shop:DAY:2026-09-21");
			purchaseService.when(() -> VoteShopPurchaseService.resetMysqlLimitWithPurchaseFence(plugin,
					"VoteShopLimitdaily", "time-shop:DAY:2026-09-21")).thenReturn(false);

			assertThrows(IllegalStateException.class,
					() -> new TopVoterHandler(plugin).processRecoverableVoteShop(TopVoter.Daily, transition));
		}

		verify(serverData, never()).completeTimeChangePhase(transition, "VOTE_SHOP");
	}
}
