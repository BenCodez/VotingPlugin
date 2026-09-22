package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardTarget;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.user.VotingPluginUser;

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
		when(plugin.getTopVoter(TopVoter.Daily)).thenReturn(ranking);

		List<TimeChangeRewardTarget> targets = new TopVoterHandler(plugin)
				.buildTopRewardSnapshot(TopVoter.Daily, transition);

		assertEquals(List.of(
				new TimeChangeRewardTarget(first.getUuid().toString(), "first", 1, "1", 20),
				new TimeChangeRewardTarget(second.getUuid().toString(), "second", 2, "2", 10)), targets);
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
	void sqliteAuxiliaryResetDoesNotRepeatAfterNewVotes() throws Exception {
		try (Connection connection = DriverManager.getConnection("jdbc:sqlite::memory:");
				Statement statement = connection.createStatement()) {
			statement.executeUpdate("CREATE TABLE users (uuid TEXT PRIMARY KEY, VotePartyVotes INTEGER)");
			statement.executeUpdate("INSERT INTO users VALUES ('player', 4)");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VotePartyVotes", "vote-party:DAY");
			statement.executeUpdate("UPDATE users SET VotePartyVotes = 2 WHERE uuid = 'player'");
			TimeChangeTotalReset.resetSqliteToZero(connection, "users", "VotePartyVotes", "vote-party:DAY");
			try (ResultSet result = statement.executeQuery("SELECT VotePartyVotes FROM users WHERE uuid = 'player'")) {
				assertEquals(2, result.getInt(1));
			}
		}
	}
}
