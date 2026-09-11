package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

class SharedPointAdditionJournalTest {
	@Test
	void lostCommitAcknowledgementAndFailedConfirmationRetryCreditsExactlyOnce() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement missingLookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		when(missing.next()).thenReturn(false);
		when(missingLookup.executeQuery()).thenReturn(missing);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(missingLookup);

		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		ResultSet total = mock(ResultSet.class);
		when(credit.executeUpdate()).thenReturn(1);
		when(total.next()).thenReturn(true);
		when(total.getInt(1)).thenReturn(15);
		when(read.executeQuery()).thenReturn(total);
		when(complete.executeUpdate()).thenReturn(1);
		when(fixture.firstAttempt.prepareStatement(anyString())).thenReturn(insert, credit, read, complete);
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(fixture.firstAttempt).commit();
		when(fixture.failedConfirmation.prepareStatement(anyString()))
				.thenThrow(new java.sql.SQLException("confirmation unavailable"));

		PreparedStatement retryLookup = mock(PreparedStatement.class);
		ResultSet completed = completedRow("player", "Points", 5, 15);
		when(retryLookup.executeQuery()).thenReturn(completed);
		when(fixture.retryLookup.prepareStatement(anyString())).thenReturn(retryLookup);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.initialLookup, fixture.firstAttempt,
				fixture.failedConfirmation, fixture.retryLookup);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertThrows(java.sql.SQLException.class, () -> journal.add("reward-operation", "player", "Points", 5, 100L));

		SharedPointAdditionJournal.AdditionResult result = journal.add("reward-operation", "player", "Points", 5,
				101L);
		assertEquals(15, result.total());
		verify(credit, times(1)).executeUpdate();
		verify(fixture.firstAttempt, atLeastOnce()).close();
		verify(retryLookup).setString(1, "reward-operation");
	}

	@Test
	void completedOperationRejectsAConflictingRetryInsteadOfChangingPoints() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet completed = completedRow("player", "Points", 5, 15);
		when(lookup.executeQuery()).thenReturn(completed);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertThrows(java.sql.SQLException.class, () -> journal.add("reward-operation", "player", "Points", 6, 100L));
		verify(fixture.firstAttempt, org.mockito.Mockito.never()).prepareStatement(anyString());
	}

	@Test
	void distinctRewardOccurrencesCreditIndependentlyWhileRetryingOneDoesNot() throws Exception {
		Fixture fixture = fixture();
		Connection firstLookup = missingLookup();
		Attempt firstAttempt = successfulAttempt(15);
		Connection secondLookup = missingLookup();
		Attempt secondAttempt = successfulAttempt(20);
		Connection retryLookup = completedLookup("player", "Points", 5, 15);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(firstLookup, firstAttempt.connection(), secondLookup,
				secondAttempt.connection(), retryLookup);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertEquals(15, journal.add("occurrence-one/stage", "player", "Points", 5, 100L).total());
		assertEquals(20, journal.add("occurrence-two/stage", "player", "Points", 5, 101L).total());
		assertEquals(15, journal.add("occurrence-one/stage", "player", "Points", 5, 102L).total());

		verify(firstAttempt.credit(), times(1)).executeUpdate();
		verify(secondAttempt.credit(), times(1)).executeUpdate();
	}

	@Test
	void journalTableNameRemainsPortableForLongSourceNames() {
		String name = SharedPointAdditionJournal.journalTableName("u".repeat(80));
		assertTrue(name.matches("vp_pa_[0-9a-f]{32}"));
		assertEquals("VotingPlugin_Users_PointAdditions",
				SharedPointAdditionJournal.journalTableName("VotingPlugin_Users"));
	}

	private static ResultSet completedRow(String uuid, String pointsColumn, int amount, int total) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(uuid);
		when(row.getString(2)).thenReturn(pointsColumn);
		when(row.getInt(3)).thenReturn(amount);
		when(row.getString(4)).thenReturn("COMPLETED");
		when(row.getObject(5)).thenReturn(Integer.valueOf(total));
		when(row.getInt(5)).thenReturn(total);
		return row;
	}

	private static Connection missingLookup() throws Exception {
		Connection connection = mock(Connection.class);
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(connection.prepareStatement(anyString())).thenReturn(lookup);
		return connection;
	}

	private static Connection completedLookup(String uuid, String pointsColumn, int amount, int total) throws Exception {
		Connection connection = mock(Connection.class);
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet completed = completedRow(uuid, pointsColumn, amount, total);
		when(lookup.executeQuery()).thenReturn(completed);
		when(connection.prepareStatement(anyString())).thenReturn(lookup);
		return connection;
	}

	private static Attempt successfulAttempt(int total) throws Exception {
		Connection connection = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		ResultSet result = mock(ResultSet.class);
		when(credit.executeUpdate()).thenReturn(1);
		when(result.next()).thenReturn(true);
		when(result.getInt(1)).thenReturn(total);
		when(read.executeQuery()).thenReturn(result);
		when(complete.executeUpdate()).thenReturn(1);
		when(connection.prepareStatement(anyString())).thenReturn(insert, credit, read, complete);
		return new Attempt(connection, credit);
	}

	private record Attempt(Connection connection, PreparedStatement credit) {}

	private static Fixture fixture() throws Exception {
		Fixture fixture = new Fixture();
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.initialLookup = mock(Connection.class);
		fixture.firstAttempt = mock(Connection.class);
		fixture.failedConfirmation = mock(Connection.class);
		fixture.retryLookup = mock(Connection.class);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.initialLookup, fixture.firstAttempt,
				fixture.failedConfirmation, fixture.retryLookup);
		return fixture;
	}

	private static final class Fixture {
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		Connection initialLookup;
		Connection firstAttempt;
		Connection failedConfirmation;
		Connection retryLookup;
	}
}
