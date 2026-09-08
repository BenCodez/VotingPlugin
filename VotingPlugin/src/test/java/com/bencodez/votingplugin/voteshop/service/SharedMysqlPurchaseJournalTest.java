package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

class SharedMysqlPurchaseJournalTest {
	@Test
	void journalTableNameIsPortableAndCollisionResistantForLongSourceNames() {
		String source = "u".repeat(80);
		String journalTable = SharedMysqlPurchaseJournal.journalTableName(source);

		assertEquals(journalTable, SharedMysqlPurchaseJournal.journalTableName(source));
		assertTrue(journalTable.getBytes(java.nio.charset.StandardCharsets.UTF_8).length <= 63);
		assertTrue(journalTable.matches("vp_vsp_[0-9a-f]{32}"));
		assertNotEquals(journalTable, SharedMysqlPurchaseJournal.journalTableName(source + "x"));
		assertTrue(SharedMysqlPurchaseJournal.journalTableName("é".repeat(30)).matches("vp_vsp_[0-9a-f]{32}"));
		assertEquals("VotingPlugin_Users_VoteShopPurchases",
				SharedMysqlPurchaseJournal.journalTableName("VotingPlugin_Users"));
	}

	@Test
	void reservationPersistsPendingDebitInTheSameTransaction() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(insert, debit);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.reserve("purchase-1", "player", "Points", "VoteShopLimitdaily", 10, 1,
				SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION, 0L, 100L));

		verify(insert).setString(7, SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION);
		verify(insert).setString(9, "PENDING");
		verify(debit).setInt(1, 10);
		verify(fixture.work).commit();
	}

	@Test
	void recoveryRefundsOnlyExpiredPendingPurchase() throws Exception {
		Fixture fixture = fixture();
		Connection candidates = mock(Connection.class);
		Connection compensatingCandidates = mock(Connection.class);
		Connection refund = mock(Connection.class);
		Connection cleanup = mock(Connection.class);
		PreparedStatement candidateStatement = mock(PreparedStatement.class);
		PreparedStatement compensatingStatement = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		ResultSet expiredPending = ids("expired-pending");
		ResultSet noCompensating = ids();
		ResultSet pending = pendingRow();
		ResultSet noTerminalRows = ids();
		when(candidates.prepareStatement(anyString())).thenReturn(candidateStatement);
		when(candidateStatement.executeQuery()).thenReturn(expiredPending);
		when(compensatingCandidates.prepareStatement(anyString())).thenReturn(compensatingStatement);
		when(compensatingStatement.executeQuery()).thenReturn(noCompensating);
		when(refund.prepareStatement(anyString())).thenReturn(select, credit, terminal);
		when(select.executeQuery()).thenReturn(pending);
		when(credit.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);
		when(cleanup.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(noTerminalRows);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(candidates, refund, compensatingCandidates, cleanup);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		journal.recoverAndCleanup(SharedMysqlPurchaseJournal.PENDING_RECOVERY_AGE_MILLIS + 1L);

		verify(credit).setInt(1, 10);
		verify(credit).setString(2, "player");
		verify(terminal).setString(1, "REFUNDED");
		verify(refund).commit();
	}

	@Test
	void hookStartedPurchaseIsNeverRefundedByCompensation() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		ResultSet hookStarted = hookStartedRow();
		when(fixture.work.prepareStatement(anyString())).thenReturn(select);
		when(select.executeQuery()).thenReturn(hookStarted);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertFalse(journal.refundPending("claimed-purchase"));
	}

	@Test
	void staleRefundRestoresPointsWithoutDecrementingANewerLimitGeneration() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		when(pending.getString(6)).thenReturn("D:2026-09-08");
		when(pending.getLong(7)).thenReturn(100L);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, refund, terminal);
		when(select.executeQuery()).thenReturn(pending);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundPending("old-generation", 100L));

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(3)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(1).contains("`Points` = `Points` + ?"));
		assertFalse(sql.getAllValues().get(1).contains("`VoteShopLimitdaily` = GREATEST"));
	}

	@Test
	void refundStillReleasesALimitThatHasNoConfiguredReset() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		when(pending.getString(6)).thenReturn(SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, refund, terminal);
		when(select.executeQuery()).thenReturn(pending);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundPending("unbounded-generation", 100L));

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(3)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(1).contains("`VoteShopLimitdaily` = GREATEST"));
	}

	@Test
	void schedulerProvenUnstartedHookCanBeRefunded() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markCompensating = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet compensating = pendingRow("COMPENSATING");
		when(fixture.work.prepareStatement(anyString())).thenReturn(markCompensating, select, credit, terminal);
		when(markCompensating.executeUpdate()).thenReturn(1);
		when(select.executeQuery()).thenReturn(compensating);
		when(credit.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundUnstartedReward("scheduler-rejected"));

		verify(fixture.work, org.mockito.Mockito.times(2)).setAutoCommit(false);
		verify(terminal).setString(1, "REFUNDED");
	}

	@Test
	void refundAcceptsHyphenatedConfiguredLimitIdentifier() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		when(pending.getString(4)).thenReturn("VoteShopLimitdaily-key");
		when(pending.getString(6)).thenReturn(SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, refund, terminal);
		when(select.executeQuery()).thenReturn(pending);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		boolean refunded = journal.refundPending("hyphenated-limit", 100L);
		verify(refund).executeUpdate();
		verify(terminal).executeUpdate();
		assertTrue(refunded);

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(3)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(1).contains("VoteShopLimitdaily-key"));
	}

	@Test
	void failedCompensationIsRetriedWithTheDurableMarker() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markFirst = mock(PreparedStatement.class);
		PreparedStatement selectFirst = mock(PreparedStatement.class);
		PreparedStatement creditFirst = mock(PreparedStatement.class);
		PreparedStatement markSecond = mock(PreparedStatement.class);
		PreparedStatement selectSecond = mock(PreparedStatement.class);
		PreparedStatement creditSecond = mock(PreparedStatement.class);
		PreparedStatement terminalSecond = mock(PreparedStatement.class);
		ResultSet compensating = pendingRow("COMPENSATING");
		when(fixture.work.prepareStatement(anyString())).thenReturn(markFirst, selectFirst, creditFirst,
				markSecond, selectSecond, creditSecond, terminalSecond);
		when(markFirst.executeUpdate()).thenReturn(1);
		when(selectFirst.executeQuery()).thenReturn(compensating);
		doThrow(new java.sql.SQLException("temporary database outage")).when(creditFirst).executeUpdate();
		when(markSecond.executeUpdate()).thenReturn(1);
		when(selectSecond.executeQuery()).thenReturn(compensating);
		when(creditSecond.executeUpdate()).thenReturn(1);
		when(terminalSecond.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundUnstartedReward("retry-compensation"));

		verify(markFirst).setString(1, "COMPENSATING");
		verify(markSecond).setString(1, "COMPENSATING");
		verify(creditSecond).executeUpdate();
		verify(terminalSecond).setString(1, "REFUNDED");
	}

	@Test
	void ambiguousReservationCommitIsConfirmedAfterItsConnectionIsReleased() throws Exception {
		Fixture fixture = fixture();
		Connection reservation = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		ResultSet committed = mock(ResultSet.class);
		when(reservation.prepareStatement(anyString())).thenReturn(insert, debit);
		when(debit.executeUpdate()).thenReturn(1);
		when(confirmation.prepareStatement(anyString())).thenReturn(select);
		when(select.executeQuery()).thenReturn(committed);
		when(committed.next()).thenReturn(true);
		when(committed.getString(1)).thenReturn("PENDING");
		AtomicBoolean reservationClosed = new AtomicBoolean();
		org.mockito.Mockito.doAnswer(ignored -> {
			reservationClosed.set(true);
			return null;
		}).when(reservation).close();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(reservation).thenAnswer(ignored -> {
			assertTrue(reservationClosed.get(), "The ambiguous reservation handle must be released before confirmation");
			return confirmation;
		});
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(reservation).commit();

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.reserve("purchase-ambiguous", "player", "Points", null, 10, 0,
				null, 0L, 100L));

		verify(reservation, atLeastOnce()).close();
		verify(confirmation).prepareStatement(anyString());
	}

	@Test
	void ambiguousClaimUpdateIsConfirmedBeforeRewardMayRun() throws Exception {
		Fixture fixture = fixture();
		Connection claimConnection = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement claim = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		ResultSet committed = mock(ResultSet.class);
		when(claimConnection.prepareStatement(anyString())).thenReturn(claim);
		when(confirmation.prepareStatement(anyString())).thenReturn(select);
		when(select.executeQuery()).thenReturn(committed);
		when(committed.next()).thenReturn(true);
		when(committed.getString(1)).thenReturn("HOOK_STARTED");
		AtomicBoolean claimConnectionClosed = new AtomicBoolean();
		org.mockito.Mockito.doAnswer(ignored -> {
			claimConnectionClosed.set(true);
			return null;
		}).when(claimConnection).close();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(claimConnection).thenAnswer(ignored -> {
			assertTrue(claimConnectionClosed.get(), "The ambiguous claim handle must be released before confirmation");
			return confirmation;
		});
		doThrow(new java.sql.SQLException("update acknowledgement lost")).when(claim).executeUpdate();

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertEquals(SharedMysqlPurchaseJournal.ClaimOutcome.CLAIMED,
				journal.claimReward("purchase-ambiguous-claim", 200L));

		verify(claimConnection, atLeastOnce()).close();
		verify(confirmation).prepareStatement(anyString());
	}

	@Test
	void ambiguousRefundCommitIsConfirmedBeforeReturningSuccess() throws Exception {
		Fixture fixture = fixture();
		Connection refundConnection = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement selectRefund = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		PreparedStatement selectConfirmation = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		ResultSet refunded = mock(ResultSet.class);
		when(refundConnection.prepareStatement(anyString())).thenReturn(selectRefund, credit, terminal);
		when(selectRefund.executeQuery()).thenReturn(pending);
		when(credit.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);
		when(confirmation.prepareStatement(anyString())).thenReturn(selectConfirmation);
		when(selectConfirmation.executeQuery()).thenReturn(refunded);
		when(refunded.next()).thenReturn(true);
		when(refunded.getString(1)).thenReturn("REFUNDED");
		AtomicBoolean refundClosed = new AtomicBoolean();
		org.mockito.Mockito.doAnswer(ignored -> {
			refundClosed.set(true);
			return null;
		}).when(refundConnection).close();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(refundConnection).thenAnswer(ignored -> {
			assertTrue(refundClosed.get(), "The ambiguous refund handle must be released before confirmation");
			return confirmation;
		});
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(refundConnection).commit();

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundPending("purchase-ambiguous-refund", 100L));

		verify(refundConnection, atLeastOnce()).close();
		verify(confirmation).prepareStatement(anyString());
	}

	@Test
	void unstartedRewardRetryRecognizesAnAlreadyCommittedRefund() throws Exception {
		Fixture fixture = fixture();
		Connection mark = mock(Connection.class);
		Connection refund = mock(Connection.class);
		Connection failedConfirmation = mock(Connection.class);
		Connection retryMark = mock(Connection.class);
		Connection finalConfirmation = mock(Connection.class);
		PreparedStatement markStatement = mock(PreparedStatement.class);
		PreparedStatement refundSelect = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		PreparedStatement failedSelect = mock(PreparedStatement.class);
		PreparedStatement retryMarkStatement = mock(PreparedStatement.class);
		PreparedStatement finalSelect = mock(PreparedStatement.class);
		ResultSet compensating = pendingRow("COMPENSATING");
		ResultSet refunded = mock(ResultSet.class);
		when(mark.prepareStatement(anyString())).thenReturn(markStatement);
		when(markStatement.executeUpdate()).thenReturn(1);
		when(refund.prepareStatement(anyString())).thenReturn(refundSelect, credit, terminal);
		when(refundSelect.executeQuery()).thenReturn(compensating);
		when(credit.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(refund).commit();
		when(failedConfirmation.prepareStatement(anyString())).thenReturn(failedSelect);
		doThrow(new java.sql.SQLException("confirmation unavailable")).when(failedSelect).executeQuery();
		when(retryMark.prepareStatement(anyString())).thenReturn(retryMarkStatement);
		when(retryMarkStatement.executeUpdate()).thenReturn(0);
		when(finalConfirmation.prepareStatement(anyString())).thenReturn(finalSelect);
		when(finalSelect.executeQuery()).thenReturn(refunded);
		when(refunded.next()).thenReturn(true);
		when(refunded.getString(1)).thenReturn("REFUNDED");
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(mark, refund, failedConfirmation,
				retryMark, finalConfirmation);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundUnstartedReward("already-refunded"));

		verify(finalSelect).executeQuery();
	}

	private static Fixture fixture() throws Exception {
		Fixture fixture = new Fixture();
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.work = mock(Connection.class);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.work);
		return fixture;
	}

	private static ResultSet ids(String... values) throws Exception {
		ResultSet rows = mock(ResultSet.class);
		Boolean[] next = new Boolean[values.length + 1];
		for (int index = 0; index < values.length; index++) next[index] = Boolean.TRUE;
		next[values.length] = Boolean.FALSE;
		when(rows.next()).thenReturn(next[0], java.util.Arrays.copyOfRange(next, 1, next.length));
		if (values.length > 0) when(rows.getString(1)).thenReturn(values[0]);
		return rows;
	}

	private static ResultSet pendingRow() throws Exception {
		return pendingRow("PENDING");
	}

	private static ResultSet pendingRow(String state) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(state);
		when(row.getString(2)).thenReturn("player");
		when(row.getString(3)).thenReturn("Points");
		when(row.getString(4)).thenReturn("VoteShopLimitdaily");
		when(row.getInt(5)).thenReturn(10);
		return row;
	}

	private static ResultSet hookStartedRow() throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn("HOOK_STARTED");
		return row;
	}

	private static final class Fixture {
		private MySQL table;
		private com.bencodez.simpleapi.sql.mysql.MySQL sql;
		private Connection work;
	}
}
