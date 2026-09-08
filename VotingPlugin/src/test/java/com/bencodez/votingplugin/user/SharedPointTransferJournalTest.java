package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

class SharedPointTransferJournalTest {
	@Test
	void schemaInitializationIsOncePerLiveMysqlHandle() throws Exception {
		Fixture fixture = fixture();
		assertNotNull(SharedPointTransferJournal.forTable(fixture.table));
		assertNotNull(SharedPointTransferJournal.forTable(fixture.table));
		verify(fixture.sql.getConnectionManager(), times(1)).getConnection();
	}

	@Test
	void reservationDebitsAndJournalsInOneShortTransaction() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(lookup);
		when(fixture.reservation.prepareStatement(anyString())).thenReturn(insert, debit);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.reserve("transfer-1", "source", "Points", 10, "target", 10, 100L));

		verify(insert).setString(7, "RESERVED");
		verify(debit).setInt(1, 10);
		verify(fixture.reservation).commit();
		// commitAndConfirm closes before its confirmation lookup; the enclosing
		// try-with-resources then closes the same JDBC handle idempotently.
		verify(fixture.reservation, atLeastOnce()).close();
	}

	@Test
	void insufficientSourceRollsBackTheJournalInsertAndDoesNotRunAHook() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(debit.executeUpdate()).thenReturn(0);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(lookup);
		when(fixture.reservation.prepareStatement(anyString())).thenReturn(insert, debit);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertFalse(journal.reserve("transfer-2", "source", "Points", 10, "target", 10, 100L));

		verify(fixture.reservation).rollback();
	}

	@Test
	void cancelledHookRefundsExactlyTheReservedDebit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		ResultSet row = row("HOOK_STARTED", "owner-1");
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement journalUpdate = mock(PreparedStatement.class);
		when(select.executeQuery()).thenReturn(row);
		when(refund.executeUpdate()).thenReturn(1);
		when(journalUpdate.executeUpdate()).thenReturn(1);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(select, refund, journalUpdate);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertFalse(journal.settle("transfer-3", "owner-1", "source", "Points", "target", "Points", 10, null));

		verify(refund).setInt(1, 10);
		verify(refund).setString(2, "source");
		verify(journalUpdate).setString(1, "REFUNDED");
		verify(fixture.lookup).commit();
	}

	@Test
	void acceptedHookCreditsAdjustedAmountAndMarksTerminalState() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		ResultSet row = row("HOOK_STARTED", "owner-2");
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement journalUpdate = mock(PreparedStatement.class);
		when(select.executeQuery()).thenReturn(row);
		when(credit.executeUpdate()).thenReturn(1);
		when(journalUpdate.executeUpdate()).thenReturn(1);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(select, credit, journalUpdate);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.settle("transfer-4", "owner-2", "source", "Points", "target", "Points", 10, 4));

		verify(credit).setInt(1, 4);
		verify(credit).setString(2, "target");
		verify(journalUpdate).setString(1, "COMPLETED");
		verify(journalUpdate).setInt(2, 4);
		verify(fixture.lookup).commit();
	}

	@Test
	void ambiguousReservationCommitIsConfirmedAfterConnectionIsReleased() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement initialLookup = mock(PreparedStatement.class);
		ResultSet missing = mock(ResultSet.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement confirmationLookup = mock(PreparedStatement.class);
		ResultSet confirmed = transferRow("source", "target", 10, 10, "RESERVED");
		when(missing.next()).thenReturn(false);
		when(initialLookup.executeQuery()).thenReturn(missing);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.lookup.prepareStatement(anyString())).thenReturn(initialLookup);
		when(fixture.reservation.prepareStatement(anyString())).thenReturn(insert, debit);
		Connection confirmation = mock(Connection.class);
		when(confirmation.prepareStatement(anyString())).thenReturn(confirmationLookup);
		when(confirmationLookup.executeQuery()).thenReturn(confirmed);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, fixture.lookup,
				fixture.reservation, confirmation);
		doThrow(new java.sql.SQLException("ack lost")).when(fixture.reservation).commit();

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.reserve("transfer-5", "source", "Points", 10, "target", 10, 100L));

		// Confirmation must happen after the possibly-broken handle is released;
		// the outer try-with-resources may then close it idempotently once more.
		verify(fixture.reservation, atLeastOnce()).close();
		verify(confirmationLookup).executeQuery();
	}

	@Test
	void ambiguousClaimCommitIsConfirmedBeforeTheHookMayRun() throws Exception {
		Fixture fixture = fixture();
		Connection claim = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement claimSelect = mock(PreparedStatement.class);
		PreparedStatement claimUpdate = mock(PreparedStatement.class);
		PreparedStatement confirmationLookup = mock(PreparedStatement.class);
		ResultSet reserved = row("RESERVED", null);
		ResultSet confirmed = transferRow("source", "target", 10, 10, "HOOK_STARTED");
		when(claim.prepareStatement(anyString())).thenReturn(claimSelect, claimUpdate);
		when(claimSelect.executeQuery()).thenReturn(reserved);
		when(claimUpdate.executeUpdate()).thenReturn(1);
		doThrow(new java.sql.SQLException("ack lost")).when(claim).commit();
		when(confirmation.prepareStatement(anyString())).thenReturn(confirmationLookup);
		when(confirmationLookup.executeQuery()).thenReturn(confirmed);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, claim, confirmation);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.claimHookWithConfirmation("transfer-claim", "owner-claim", 100L)
				== SharedPointTransferJournal.ClaimOutcome.CLAIMED);

		verify(claim, atLeastOnce()).close();
		verify(confirmationLookup).executeQuery();
	}

	@Test
	void ambiguousClaimThatCannotBeConfirmedRemainsIndeterminate() throws Exception {
		Fixture fixture = fixture();
		Connection claim = mock(Connection.class);
		Connection unavailable = mock(Connection.class);
		PreparedStatement claimSelect = mock(PreparedStatement.class);
		PreparedStatement claimUpdate = mock(PreparedStatement.class);
		ResultSet reserved = row("RESERVED", null);
		when(claim.prepareStatement(anyString())).thenReturn(claimSelect, claimUpdate);
		when(claimSelect.executeQuery()).thenReturn(reserved);
		when(claimUpdate.executeUpdate()).thenReturn(1);
		doThrow(new java.sql.SQLException("ack lost")).when(claim).commit();
		when(unavailable.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("database unavailable"));
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, claim, unavailable,
				unavailable, unavailable);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.claimHookWithConfirmation("transfer-unknown", "owner", 100L)
				== SharedPointTransferJournal.ClaimOutcome.INDETERMINATE);
	}

	@Test
	void settlementRetriesTheSameTransferAfterAmbiguousCommitAndFailedConfirmation() throws Exception {
		Fixture fixture = fixture();
		Connection firstSettlement = mock(Connection.class);
		Connection unavailableConfirmation = mock(Connection.class);
		Connection confirmedSettlement = mock(Connection.class);
		PreparedStatement firstSelect = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement firstUpdate = mock(PreparedStatement.class);
		PreparedStatement confirmedSelect = mock(PreparedStatement.class);
		ResultSet hookStarted = row("HOOK_STARTED", "owner-settle");
		ResultSet completed = row("COMPLETED", "owner-settle");
		when(firstSettlement.prepareStatement(anyString())).thenReturn(firstSelect, credit, firstUpdate);
		when(firstSelect.executeQuery()).thenReturn(hookStarted);
		when(credit.executeUpdate()).thenReturn(1);
		when(firstUpdate.executeUpdate()).thenReturn(1);
		doThrow(new java.sql.SQLException("ack lost")).when(firstSettlement).commit();
		when(confirmedSettlement.prepareStatement(anyString())).thenReturn(confirmedSelect);
		when(confirmedSelect.executeQuery()).thenReturn(completed);
		when(unavailableConfirmation.prepareStatement(anyString()))
				.thenThrow(new java.sql.SQLException("confirmation unavailable"));
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, firstSettlement)
				.thenReturn(unavailableConfirmation, confirmedSettlement);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.settleWithConfirmation("transfer-settle", "owner-settle", "source", "Points", "target",
				"Points", 10, 4) == SharedPointTransferJournal.SettlementOutcome.COMPLETED);

		verify(credit).executeUpdate();
		verify(confirmedSelect).executeQuery();
	}

	@Test
	void settlementRemainsIndeterminateWhenTheSameTransferCannotBeReconfirmed() throws Exception {
		Fixture fixture = fixture();
		Connection unavailable = mock(Connection.class);
		when(unavailable.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("database unavailable"));
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, unavailable, unavailable,
				unavailable);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		assertTrue(journal.settleWithConfirmation("transfer-unknown", "owner", "source", "Points", "target", "Points",
				10, 4) == SharedPointTransferJournal.SettlementOutcome.INDETERMINATE);
	}

	@Test
	void recoveryRefundsAnExpiredReservationUsingItsPersistedSourceColumn() throws Exception {
		Fixture fixture = fixture();
		Connection reservedCandidates = mock(Connection.class);
		Connection recovery = mock(Connection.class);
		Connection cleanup = mock(Connection.class);
		PreparedStatement reservedCandidateQuery = mock(PreparedStatement.class);
		PreparedStatement recoverySelect = mock(PreparedStatement.class);
		PreparedStatement recoveryRefund = mock(PreparedStatement.class);
		PreparedStatement recoveryUpdate = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		ResultSet expiredReservation = ids("expired-reservation");
		ResultSet reservedRecovery = recoveryRow("RESERVED", 1L, "source", "Points", 10);
		ResultSet noCleanupCandidates = ids();
		when(reservedCandidates.prepareStatement(anyString())).thenReturn(reservedCandidateQuery);
		when(reservedCandidateQuery.executeQuery()).thenReturn(expiredReservation);
		when(recovery.prepareStatement(anyString())).thenReturn(recoverySelect, recoveryRefund, recoveryUpdate);
		when(recoverySelect.executeQuery()).thenReturn(reservedRecovery);
		when(recoveryRefund.executeUpdate()).thenReturn(1);
		when(recoveryUpdate.executeUpdate()).thenReturn(1);
		when(cleanup.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(noCleanupCandidates);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, reservedCandidates, recovery,
				cleanup);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		journal.recoverAndCleanup(SharedPointTransferJournal.RESERVED_RECOVERY_AGE_MILLIS + 2L);

		verify(recoveryRefund).setString(2, "source");
		verify(recoveryRefund).setInt(1, 10);
		verify(recoveryUpdate).setString(1, "REFUNDED");
		verify(recovery).commit();
	}

	@Test
	void recoveryRechecksAndNeverRefundsAHookStartedRow() throws Exception {
		Fixture fixture = fixture();
		Connection reservedCandidates = mock(Connection.class);
		Connection recovery = mock(Connection.class);
		Connection cleanup = mock(Connection.class);
		PreparedStatement reservedCandidateQuery = mock(PreparedStatement.class);
		PreparedStatement recoverySelect = mock(PreparedStatement.class);
		PreparedStatement recoveryRefund = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		ResultSet claimedCandidate = ids("claimed-transfer");
		ResultSet claimedHook = recoveryRow("HOOK_STARTED", 1L, "source", "Points", 10);
		ResultSet noCleanupCandidates = ids();
		when(reservedCandidates.prepareStatement(anyString())).thenReturn(reservedCandidateQuery);
		when(reservedCandidateQuery.executeQuery()).thenReturn(claimedCandidate);
		when(recovery.prepareStatement(anyString())).thenReturn(recoverySelect, recoveryRefund);
		when(recoverySelect.executeQuery()).thenReturn(claimedHook);
		when(cleanup.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(noCleanupCandidates);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, reservedCandidates,
				recovery, cleanup);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		journal.recoverAndCleanup(1L);

		verify(recoveryRefund, org.mockito.Mockito.never()).executeUpdate();
	}

	@Test
	void cleanupDeletesOnlyTheSelectedBoundedTerminalRows() throws Exception {
		Fixture fixture = fixture();
		Connection reservedCandidates = mock(Connection.class);
		Connection cleanup = mock(Connection.class);
		PreparedStatement reservedCandidateQuery = mock(PreparedStatement.class);
		PreparedStatement cleanupSelect = mock(PreparedStatement.class);
		PreparedStatement cleanupDelete = mock(PreparedStatement.class);
		ResultSet noReservedCandidates = ids();
		ResultSet oldCompleted = ids("old-completed");
		when(reservedCandidates.prepareStatement(anyString())).thenReturn(reservedCandidateQuery);
		when(reservedCandidateQuery.executeQuery()).thenReturn(noReservedCandidates);
		when(cleanup.prepareStatement(anyString())).thenReturn(cleanupSelect, cleanupDelete);
		when(cleanupSelect.executeQuery()).thenReturn(oldCompleted);
		when(cleanupDelete.executeUpdate()).thenReturn(1);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, reservedCandidates, cleanup);

		SharedPointTransferJournal journal = new SharedPointTransferJournal(fixture.table);
		journal.recoverAndCleanup(SharedPointTransferJournal.TERMINAL_RETENTION_MILLIS + 2L);

		verify(cleanupDelete).setString(1, "old-completed");
		verify(cleanupSelect).setInt(4, 100);
		verify(cleanupDelete).executeUpdate();
	}

	private static ResultSet row(String state, String owner) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(state);
		when(row.getString(2)).thenReturn(owner);
		return row;
	}

	private static ResultSet transferRow(String source, String target, int debit, int requestedCredit, String state)
			throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(source);
		when(row.getString(2)).thenReturn(target);
		when(row.getInt(3)).thenReturn(debit);
		when(row.getInt(4)).thenReturn(requestedCredit);
		when(row.getString(5)).thenReturn(state);
		return row;
	}

	private static ResultSet ids(String... transferIds) throws Exception {
		ResultSet rows = mock(ResultSet.class);
		Boolean[] next = new Boolean[transferIds.length + 1];
		for (int index = 0; index < transferIds.length; index++) {
			next[index] = Boolean.TRUE;
		}
		next[transferIds.length] = Boolean.FALSE;
		when(rows.next()).thenReturn(next[0], java.util.Arrays.copyOfRange(next, 1, next.length));
		for (int index = 0; index < transferIds.length; index++) {
			when(rows.getString(1)).thenReturn(transferIds[index]);
		}
		return rows;
	}

	private static ResultSet recoveryRow(String state, long createdAt, String sourceUuid, String sourceColumn,
			int debitPoints) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(state);
		when(row.getLong(2)).thenReturn(createdAt);
		when(row.getString(3)).thenReturn(sourceUuid);
		when(row.getString(4)).thenReturn(sourceColumn);
		when(row.getInt(5)).thenReturn(debitPoints);
		return row;
	}

	private static Fixture fixture() throws Exception {
		Fixture fixture = new Fixture();
		fixture.table = mock(MySQL.class);
		fixture.sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		fixture.schema = mock(Connection.class);
		fixture.lookup = mock(Connection.class);
		fixture.reservation = mock(Connection.class);
		when(fixture.table.getTableName()).thenReturn("VotingPlugin_Users");
		when(fixture.table.qi(anyString())).thenAnswer(invocation -> "`" + invocation.getArgument(0) + "`");
		when(fixture.table.getMysql()).thenReturn(fixture.sql);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.schema, fixture.lookup,
				fixture.reservation);
		when(fixture.schema.prepareStatement(anyString())).thenReturn(mock(PreparedStatement.class));
		return fixture;
	}

	private static final class Fixture {
		MySQL table;
		com.bencodez.simpleapi.sql.mysql.MySQL sql;
		Connection schema;
		Connection lookup;
		Connection reservation;
	}
}
