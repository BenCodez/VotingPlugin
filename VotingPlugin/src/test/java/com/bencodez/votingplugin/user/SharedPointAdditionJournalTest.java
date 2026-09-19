package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

class SharedPointAdditionJournalTest {
	@Test
	void conditionalDebitIsJournaledAndCanBeRetriedWithoutASecondDebit() throws Exception {
		Fixture fixture = fixture();
		Connection missing = missingLookup();
		Attempt debit = successfulAttempt(7);
		Connection completed = completedLookup("player", "Points", -3, 7);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(missing, debit.connection(), completed);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertEquals(7, journal.subtract("admin-remove", "player", "Points", 3, 100L).total());
		assertEquals(7, journal.subtract("admin-remove", "player", "Points", 3, 101L).total());

		verify(debit.credit(), times(1)).executeUpdate();
		verify(debit.credit()).setInt(1, -3);
		verify(debit.credit()).setInt(3, 3);
	}

	@Test
	void conditionalDebitRejectsMissingOrInsufficientUserWithoutCompletingTheJournal() throws Exception {
		Fixture fixture = fixture();
		Connection missing = missingLookup();
		Connection attempt = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		when(debit.executeUpdate()).thenReturn(0);
		when(attempt.prepareStatement(anyString())).thenReturn(insert, debit, read, complete);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(missing, attempt);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertThrows(SharedPointAdditionJournal.DebitRejectedException.class,
				() -> journal.subtract("admin-remove", "player", "Points", 11, 100L));
		verify(attempt, atLeastOnce()).rollback();
		verify(complete, org.mockito.Mockito.never()).executeUpdate();
	}

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
	void completedOperationCanBeFoundBeforeReplayingTheReceiveHook() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet completed = completedRow("player", "Points", 7, 17);
		when(lookup.executeQuery()).thenReturn(completed);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);

		SharedPointAdditionJournal.AdditionResult result = new SharedPointAdditionJournal(fixture.table, false)
				.findCompleted("reward-operation", "player", "Points");

		assertEquals(17, result.total());
		verify(lookup).setString(1, "reward-operation");
		verify(fixture.firstAttempt, org.mockito.Mockito.never()).prepareStatement(anyString());
	}

	@Test
	void claimedHookPreventsAnotherBackendFromReplayingTheReceiveEvent() throws Exception {
		Fixture fixture = fixture();
		Connection missing = missingLookup();
		Connection claim = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		when(claim.prepareStatement(anyString())).thenReturn(insert);
		Connection otherBackend = mock(Connection.class);
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		when(lookup.executeQuery()).thenReturn(claimed);
		when(otherBackend.prepareStatement(anyString())).thenReturn(lookup);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(missing, claim, otherBackend);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertTrue(journal.claimHook("reward-operation", "player", "Points", 5, "first-backend", 100L).claimed());
		SharedPointAdditionJournal.HookClaim duplicate = journal.claimHook("reward-operation", "player", "Points", 5,
				"second-backend", 101L);

		assertFalse(duplicate.claimed());
		assertFalse(duplicate.completed());
		assertTrue(duplicate.requiresReconciliation());
		verify(insert, times(1)).executeUpdate();
	}

	@Test
	void rejectedOrStoppedHookClaimIsDurablyMarkedForManualReconciliation() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement update = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		when(select.executeQuery()).thenReturn(claimed);
		when(update.executeUpdate()).thenReturn(1);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, update);

		new SharedPointAdditionJournal(fixture.table, false).markIndeterminate("reward-operation", "player",
				"Points", 5, "first-backend");

		verify(update).setString(1, "INDETERMINATE");
		verify(update).setString(2, "reward-operation");
		verify(update).setString(3, "HOOK_STARTED");
		verify(update).setString(4, "first-backend");
		verify(fixture.initialLookup).commit();
	}

	@Test
	void provenUnstartedHookClaimIsReleasedForASafeRetry() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement delete = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		when(select.executeQuery()).thenReturn(claimed);
		when(delete.executeUpdate()).thenReturn(1);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, delete);

		new SharedPointAdditionJournal(fixture.table, false).releaseUnstartedHook("reward-operation", "player",
				"Points", 5, "first-backend");

		verify(delete).setString(1, "reward-operation");
		verify(delete).setString(2, "HOOK_STARTED");
		verify(delete).setString(3, "first-backend");
		verify(delete).executeUpdate();
		verify(fixture.initialLookup).commit();
	}

	@Test
	void ambiguousHookClaimReleasesTheExactUnstartedOwnerBeforeFailing() throws Exception {
		Fixture fixture = fixture();
		Connection missing = missingLookup();
		Connection claim = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		Connection release = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement delete = mock(PreparedStatement.class);
		when(claim.prepareStatement(anyString())).thenReturn(insert);
		doThrow(new java.sql.SQLException("claim acknowledgement lost")).when(claim).commit();
		when(confirmation.prepareStatement(anyString()))
				.thenThrow(new java.sql.SQLException("confirmation unavailable"));
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		when(select.executeQuery()).thenReturn(claimed);
		when(delete.executeUpdate()).thenReturn(1);
		when(release.prepareStatement(anyString())).thenReturn(select, delete);
		when(fixture.sql.getConnectionManager().getConnection())
				.thenReturn(missing, claim, confirmation, release);

		assertThrows(java.sql.SQLException.class, () -> new SharedPointAdditionJournal(fixture.table, false)
				.claimHook("reward-operation", "player", "Points", 5, "first-backend", 100L));

		verify(delete).setString(1, "reward-operation");
		verify(delete).setString(2, "HOOK_STARTED");
		verify(delete).setString(3, "first-backend");
		verify(delete).executeUpdate();
		verify(release).commit();
	}

	@Test
	void ambiguousUnstartedHookReleaseCommitIsConfirmedAsSafeAfterRestart() throws Exception {
		Fixture fixture = fixture();
		Connection release = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement delete = mock(PreparedStatement.class);
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		ResultSet missing = mock(ResultSet.class);
		when(claimed.next()).thenReturn(true);
		when(select.executeQuery()).thenReturn(claimed);
		when(delete.executeUpdate()).thenReturn(1);
		when(release.prepareStatement(anyString())).thenReturn(select, delete);
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(release).commit();
		when(missing.next()).thenReturn(false);
		when(lookup.executeQuery()).thenReturn(missing);
		when(confirmation.prepareStatement(anyString())).thenReturn(lookup);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(release, confirmation);

		new SharedPointAdditionJournal(fixture.table, false).releaseUnstartedHook("reward-operation", "player",
				"Points", 5, "first-backend");

		verify(release, atLeastOnce()).close();
		verify(lookup).setString(1, "reward-operation");
	}

	@Test
	void restartedBackendReportsIndeterminateClaimInsteadOfWaitingForADeadOwner() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet indeterminate = hookStartedRow("player", "Points", 5, "stopped-backend");
		when(indeterminate.getString(4)).thenReturn("INDETERMINATE");
		when(lookup.executeQuery()).thenReturn(indeterminate);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);

		SharedPointAdditionJournal.HookClaim claim = new SharedPointAdditionJournal(fixture.table, false)
				.claimHook("reward-operation", "player", "Points", 5, "restarted-backend", 101L);

		assertFalse(claim.claimed());
		assertTrue(claim.requiresReconciliation());
	}

	@Test
	void liveForeignHookClaimIsNotPreemptedBeforeItsRecoveryLeaseExpires() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet live = hookStartedRow("player", "Points", 5, "live-backend", 100L);
		when(lookup.executeQuery()).thenReturn(live);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);

		SharedPointAdditionJournal.HookClaim claim = new SharedPointAdditionJournal(fixture.table, false)
				.claimHook("reward-operation", "player", "Points", 5, "replacement",
						100L + SharedPointAdditionJournal.HOOK_RECOVERY_LEASE_MILLIS - 1L);

		assertFalse(claim.claimed());
		assertTrue(claim.requiresReconciliation());
		verify(fixture.sql.getConnectionManager(), times(1)).getConnection();
	}

	@Test
	void staleForeignHookClaimTransitionsDurablyToReconciliation() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet stale = hookStartedRow("player", "Points", 5, "stopped-backend", 100L);
		when(lookup.executeQuery()).thenReturn(stale);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);
		Connection transition = mock(Connection.class);
		PreparedStatement update = mock(PreparedStatement.class);
		when(update.executeUpdate()).thenReturn(1);
		when(transition.prepareStatement(anyString())).thenReturn(update);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.initialLookup, transition);
		long now = 100L + SharedPointAdditionJournal.HOOK_RECOVERY_LEASE_MILLIS;

		SharedPointAdditionJournal.HookClaim claim = new SharedPointAdditionJournal(fixture.table, false)
				.claimHook("reward-operation", "player", "Points", 5, "replacement", now);

		assertFalse(claim.claimed());
		assertTrue(claim.requiresReconciliation());
		verify(update).setString(1, "INDETERMINATE");
		verify(update).setString(2, "reward-operation");
		verify(update).setString(3, "HOOK_STARTED");
		verify(update).setLong(4, 100L);
		verify(transition).commit();
	}

	@Test
	void cancelledHookSettlesWithARepresentableZeroCredit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "owner");
		ResultSet total = mock(ResultSet.class);
		when(total.next()).thenReturn(true);
		when(total.getInt(1)).thenReturn(12);
		when(select.executeQuery()).thenReturn(claimed);
		when(read.executeQuery()).thenReturn(total);
		when(complete.executeUpdate()).thenReturn(1);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, read, complete);

		assertEquals(12, new SharedPointAdditionJournal(fixture.table, false).settleClaim("reward-operation",
				"player", "Points", "Points", 5, "owner", null).total());

		verify(complete).setInt(1, 0);
		verify(complete, org.mockito.Mockito.never()).setNull(org.mockito.ArgumentMatchers.eq(1),
				org.mockito.ArgumentMatchers.anyInt());
	}

	@Test
	void perServerSettlementCreditsTheLocalColumnAndCompletesTheSharedClaimAtomically() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "owner");
		ResultSet total = mock(ResultSet.class);
		when(credit.executeUpdate()).thenReturn(1);
		when(total.next()).thenReturn(true);
		when(total.getInt(1)).thenReturn(17);
		when(select.executeQuery()).thenReturn(claimed);
		when(read.executeQuery()).thenReturn(total);
		when(complete.executeUpdate()).thenReturn(1);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, credit, read, complete);

		assertEquals(17, new SharedPointAdditionJournal(fixture.table, false).settleClaim("reward-operation",
				"player", "Points", "lobby_Points", 5, "owner", Integer.valueOf(3)).total());

		verify(credit).setInt(1, 3);
		verify(credit).setString(2, "player");
		verify(complete).setInt(1, 3);
		verify(complete).setString(2, "COMPLETED");
		verify(complete).setInt(3, 17);
		org.mockito.ArgumentCaptor<String> statements = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.initialLookup, times(4)).prepareStatement(statements.capture());
		assertTrue(statements.getAllValues().stream().anyMatch(statement -> statement.startsWith(
				"UPDATE `VotingPlugin_Users` SET `lobby_Points` = COALESCE(`lobby_Points`, 0) + ?")));
		assertTrue(statements.getAllValues().stream().anyMatch(statement -> statement.startsWith(
				"SELECT `lobby_Points` FROM `VotingPlugin_Users`")));
		assertTrue(statements.getAllValues().stream().noneMatch(statement -> statement.startsWith(
				"UPDATE `VotingPlugin_Users` SET `Points` = `Points` + ?")));
		verify(fixture.initialLookup).commit();
	}

	@Test
	void failedPerServerLocalCreditRollsBackBeforeTheJournalCanComplete() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement credit = mock(PreparedStatement.class);
		PreparedStatement read = mock(PreparedStatement.class);
		PreparedStatement complete = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "owner");
		when(select.executeQuery()).thenReturn(claimed);
		when(credit.executeUpdate()).thenThrow(new java.sql.SQLException("local write failed"));
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, credit, read, complete);

		assertThrows(java.sql.SQLException.class, () -> new SharedPointAdditionJournal(fixture.table, false)
				.settleClaim("reward-operation", "player", "Points", "lobby_Points", 5, "owner",
						Integer.valueOf(3)));

		verify(fixture.initialLookup, atLeastOnce()).rollback();
		verify(complete, org.mockito.Mockito.never()).executeUpdate();
	}

	@Test
	void settlementRejectsAnUnsafePhysicalCreditColumnBeforeOpeningSql() throws Exception {
		Fixture fixture = fixture();

		assertThrows(java.sql.SQLException.class, () -> new SharedPointAdditionJournal(fixture.table, false)
				.settleClaim("reward-operation", "player", "Points", "lobby\0Points", 5, "owner",
						Integer.valueOf(3)));
		verify(fixture.sql.getConnectionManager(), org.mockito.Mockito.never()).getConnection();
	}

	@Test
	void claimedHookRejectsAConflictingRequestedAmountBeforeAnotherEventCanRun() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement lookup = mock(PreparedStatement.class);
		ResultSet claimed = hookStartedRow("player", "Points", 5, "first-backend");
		when(lookup.executeQuery()).thenReturn(claimed);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(lookup);

		SharedPointAdditionJournal journal = new SharedPointAdditionJournal(fixture.table, false);
		assertThrows(java.sql.SQLException.class,
				() -> journal.claimHook("reward-operation", "player", "Points", 6, "second-backend", 101L));
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

	@Test
	void onlyAcknowledgedOrEphemeralAdminEntriesExpireInABoundedRetentionBatch() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement delete = mock(PreparedStatement.class);
		ResultSet completed = mock(ResultSet.class);
		when(completed.next()).thenReturn(true, true, false);
		when(completed.getString(1)).thenReturn("old-one", "old-two");
		when(select.executeQuery()).thenReturn(completed);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(select, delete);

		long now = TimeUnit.DAYS.toMillis(10);
		new SharedPointAdditionJournal(fixture.table, false).cleanupAcknowledged(now);

		verify(select).setString(1, "ACKNOWLEDGED");
		verify(select).setString(2, "COMPLETED");
		verify(select).setString(3, "admin-points/%");
		verify(select).setString(4, "admin-bulk-points/%");
		verify(select).setString(5, "admin-bulk-remove/%");
		verify(select).setString(6, "remove-points/%");
		verify(select).setLong(7, now - SharedPointAdditionJournal.COMPLETED_RETENTION_MILLIS);
		verify(select).setInt(8, 100);
		verify(delete, times(2)).setString(3, "ACKNOWLEDGED");
		verify(delete, times(2)).setString(4, "COMPLETED");
		verify(delete, times(2)).executeUpdate();
	}

	@Test
	void durableReplayCheckpointAcknowledgesAnAdditionBeforeRetentionStarts() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement update = mock(PreparedStatement.class);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(update);

		new SharedPointAdditionJournal(fixture.table, false).acknowledge("reward-operation", 123L);

		verify(update).setString(1, "ACKNOWLEDGED");
		verify(update).setLong(2, 123L);
		verify(update).setString(3, "reward-operation");
		verify(update).setString(4, "COMPLETED");
		verify(update).executeUpdate();
	}

	@Test
	void schemaIndexesTheBoundedCleanupPredicate() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement createTable = mock(PreparedStatement.class);
		PreparedStatement addRequestedAmount = mock(PreparedStatement.class);
		PreparedStatement addHookOwner = mock(PreparedStatement.class);
		PreparedStatement createIndex = mock(PreparedStatement.class);
		when(fixture.initialLookup.prepareStatement(anyString())).thenReturn(createTable, addRequestedAmount,
				addHookOwner, createIndex);

		new SharedPointAdditionJournal(fixture.table, true);

		org.mockito.ArgumentCaptor<String> statements = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.initialLookup, times(4)).prepareStatement(statements.capture());
		assertTrue(statements.getAllValues().get(3).contains("(`state`, `created_at`)"));
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

	private static ResultSet hookStartedRow(String uuid, String pointsColumn, int requestedAmount, String owner)
			throws Exception {
		return hookStartedRow(uuid, pointsColumn, requestedAmount, owner, 0L);
	}

	private static ResultSet hookStartedRow(String uuid, String pointsColumn, int requestedAmount, String owner,
			long createdAt) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(uuid);
		when(row.getString(2)).thenReturn(pointsColumn);
		when(row.getInt(3)).thenReturn(requestedAmount);
		when(row.getString(4)).thenReturn("HOOK_STARTED");
		when(row.getObject(5)).thenReturn(null);
		when(row.getObject(6)).thenReturn(Integer.valueOf(requestedAmount));
		when(row.getInt(6)).thenReturn(requestedAmount);
		when(row.getString(7)).thenReturn(owner);
		when(row.getLong(8)).thenReturn(createdAt);
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
