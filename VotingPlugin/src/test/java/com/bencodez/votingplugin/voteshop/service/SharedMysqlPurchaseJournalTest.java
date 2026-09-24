package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

class SharedMysqlPurchaseJournalTest {
	@Test
	void dailyStreakResetCompletionFollowsThePersistedCopyMarker() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement copyInsert = mock(PreparedStatement.class);
		PreparedStatement copySelect = mock(PreparedStatement.class);
		PreparedStatement resetInsert = mock(PreparedStatement.class);
		PreparedStatement resetSelect = mock(PreparedStatement.class);
		PreparedStatement update = mock(PreparedStatement.class);
		ResultSet copyMarker = mock(ResultSet.class);
		ResultSet resetMarker = mock(ResultSet.class);
		when(copyMarker.next()).thenReturn(true);
		when(copyMarker.getString(2)).thenReturn("time-streak-copy:DAY:2026-09-22");
		when(resetMarker.next()).thenReturn(true);
		when(resetMarker.getLong(1)).thenReturn(3L);
		when(copySelect.executeQuery()).thenReturn(copyMarker);
		when(resetSelect.executeQuery()).thenReturn(resetMarker);
		when(update.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(copyInsert, copySelect, resetInsert, resetSelect,
				update);

		new SharedMysqlPurchaseJournal(fixture.table, false)
				.completeDailyStreakReset("time-streak-reset:DAY:2026-09-22");

		verify(update).setLong(1, 4L);
		verify(update).setString(2, "time-streak-reset:DAY:2026-09-22");
		verify(fixture.work).commit();
	}

	@Test
	void voteAdmissionDurablyIncludesEligibleDailyStreakAndReward() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement copyInsert = mock(PreparedStatement.class);
		PreparedStatement copySelect = mock(PreparedStatement.class);
		PreparedStatement resetInsert = mock(PreparedStatement.class);
		PreparedStatement resetSelect = mock(PreparedStatement.class);
		PreparedStatement accountingInsert = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement candidateSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet copyEpoch = mock(ResultSet.class);
		ResultSet resetEpoch = mock(ResultSet.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 0, 0);
		ResultSet candidate = mock(ResultSet.class);
		when(copyEpoch.next()).thenReturn(true);
		when(resetEpoch.next()).thenReturn(true);
		when(copySelect.executeQuery()).thenReturn(copyEpoch);
		when(resetSelect.executeQuery()).thenReturn(resetEpoch);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(candidate.next()).thenReturn(true);
		when(candidate.getInt(1)).thenReturn(0);
		when(candidate.getInt(2)).thenReturn(4);
		when(candidate.getString(3)).thenReturn("");
		when(candidateSelect.executeQuery()).thenReturn(candidate);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(copyInsert, copySelect, resetInsert, resetSelect,
				accountingInsert, accountingSelect, candidateSelect, accountingUpdate);

		int requested = new SharedMysqlPurchaseJournal(fixture.table, false).prepareVoteAccounting(
				java.util.UUID.randomUUID(), "00000000-0000-0000-0000-000000000001", false, true, false,
				null, null, false, 0.0, 1, true, 1234L);

		assertEquals(304, requested);
		verify(accountingUpdate).setInt(2, 368);
		verify(accountingUpdate).setInt(3, 320);
		verify(accountingUpdate).setInt(6, 5);
		verify(accountingUpdate).setLong(7, 1234L);
		verify(accountingUpdate).setInt(8, 1);
		verify(fixture.work).commit();
	}

	@Test
	void repeatedVoteAdmissionReturnsThePersistedPointsDecision() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement copyInsert = mock(PreparedStatement.class);
		PreparedStatement copySelect = mock(PreparedStatement.class);
		PreparedStatement resetInsert = mock(PreparedStatement.class);
		PreparedStatement resetSelect = mock(PreparedStatement.class);
		PreparedStatement accountingInsert = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet copyEpoch = mock(ResultSet.class);
		ResultSet resetEpoch = mock(ResultSet.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 320, 64, 9, 40, true);
		when(copyEpoch.next()).thenReturn(true);
		when(resetEpoch.next()).thenReturn(true);
		when(copySelect.executeQuery()).thenReturn(copyEpoch);
		when(resetSelect.executeQuery()).thenReturn(resetEpoch);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(copyInsert, copySelect, resetInsert, resetSelect,
				accountingInsert, accountingSelect, accountingUpdate);

		SharedMysqlPurchaseJournal.VoteAccountingDecision decision = new SharedMysqlPurchaseJournal(fixture.table, false)
				.prepareVoteAccounting(
				UUID.randomUUID(), "00000000-0000-0000-0000-000000000001", false, false, false,
				null, null, false, 0.0, 1, false, 1234L, 2, 10, "current_Points");

		assertEquals(256, decision.bits());
		assertEquals(9, decision.pointAmount());
		assertEquals(40, decision.pointCap());
		assertEquals("persisted_Points", decision.pointColumn());
		assertEquals(true, decision.replayUnsafe());
		verify(accountingUpdate).setInt(2, 320);
		verify(accountingUpdate).setInt(3, 320);
		verify(accountingUpdate).setString(11, "persisted_Points");
		verify(fixture.work).commit();
	}

	@Test
	void pendingDailyTotalsContributeToPercentageStreakAdmission() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement dailyCopyInsert = mock(PreparedStatement.class);
		PreparedStatement dailyCopySelect = mock(PreparedStatement.class);
		PreparedStatement copyInsert = mock(PreparedStatement.class);
		PreparedStatement copySelect = mock(PreparedStatement.class);
		PreparedStatement resetInsert = mock(PreparedStatement.class);
		PreparedStatement resetSelect = mock(PreparedStatement.class);
		PreparedStatement accountingInsert = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement dailyResetInsert = mock(PreparedStatement.class);
		PreparedStatement dailyResetSelect = mock(PreparedStatement.class);
		PreparedStatement candidateSelect = mock(PreparedStatement.class);
		PreparedStatement pendingTotalSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet dailyCopyEpoch = mock(ResultSet.class);
		ResultSet copyEpoch = mock(ResultSet.class);
		ResultSet resetEpoch = mock(ResultSet.class);
		ResultSet dailyResetEpoch = mock(ResultSet.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 0, 0);
		ResultSet candidate = mock(ResultSet.class);
		ResultSet pendingTotals = mock(ResultSet.class);
		when(dailyCopyEpoch.next()).thenReturn(true);
		when(dailyCopyEpoch.getString(2)).thenReturn("time-copy:DAY:2026-09-22");
		when(copyEpoch.next()).thenReturn(true);
		when(resetEpoch.next()).thenReturn(true);
		when(dailyResetEpoch.next()).thenReturn(true);
		when(dailyResetEpoch.getString(2)).thenReturn("time-total:DAY:2026-09-21");
		when(dailyCopySelect.executeQuery()).thenReturn(dailyCopyEpoch);
		when(copySelect.executeQuery()).thenReturn(copyEpoch);
		when(resetSelect.executeQuery()).thenReturn(resetEpoch);
		when(dailyResetSelect.executeQuery()).thenReturn(dailyResetEpoch);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(candidate.next()).thenReturn(true);
		when(candidate.getInt(1)).thenReturn(0);
		when(candidate.getInt(2)).thenReturn(4);
		when(candidate.getString(3)).thenReturn("");
		when(candidateSelect.executeQuery()).thenReturn(candidate);
		when(pendingTotals.next()).thenReturn(true);
		when(pendingTotals.getInt(1)).thenReturn(1);
		when(pendingTotalSelect.executeQuery()).thenReturn(pendingTotals);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(dailyCopyInsert, dailyCopySelect,
				copyInsert, copySelect, resetInsert, resetSelect, accountingInsert, accountingSelect,
				dailyResetInsert, dailyResetSelect, candidateSelect, pendingTotalSelect, accountingUpdate);

		int requested = new SharedMysqlPurchaseJournal(fixture.table, false).prepareVoteAccounting(
				UUID.randomUUID(), "00000000-0000-0000-0000-000000000001", false, false, false,
				null, null, true, 50.0, 1, false, 1234L);

		assertEquals(48, requested);
		verify(accountingUpdate).setInt(2, 112);
		verify(pendingTotalSelect).setInt(5, 64);
		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(13)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(10).contains("LastDailyTotal"),
				"active daily reset must exclude the copied boundary total");
	}

	@Test
	void firstVoteCanBeAdmittedBeforeTheUserRowIsCreated() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement copyInsert = mock(PreparedStatement.class);
		PreparedStatement copySelect = mock(PreparedStatement.class);
		PreparedStatement resetInsert = mock(PreparedStatement.class);
		PreparedStatement resetSelect = mock(PreparedStatement.class);
		PreparedStatement accountingInsert = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement candidateSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet copyEpoch = mock(ResultSet.class);
		ResultSet resetEpoch = mock(ResultSet.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 0, 0);
		ResultSet missingCandidate = mock(ResultSet.class);
		when(copyEpoch.next()).thenReturn(true);
		when(resetEpoch.next()).thenReturn(true);
		when(copySelect.executeQuery()).thenReturn(copyEpoch);
		when(resetSelect.executeQuery()).thenReturn(resetEpoch);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(missingCandidate.next()).thenReturn(false);
		when(candidateSelect.executeQuery()).thenReturn(missingCandidate);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(copyInsert, copySelect, resetInsert, resetSelect,
				accountingInsert, accountingSelect, candidateSelect, accountingUpdate);

		int requested = new SharedMysqlPurchaseJournal(fixture.table, false).prepareVoteAccounting(
				java.util.UUID.randomUUID(), "00000000-0000-0000-0000-000000000001", false, false, false,
				null, null, false, 0.0, 1, false, 1234L);

		assertEquals(48, requested);
		verify(accountingUpdate).setInt(6, 1);
		verify(fixture.work).commit();
	}

	@Test
	void completedDailyStreakCanClaimItsDeferredRewardOnce() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 48, 16);
		when(accounting.getObject(8)).thenReturn(Integer.valueOf(1));
		when(accounting.getObject(9)).thenReturn(Integer.valueOf(7));
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(accountingSelect, accountingUpdate);
		java.util.UUID voteId = java.util.UUID.randomUUID();

		SharedMysqlPurchaseJournal.RecoveredDailyStreak reward =
				new SharedMysqlPurchaseJournal(fixture.table, false).claimDailyStreakReward(voteId);

		assertEquals(voteId, reward.voteId());
		assertEquals(7, reward.streak());
		assertTrue(reward.forceProxyRouting());
		verify(accountingUpdate).setInt(1, 144);
		verify(fixture.work).commit();
	}

	@Test
	void claimedDailyStreakRewardCompletesOnlyAfterDeliveryReturns() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 48, 144);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(accountingSelect, accountingUpdate);

		new SharedMysqlPurchaseJournal(fixture.table, false)
				.completeDailyStreakReward(java.util.UUID.randomUUID());

		verify(accountingUpdate).setInt(1, 48);
		verify(fixture.work).commit();
	}

	@Test
	void ambiguousDailyStreakRewardIsNotClaimedAgain() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 48, 144);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(fixture.work.prepareStatement(anyString())).thenReturn(accountingSelect);

		assertThrows(java.sql.SQLException.class, () -> new SharedMysqlPurchaseJournal(fixture.table, false)
				.claimDailyStreakReward(java.util.UUID.randomUUID()));

		verify(fixture.work).rollback();
	}

	@Test
	void dailyStreakDateFenceRecognizesTheSameLocalDay() {
		java.time.ZoneId zone = java.time.ZoneId.systemDefault();
		long morning = java.time.LocalDate.of(2026, 9, 22).atTime(1, 0).atZone(zone).toInstant().toEpochMilli();
		long evening = java.time.LocalDate.of(2026, 9, 22).atTime(23, 0).atZone(zone).toInstant().toEpochMilli();
		long nextDay = java.time.LocalDate.of(2026, 9, 23).atStartOfDay(zone).toInstant().toEpochMilli();

		assertTrue(SharedMysqlPurchaseJournal.sameLocalDay(Long.toString(morning), evening));
		assertFalse(SharedMysqlPurchaseJournal.sameLocalDay(Long.toString(morning), nextDay));
	}
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
		assertTrue(SharedMysqlPurchaseJournal.epochTableName(source).matches("vp_vse_[0-9a-f]{32}"));
		assertEquals("VotingPlugin_Users_VoteShopLimitEpochs",
				SharedMysqlPurchaseJournal.epochTableName("VotingPlugin_Users"));
	}

	@Test
	void reservationPersistsPendingDebitInTheSameTransaction() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(3L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, insert, debit);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.reserve("purchase-1", "player", "Points", "VoteShopLimitdaily", 10, 1,
				SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION, 0L, 100L));

		verify(insert).setString(7, SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION);
		verify(insert).setLong(9, 3L);
		verify(insert).setString(10, "PENDING");
		verify(debit).setInt(1, 10);
		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(4)).prepareStatement(sql.capture());
		String conditionalDebit = sql.getAllValues().get(3);
		assertTrue(conditionalDebit.contains("`Points` = COALESCE(`Points`, 0) - ?"));
		assertTrue(conditionalDebit.contains("COALESCE(`Points`, 0) >= ?"));
		assertTrue(conditionalDebit.contains("COALESCE(`VoteShopLimitdaily`, 0) < ?"));
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
		var refunded = journal.recoverAndCleanup(SharedMysqlPurchaseJournal.PENDING_RECOVERY_AGE_MILLIS + 1L);

		verify(credit).setInt(1, 10);
		verify(credit).setString(2, "player");
		verify(terminal).setString(1, "REFUNDED");
		verify(refund).commit();
		assertEquals(1, refunded.size());
		assertEquals("player", refunded.get(0).uuid());
		assertEquals("Points", refunded.get(0).pointsColumn());
		assertEquals("VoteShopLimitdaily", refunded.get(0).limitColumn());
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
	void legacyRefundRestoresPointsWithoutDecrementingAResettableLimit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		when(pending.getString(6)).thenReturn("D:2026-09-08");
		when(pending.getLong(7)).thenReturn(Long.MAX_VALUE);
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
	void reservationLocksTheCurrentEpochBeforeItsConditionalDebit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement debit = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(7L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(debit.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, insert, debit);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.reserve("epoch-purchase", "player", "Points", "VoteShopLimitdaily", 10, 1,
				"D:2026-09-08", 100L, 10L));

		verify(insert).setLong(9, 7L);
		org.mockito.InOrder lockBeforeDebit = org.mockito.Mockito.inOrder(markerSelect, debit);
		lockBeforeDebit.verify(markerSelect).executeQuery();
		lockBeforeDebit.verify(debit).executeUpdate();
	}

	@Test
	void epochMismatchRefundRestoresPointsWithoutTouchingTheNewLimit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement epochSelect = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		ResultSet currentEpoch = mock(ResultSet.class);
		when(pending.getObject(8)).thenReturn(4L);
		when(currentEpoch.next()).thenReturn(true);
		when(currentEpoch.getLong(1)).thenReturn(5L);
		when(select.executeQuery()).thenReturn(pending);
		when(epochSelect.executeQuery()).thenReturn(currentEpoch);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, epochSelect, refund, terminal);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		assertTrue(journal.refundPending("old-epoch", 100L));

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(4)).prepareStatement(sql.capture());
		assertFalse(sql.getAllValues().get(2).contains("`VoteShopLimitdaily` = GREATEST"));
	}

	@Test
	void matchingEpochRefundReleasesTheReservedLimit() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement epochSelect = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		ResultSet currentEpoch = mock(ResultSet.class);
		when(pending.getObject(8)).thenReturn(5L);
		when(currentEpoch.next()).thenReturn(true);
		when(currentEpoch.getLong(1)).thenReturn(5L);
		when(select.executeQuery()).thenReturn(pending);
		when(epochSelect.executeQuery()).thenReturn(currentEpoch);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, epochSelect, refund, terminal);

		assertTrue(new SharedMysqlPurchaseJournal(fixture.table, false).refundPending("current-epoch", 100L));

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(4)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(2).contains("`VoteShopLimitdaily` = GREATEST"));
	}

	@Test
	void resetRollsBackWhenItsEpochAdvanceDoesNotAffectTheMarker() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement wipe = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(2L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(advance.executeUpdate()).thenReturn(0);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, wipe, advance);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		org.junit.jupiter.api.Assertions.assertThrows(java.sql.SQLException.class,
				() -> journal.resetLimit("VoteShopLimitdaily", "D:2026-09-08"));

		verify(fixture.work).rollback();
		verify(fixture.work, org.mockito.Mockito.never()).commit();
	}

	@Test
	void repeatedResetGenerationDoesNotWipeNewPeriodPurchases() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(3L);
		when(epoch.getString(2)).thenReturn("D:2026-09-08");
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect);

		new SharedMysqlPurchaseJournal(fixture.table, false).resetLimit(
				"VoteShopLimitdaily", "D:2026-09-08");

		verify(fixture.work, org.mockito.Mockito.times(2)).prepareStatement(anyString());
		verify(fixture.work).rollback();
		verify(fixture.work, org.mockito.Mockito.never()).commit();
	}

	@Test
	void periodResetSubtractsTheBoundaryCopyAndAdvancesItsGeneration() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement reset = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(4L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(advance.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, reset, advance);

		new SharedMysqlPurchaseJournal(fixture.table, false).resetPeriodTotal(
				"DailyTotal", "LastDailyTotal", "time-total:DAY:2026-09-21");

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(4)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(2).contains(
				"`DailyTotal` = GREATEST(0, COALESCE(`DailyTotal`, 0) - COALESCE(`LastDailyTotal`, 0))"));
		verify(advance).setLong(1, 5L);
		verify(advance).setString(2, "time-total:DAY:2026-09-21");
		verify(advance).setString(3, "period-reset:DailyTotal");
		verify(fixture.work).commit();
	}

	@Test
	void periodBoundaryCopyAndGenerationAdvanceShareOneTransaction() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement pending = mock(PreparedStatement.class);
		PreparedStatement copy = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet noPending = ids();
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(8L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(pending.executeQuery()).thenReturn(noPending);
		when(advance.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, pending, copy, advance);

		new SharedMysqlPurchaseJournal(fixture.table, false).copyPeriodBoundary(
				"DailyTotal", "LastDailyTotal", "time-copy:DAY:2026-09-21");

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(5)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(3).contains(
				"`LastDailyTotal` = COALESCE(`DailyTotal`, 0)"));
		verify(advance).setLong(1, 9L);
		verify(advance).setString(3, "period-copy:DailyTotal");
		verify(fixture.work).commit();
	}

	@Test
	void periodIncrementLocksTheSameBoundaryRowBeforeUpdatingTheUser() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement requestInsert = mock(PreparedStatement.class);
		PreparedStatement requestSelect = mock(PreparedStatement.class);
		PreparedStatement requestUpdate = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement resetMarkerInsert = mock(PreparedStatement.class);
		PreparedStatement resetMarkerSelect = mock(PreparedStatement.class);
		PreparedStatement increment = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet requested = accountingRow("00000000-0000-0000-0000-000000000001", 0, 0);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 4, 0);
		ResultSet resetEpoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(9L);
		when(epoch.getString(2)).thenReturn("time-copy:MONTH:2026-09");
		when(resetEpoch.next()).thenReturn(true);
		when(resetEpoch.getString(2)).thenReturn("time-total:MONTH:2026-08");
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(requestSelect.executeQuery()).thenReturn(requested);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(resetMarkerSelect.executeQuery()).thenReturn(resetEpoch);
		when(increment.executeUpdate()).thenReturn(1);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(requestUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(requestInsert, requestSelect, requestUpdate,
				markerInsert, markerSelect,
				accountingSelect, resetMarkerInsert, resetMarkerSelect, increment, accountingUpdate);

		new SharedMysqlPurchaseJournal(fixture.table, false).incrementPeriodTotals(java.util.UUID.randomUUID(),
				"00000000-0000-0000-0000-000000000001", "MonthTotal", "LastMonthTotal",
				java.util.List.of("MonthTotal", "MonthTotal-SEPTEMBER-2026"), Integer.valueOf(42));

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(10)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(4).contains("FOR UPDATE"));
		assertTrue(sql.getAllValues().get(8).contains("`MonthTotal` = GREATEST(COALESCE(`MonthTotal`, 0), "
				+ "LEAST(COALESCE(`LastMonthTotal`, 0) + ?, COALESCE(`MonthTotal`, 0) + 1))"));
		assertTrue(sql.getAllValues().get(8).contains("`MonthTotal-SEPTEMBER-2026` = "
				+ "GREATEST(COALESCE(`MonthTotal-SEPTEMBER-2026`, 0), "
				+ "LEAST(?, COALESCE(`MonthTotal-SEPTEMBER-2026`, 0) + 1))"));
		verify(markerSelect).setString(1, "period-copy:MonthTotal");
		verify(increment).setInt(1, 42);
		verify(increment).setInt(2, 42);
		verify(increment).setString(3, "00000000-0000-0000-0000-000000000001");
		verify(fixture.work, org.mockito.Mockito.times(2)).commit();
	}

	@Test
	void repeatedVoteOperationDoesNotIncrementThePeriodTwice() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement requestInsert = mock(PreparedStatement.class);
		PreparedStatement requestSelect = mock(PreparedStatement.class);
		PreparedStatement requestUpdate = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet requested = accountingRow("player", 1, 1);
		ResultSet accounting = accountingRow("player", 1, 1);
		when(epoch.next()).thenReturn(true);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(requestSelect.executeQuery()).thenReturn(requested);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(requestUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(requestInsert, requestSelect, requestUpdate,
				markerInsert, markerSelect,
				accountingSelect);

		new SharedMysqlPurchaseJournal(fixture.table, false).incrementPeriodTotals(java.util.UUID.randomUUID(),
				"player", "DailyTotal", "LastDailyTotal", java.util.List.of("DailyTotal"), null);

		verify(fixture.work).rollback();
		verify(fixture.work).commit();
		verify(fixture.work, org.mockito.Mockito.times(6)).prepareStatement(anyString());
	}

	@Test
	void repeatedVoteOperationDoesNotIncrementAllTimeTotalTwice() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement requestInsert = mock(PreparedStatement.class);
		PreparedStatement requestSelect = mock(PreparedStatement.class);
		PreparedStatement requestUpdate = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet requested = accountingRow("player", 512, 512);
		ResultSet accounting = accountingRow("player", 512, 512);
		when(epoch.next()).thenReturn(true);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(requestSelect.executeQuery()).thenReturn(requested);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(requestUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(requestInsert, requestSelect, requestUpdate,
				markerInsert, markerSelect, accountingSelect);

		new SharedMysqlPurchaseJournal(fixture.table, false).incrementPeriodTotals(UUID.randomUUID(),
				"player", "AllTimeTotal", "AllTimeTotal", List.of("AllTimeTotal"), null);

		verify(fixture.work).rollback();
		verify(fixture.work).commit();
		verify(fixture.work, org.mockito.Mockito.times(6)).prepareStatement(anyString());
	}

	@Test
	void admittedMonthlyIncrementUsesItsJournaledColumnAfterConfigurationChanges() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement admittedSelect = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement increment = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		ResultSet admitted = accountingRow("player", 4, 0);
		when(admitted.getString(4)).thenReturn("MonthTotal-SEPTEMBER-2026");
		when(admitted.wasNull()).thenReturn(true);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		ResultSet accounting = accountingRow("player", 4, 0);
		when(admittedSelect.executeQuery()).thenReturn(admitted);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(increment.executeUpdate()).thenReturn(1);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(admittedSelect, markerInsert, markerSelect,
				accountingSelect, increment, accountingUpdate);

		SharedMysqlPurchaseJournal.PeriodTotalResult result = new SharedMysqlPurchaseJournal(fixture.table, false)
				.incrementPeriodTotalsResolved(UUID.randomUUID(), "player", "MonthTotal", "LastMonthTotal",
						List.of("MonthTotal", "MonthTotal-OCTOBER-2026"), Integer.valueOf(99), true);

		assertEquals(List.of("MonthTotal", "MonthTotal-SEPTEMBER-2026"), result.columns());
		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(6)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(4).contains("`MonthTotal-SEPTEMBER-2026`"));
		assertFalse(sql.getAllValues().get(4).contains("`MonthTotal-OCTOBER-2026`"));
	}

	@Test
	void ambiguousPeriodIncrementCommitIsConfirmedByTheVoteMarker() throws Exception {
		Fixture fixture = fixture();
		Connection confirmation = mock(Connection.class);
		PreparedStatement requestInsert = mock(PreparedStatement.class);
		PreparedStatement requestSelect = mock(PreparedStatement.class);
		PreparedStatement requestUpdate = mock(PreparedStatement.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement increment = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		PreparedStatement confirm = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet requested = accountingRow("player", 0, 0);
		ResultSet accounting = accountingRow("player", 1, 0);
		ResultSet confirmed = accountingRow("player", 1, 1);
		when(epoch.next()).thenReturn(true);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(requestSelect.executeQuery()).thenReturn(requested);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(increment.executeUpdate()).thenReturn(1);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(requestUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(requestInsert, requestSelect, requestUpdate,
				markerInsert, markerSelect,
				accountingSelect, increment, accountingUpdate);
		org.mockito.Mockito.doNothing().doThrow(new java.sql.SQLException("commit acknowledgement lost"))
				.when(fixture.work).commit();
		when(confirm.executeQuery()).thenReturn(confirmed);
		when(confirmation.prepareStatement(anyString())).thenReturn(confirm);
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(fixture.work, fixture.work, confirmation);

		new SharedMysqlPurchaseJournal(fixture.table, false).incrementPeriodTotals(java.util.UUID.randomUUID(),
				"player", "DailyTotal", "LastDailyTotal", java.util.List.of("DailyTotal"), null);

		verify(increment).executeUpdate();
		verify(confirm).executeQuery();
	}

	@Test
	void dailyStreakValueAndTimestampCopyShareOneTransaction() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement pending = mock(PreparedStatement.class);
		PreparedStatement copy = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet noPending = ids();
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(2L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(pending.executeQuery()).thenReturn(noPending);
		when(advance.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, pending, copy, advance);

		new SharedMysqlPurchaseJournal(fixture.table, false).copyDailyStreakBoundary("DayVoteStreak",
				"LastDayVoteStreak", "DayVoteStreakLastUpdate", "LastDayVoteStreakLastUpdate",
				"time-streak-copy:DAY:2026-09-21");

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(5)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(3).contains("`LastDayVoteStreak` = COALESCE(`DayVoteStreak`, 0), "
				+ "`LastDayVoteStreakLastUpdate` = COALESCE(`DayVoteStreakLastUpdate`, '')"));
		verify(advance).setString(3, "streak-copy:DayVoteStreak");
		verify(fixture.work).commit();
	}

	@Test
	void dailyStreakUpdateLocksTheSameBoundaryRow() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement requestMarkerInsert = mock(PreparedStatement.class);
		PreparedStatement requestMarkerSelect = mock(PreparedStatement.class);
		PreparedStatement requestInsert = mock(PreparedStatement.class);
		PreparedStatement requestSelect = mock(PreparedStatement.class);
		PreparedStatement requestUpdate = mock(PreparedStatement.class);
		PreparedStatement copyMarkerInsert = mock(PreparedStatement.class);
		PreparedStatement copyMarkerSelect = mock(PreparedStatement.class);
		PreparedStatement resetMarkerInsert = mock(PreparedStatement.class);
		PreparedStatement resetMarkerSelect = mock(PreparedStatement.class);
		PreparedStatement accountingSelect = mock(PreparedStatement.class);
		PreparedStatement earlierStreakSelect = mock(PreparedStatement.class);
		PreparedStatement persistedUpdateSelect = mock(PreparedStatement.class);
		PreparedStatement update = mock(PreparedStatement.class);
		PreparedStatement accountingUpdate = mock(PreparedStatement.class);
		PreparedStatement streakValueSelect = mock(PreparedStatement.class);
		ResultSet requestEpoch = mock(ResultSet.class);
		ResultSet copyEpoch = mock(ResultSet.class);
		ResultSet resetEpoch = mock(ResultSet.class);
		ResultSet requested = accountingRow("00000000-0000-0000-0000-000000000001", 0, 0);
		ResultSet accounting = accountingRow("00000000-0000-0000-0000-000000000001", 16, 0);
		ResultSet noEarlierStreak = ids();
		ResultSet persistedUpdate = mock(ResultSet.class);
		ResultSet streakValue = mock(ResultSet.class);
		when(requestEpoch.next()).thenReturn(true);
		when(copyEpoch.next()).thenReturn(true);
		when(resetEpoch.next()).thenReturn(true);
		when(requestMarkerSelect.executeQuery()).thenReturn(requestEpoch);
		when(copyMarkerSelect.executeQuery()).thenReturn(copyEpoch);
		when(resetMarkerSelect.executeQuery()).thenReturn(resetEpoch);
		when(requestSelect.executeQuery()).thenReturn(requested);
		when(accountingSelect.executeQuery()).thenReturn(accounting);
		when(earlierStreakSelect.executeQuery()).thenReturn(noEarlierStreak);
		when(persistedUpdate.next()).thenReturn(true);
		when(persistedUpdate.getString(1)).thenReturn("");
		when(persistedUpdateSelect.executeQuery()).thenReturn(persistedUpdate);
		when(streakValue.next()).thenReturn(true);
		when(streakValue.getInt(1)).thenReturn(8);
		when(streakValueSelect.executeQuery()).thenReturn(streakValue);
		when(update.executeUpdate()).thenReturn(1);
		when(accountingUpdate.executeUpdate()).thenReturn(1);
		when(requestUpdate.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(requestMarkerInsert, requestMarkerSelect,
				requestInsert, requestSelect, requestUpdate, copyMarkerInsert, copyMarkerSelect,
				resetMarkerInsert, resetMarkerSelect, accountingSelect, earlierStreakSelect, persistedUpdateSelect, update,
				streakValueSelect, accountingUpdate);

		new SharedMysqlPurchaseJournal(fixture.table, false).updateDailyStreak(java.util.UUID.randomUUID(),
				"00000000-0000-0000-0000-000000000001", 7, 1234L, false);

		verify(requestMarkerSelect).setString(1, "streak-copy:DayVoteStreak");
		verify(copyMarkerSelect).setString(1, "streak-copy:DayVoteStreak");
		verify(earlierStreakSelect).setInt(4, 16);
		verify(earlierStreakSelect).setInt(5, 16);
		verify(update).setString(1, "1234");
		verify(update).setString(2, "00000000-0000-0000-0000-000000000001");
		verify(accountingUpdate).setInt(2, 8);
		verify(fixture.work, org.mockito.Mockito.times(2)).commit();
	}

	@Test
	void dailyStreakBoundaryResetUsesTheSharedDatabaseFence() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement reset = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(reset.executeUpdate()).thenReturn(1);
		when(fixture.work.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, reset);

		new SharedMysqlPurchaseJournal(fixture.table, false).resetDailyStreakAtBoundary("player", 1234L);

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(3)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(2).contains("CASE WHEN COALESCE(`DayVoteStreakLastUpdate`, '') = ?"));
		verify(markerSelect).setString(1, "streak-copy:DayVoteStreak");
		verify(reset).setString(1, "1234");
		verify(reset).setString(2, "player");
	}

	@Test
	void failedApplyLeavesTheCommittedAccountingRequestForRecovery() throws Exception {
		Fixture fixture = fixture();
		Connection request = mock(Connection.class);
		Connection apply = mock(Connection.class);
		PreparedStatement insert = mock(PreparedStatement.class);
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement update = mock(PreparedStatement.class);
		ResultSet row = accountingRow("player", 0, 0);
		when(select.executeQuery()).thenReturn(row);
		when(update.executeUpdate()).thenReturn(1);
		when(request.prepareStatement(anyString())).thenReturn(insert, select, update);
		when(apply.prepareStatement(anyString())).thenThrow(new java.sql.SQLException("database unavailable"));
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(request, apply);

		assertFalse(new SharedMysqlPurchaseJournal(fixture.table, false).incrementPeriodTotals(
				java.util.UUID.randomUUID(), "player", "DailyTotal", "LastDailyTotal",
				java.util.List.of("DailyTotal"), null));

		verify(request).commit();
		verify(request, org.mockito.Mockito.never()).rollback();
	}

	@Test
	void completedAccountingRowsRemainForTheUnboundedReplayWindow() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement pending = mock(PreparedStatement.class);
		ResultSet none = ids();
		when(pending.executeQuery()).thenReturn(none);
		when(fixture.work.prepareStatement(anyString())).thenReturn(pending);

		new SharedMysqlPurchaseJournal(fixture.table, false)
				.recoverAccounting(java.util.concurrent.TimeUnit.DAYS.toMillis(90));

		verify(fixture.work).prepareStatement(org.mockito.ArgumentMatchers.contains("requested"));
	}

	@Test
	void claimedRewardIsSkippedWithoutStarvingLaterRecoverableReward() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement pending = mock(PreparedStatement.class);
		ResultSet rows = mock(ResultSet.class);
		UUID ambiguous = UUID.randomUUID();
		UUID recoverable = UUID.randomUUID();
		when(rows.next()).thenReturn(true, true, false);
		when(rows.getString(1)).thenReturn(ambiguous.toString(), recoverable.toString());
		when(rows.getString(2)).thenReturn("player-a", "player-b");
		when(rows.getInt(3)).thenReturn(48, 48);
		when(rows.getInt(4)).thenReturn(144, 16);
		when(pending.executeQuery()).thenReturn(rows);
		when(fixture.work.prepareStatement(anyString())).thenReturn(pending);

		SharedMysqlPurchaseJournal.AccountingRecoveryBatch batch =
				new SharedMysqlPurchaseJournal(fixture.table, false).recoverAccounting(1L);

		assertEquals(java.util.List.of(recoverable), batch.pendingRewards());
		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work).prepareStatement(sql.capture());
		assertTrue(sql.getValue().contains("& 128"),
				"claimed reward rows must be excluded from the bounded recovery page");
	}

	@Test
	void ambiguousResetCommitIsConfirmedAfterTheConnectionIsReleased() throws Exception {
		Fixture fixture = fixture();
		Connection reset = mock(Connection.class);
		Connection confirmation = mock(Connection.class);
		PreparedStatement markerInsert = mock(PreparedStatement.class);
		PreparedStatement markerSelect = mock(PreparedStatement.class);
		PreparedStatement wipe = mock(PreparedStatement.class);
		PreparedStatement advance = mock(PreparedStatement.class);
		PreparedStatement confirm = mock(PreparedStatement.class);
		ResultSet epoch = mock(ResultSet.class);
		ResultSet confirmedEpoch = mock(ResultSet.class);
		when(epoch.next()).thenReturn(true);
		when(epoch.getLong(1)).thenReturn(2L);
		when(markerSelect.executeQuery()).thenReturn(epoch);
		when(advance.executeUpdate()).thenReturn(1);
		when(reset.prepareStatement(anyString())).thenReturn(markerInsert, markerSelect, wipe, advance);
		when(confirmedEpoch.next()).thenReturn(true);
		when(confirmedEpoch.getLong(1)).thenReturn(3L);
		when(confirmedEpoch.getString(2)).thenReturn("D:2026-09-08");
		when(confirm.executeQuery()).thenReturn(confirmedEpoch);
		when(confirmation.prepareStatement(anyString())).thenReturn(confirm);
		AtomicBoolean resetClosed = new AtomicBoolean();
		doThrow(new java.sql.SQLException("commit acknowledgement lost")).when(reset).commit();
		org.mockito.Mockito.doAnswer(ignored -> {
			resetClosed.set(true);
			return null;
		}).when(reset).close();
		when(fixture.sql.getConnectionManager().getConnection()).thenReturn(reset).thenAnswer(ignored -> {
			assertTrue(resetClosed.get());
			return confirmation;
		});

		new SharedMysqlPurchaseJournal(fixture.table, false).resetLimit("VoteShopLimitdaily", "D:2026-09-08");

		verify(reset, atLeastOnce()).close();
		verify(confirm).executeQuery();
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
	void refundAcceptsQuotedConfiguredLimitIdentifier() throws Exception {
		Fixture fixture = fixture();
		PreparedStatement select = mock(PreparedStatement.class);
		PreparedStatement refund = mock(PreparedStatement.class);
		PreparedStatement terminal = mock(PreparedStatement.class);
		ResultSet pending = pendingRow();
		when(pending.getString(4)).thenReturn("VoteShopLimitDaily Reward `special`");
		when(pending.getString(6)).thenReturn(SharedMysqlPurchaseJournal.NO_LIMIT_RESET_GENERATION);
		when(fixture.work.prepareStatement(anyString())).thenReturn(select, refund, terminal);
		when(select.executeQuery()).thenReturn(pending);
		when(refund.executeUpdate()).thenReturn(1);
		when(terminal.executeUpdate()).thenReturn(1);

		SharedMysqlPurchaseJournal journal = new SharedMysqlPurchaseJournal(fixture.table, false);
		boolean refunded = journal.refundPending("quoted-limit", 100L);
		verify(refund).executeUpdate();
		verify(terminal).executeUpdate();
		assertTrue(refunded);

		org.mockito.ArgumentCaptor<String> sql = org.mockito.ArgumentCaptor.forClass(String.class);
		verify(fixture.work, org.mockito.Mockito.times(3)).prepareStatement(sql.capture());
		assertTrue(sql.getAllValues().get(1).contains("VoteShopLimitDaily Reward `special`"));
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

	private static ResultSet accountingRow(String uuid, int requested, int completed) throws Exception {
		return accountingRow(uuid, requested, completed, null, null, null, false);
	}

	private static ResultSet accountingRow(String uuid, int requested, int completed, Integer points, Integer cap,
			boolean replayUnsafe) throws Exception {
		return accountingRow(uuid, requested, completed, points, cap, "persisted_Points", replayUnsafe);
	}

	private static ResultSet accountingRow(String uuid, int requested, int completed, Integer points, Integer cap,
			String pointColumn, boolean replayUnsafe) throws Exception {
		ResultSet row = mock(ResultSet.class);
		when(row.next()).thenReturn(true);
		when(row.getString(1)).thenReturn(uuid);
		when(row.getInt(2)).thenReturn(requested);
		when(row.getInt(3)).thenReturn(completed);
		when(row.getObject(10)).thenReturn(points);
		when(row.getObject(11)).thenReturn(cap);
		when(row.getString(12)).thenReturn(pointColumn);
		when(row.getInt(13)).thenReturn(replayUnsafe ? 1 : 0);
		return row;
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
