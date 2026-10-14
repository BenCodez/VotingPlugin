package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState;
import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Candidate;
import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Snapshot;

class AutoCreateVoteSitesReminderStateTest {

	@Test
	void firstReminderUsesInclusiveThresholdAndSevenDayDefaults() {
		long threshold = AutoCreateVoteSitesReminderState.DEFAULT_THRESHOLD_MILLIS;
		assertEquals(TimeUnit.DAYS.toMillis(7), threshold);
		assertEquals(threshold, AutoCreateVoteSitesReminderState.DEFAULT_REPEAT_MILLIS);
		AutoCreateVoteSitesReminderState state = fresh(0);
		Candidate before = state.propose(nanos(threshold - 1), true, true, threshold, threshold);
		assertFalse(before.reminderDue());
		state.commit(before);
		Candidate boundary = state.propose(nanos(threshold), true, true, threshold, threshold);
		assertTrue(boundary.reminderDue());
		assertEquals(new Snapshot(threshold, threshold), boundary.snapshot());
		assertEquals(new Snapshot(threshold - 1, -1), state.snapshot());
		state.commit(boundary);
		assertFalse(state.propose(nanos(threshold + 1), true, true, threshold, threshold).reminderDue());
	}

	@Test
	void disabledFlagsSuppressOnlyReminderAndKeepAccruingRuntime() {
		boolean[][] disabled = { { false, false }, { false, true }, { true, false } };
		for (boolean[] flags : disabled) {
			AutoCreateVoteSitesReminderState state = fresh(0);
			Candidate candidate = state.propose(nanos(1500), flags[0], flags[1], 1000, 500);
			assertFalse(candidate.reminderDue());
			assertEquals(new Snapshot(1500, -1), candidate.snapshot());
			state.commit(candidate);
			Candidate enabled = state.propose(nanos(1500), true, true, 1000, 500);
			assertTrue(enabled.reminderDue());
			assertEquals(new Snapshot(1500, 1500), enabled.snapshot());
		}
	}

	@Test
	void restartLoadsCumulativeRuntimeAndExcludesOfflineTime() {
		AutoCreateVoteSitesReminderState previous = fresh(0);
		previous.commit(previous.propose(nanos(900), false, false, 1000, 500));
		long restartClock = nanos(TimeUnit.DAYS.toMillis(30));
		AutoCreateVoteSitesReminderState restarted = new AutoCreateVoteSitesReminderState(previous.snapshot(),
				restartClock);
		assertFalse(restarted.propose(restartClock, true, true, 1000, 500).reminderDue());
		Candidate before = restarted.propose(restartClock + nanos(99), true, true, 1000, 500);
		assertEquals(new Snapshot(999, -1), before.snapshot());
		assertFalse(before.reminderDue());
		restarted.commit(before);
		Candidate boundary = restarted.propose(restartClock + nanos(100), true, true, 1000, 500);
		assertEquals(new Snapshot(1000, 1000), boundary.snapshot());
		assertTrue(boundary.reminderDue());
	}

	@Test
	void restartPreservesReminderCooldownWithFreshClockAnchor() {
		AutoCreateVoteSitesReminderState state = new AutoCreateVoteSitesReminderState(new Snapshot(1000, 1000),
				-nanos(5000));
		assertFalse(state.propose(-nanos(4501), true, true, 1000, 500).reminderDue());
		assertTrue(state.propose(-nanos(4500), true, true, 1000, 500).reminderDue());
	}

	@Test
	void repeatBoundaryIsInclusiveAndAdvancesOnlyAfterCommit() {
		AutoCreateVoteSitesReminderState state = new AutoCreateVoteSitesReminderState(new Snapshot(1000, 1000), 0);
		Candidate before = state.propose(nanos(499), true, true, 1000, 500);
		assertFalse(before.reminderDue());
		state.commit(before);
		Candidate boundary = state.propose(nanos(500), true, true, 1000, 500);
		assertTrue(boundary.reminderDue());
		assertEquals(new Snapshot(1499, 1000), state.snapshot());
		state.commit(boundary);
		assertEquals(new Snapshot(1500, 1500), state.snapshot());
		assertFalse(state.propose(nanos(999), true, true, 1000, 500).reminderDue());
		assertTrue(state.propose(nanos(1000), true, true, 1000, 500).reminderDue());
	}

	@Test
	void failedPersistenceLeavesUptimeAndReminderRetryableWithoutDoubleCounting() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		Candidate failedWrite = state.propose(nanos(1000), true, true, 1000, 500);
		assertTrue(failedWrite.reminderDue());
		// The storage owner must omit commit when its write fails.
		assertEquals(new Snapshot(0, -1), state.snapshot());
		Candidate sameClockRetry = state.propose(nanos(1000), true, true, 1000, 500);
		assertEquals(failedWrite.snapshot(), sameClockRetry.snapshot());
		Candidate laterRetry = state.propose(nanos(1200), true, true, 1000, 500);
		assertTrue(laterRetry.reminderDue());
		assertEquals(new Snapshot(1200, 1200), laterRetry.snapshot());
		state.commit(laterRetry);
		assertThrows(IllegalArgumentException.class, () -> state.commit(failedWrite));
		assertThrows(IllegalArgumentException.class, () -> state.commit(sameClockRetry));
		Candidate next = state.propose(nanos(1300), true, true, 1000, 500);
		assertEquals(new Snapshot(1300, 1200), next.snapshot());
		assertFalse(next.reminderDue());
	}

	@Test
	void failedNonReminderCheckpointDoesNotMintRuntimeOnRetry() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		state.propose(nanos(300), false, false, 1000, 500);
		Candidate retry = state.propose(nanos(400), false, false, 1000, 500);
		assertEquals(new Snapshot(400, -1), retry.snapshot());
		state.commit(retry);
		assertEquals(new Snapshot(500, -1), state.propose(nanos(500), false, false, 1000, 500).snapshot());
	}

	@Test
	void staleAndDuplicateCommitsAreRejectedWithoutPublishingProgress() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		Candidate first = state.propose(nanos(100), false, false, 1000, 500);
		Candidate sibling = state.propose(nanos(200), false, false, 1000, 500);
		state.commit(first);
		assertThrows(IllegalArgumentException.class, () -> state.commit(first));
		assertThrows(IllegalArgumentException.class, () -> state.commit(sibling));
		assertEquals(new Snapshot(100, -1), state.snapshot());
		Candidate retry = state.propose(nanos(200), false, false, 1000, 500);
		state.commit(retry);
		assertEquals(new Snapshot(200, -1), state.snapshot());
	}

	@Test
	void candidateFromAnotherStateIsRejectedEvenWithIdenticalSnapshots() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		AutoCreateVoteSitesReminderState other = fresh(0);
		Candidate foreign = other.propose(nanos(1000), true, true, 1000, 500);
		assertThrows(IllegalArgumentException.class, () -> state.commit(foreign));
		assertEquals(new Snapshot(0, -1), state.snapshot());
		other.commit(foreign);
		assertEquals(new Snapshot(1000, 1000), other.snapshot());
	}

	@Test
	void changingThresholdAndRepeatKeepsExistingRuntimeAndReminderMarker() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		state.commit(state.propose(nanos(1500), true, true, 2000, 1000));
		Candidate loweredThreshold = state.propose(nanos(1500), true, true, 1000, 1000);
		assertTrue(loweredThreshold.reminderDue());
		state.commit(loweredThreshold);
		assertFalse(state.propose(nanos(2000), true, true, 3000, 100).reminderDue());
		assertFalse(state.propose(nanos(2000), true, true, 1000, 1000).reminderDue());
		Candidate loweredRepeat = state.propose(nanos(2000), true, true, 1000, 500);
		assertTrue(loweredRepeat.reminderDue());
		assertEquals(new Snapshot(2000, 2000), loweredRepeat.snapshot());
	}

	@Test
	void disablingAfterReminderDoesNotClearItsMarker() {
		AutoCreateVoteSitesReminderState state = new AutoCreateVoteSitesReminderState(new Snapshot(1000, 1000), 0);
		Candidate disabled = state.propose(nanos(600), false, false, 1000, 500);
		state.commit(disabled);
		assertEquals(new Snapshot(1600, 1000), state.snapshot());
		assertFalse(disabled.reminderDue());
		assertTrue(state.propose(nanos(600), true, true, 1000, 500).reminderDue());
	}

	@Test
	void negativeNanoReadingsAccruePositiveElapsedRuntime() {
		AutoCreateVoteSitesReminderState state = fresh(-nanos(1000));
		Candidate candidate = state.propose(-nanos(900), false, false, 1000, 500);
		assertEquals(new Snapshot(100, -1), candidate.snapshot());
		state.commit(candidate);
		assertEquals(new Snapshot(200, -1), state.propose(-nanos(800), false, false, 1000, 500).snapshot());
	}

	@Test
	void signedNanoClockWrapAccruesElapsedTime() {
		AutoCreateVoteSitesReminderState state = fresh(Long.MAX_VALUE - 500_000);
		Candidate wrapped = state.propose(Long.MIN_VALUE + 499_999, true, true, 1, 1);
		assertTrue(wrapped.reminderDue());
		assertEquals(new Snapshot(1, 1), wrapped.snapshot());
		state.commit(wrapped);
		assertEquals(new Snapshot(2, 2), state.propose(Long.MIN_VALUE + 1_499_999, true, true, 1, 1).snapshot());
	}

	@Test
	void backwardClockDoesNotMoveAnchorOrMintElapsedRuntime() {
		AutoCreateVoteSitesReminderState state = fresh(nanos(1000));
		Candidate backward = state.propose(nanos(900), true, true, 1000, 500);
		assertEquals(new Snapshot(0, -1), backward.snapshot());
		state.commit(backward);
		assertEquals(new Snapshot(100, -1), state.propose(nanos(1100), false, false, 1000, 500).snapshot());
	}

	@Test
	void subMillisecondCheckpointsRetainRemainderAndNeverDoubleCount() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		for (long clock : new long[] { 400_000, 800_000, 1_200_000, 1_600_000, 2_000_000 }) {
			state.commit(state.propose(clock, false, false, 1000, 500));
		}
		assertEquals(new Snapshot(2, -1), state.snapshot());
		state.commit(state.propose(2_000_000, false, false, 1000, 500));
		assertEquals(new Snapshot(2, -1), state.snapshot());
	}

	@Test
	void runtimeSaturatesWithoutOverflowAndReminderMarkerRemainsValid() {
		AutoCreateVoteSitesReminderState state = new AutoCreateVoteSitesReminderState(new Snapshot(Long.MAX_VALUE - 2, -1), 0);
		Candidate candidate = state.propose(nanos(10), true, true, 1000, 500);
		assertTrue(candidate.reminderDue());
		assertEquals(new Snapshot(Long.MAX_VALUE, Long.MAX_VALUE), candidate.snapshot());
		state.commit(candidate);
		Candidate later = state.propose(nanos(20), true, true, 1000, 500);
		assertEquals(new Snapshot(Long.MAX_VALUE, Long.MAX_VALUE), later.snapshot());
		assertFalse(later.reminderDue());
	}

	@Test
	void saturationDoesNotOverflowRepeatElapsedComparison() {
		AutoCreateVoteSitesReminderState state = new AutoCreateVoteSitesReminderState(new Snapshot(Long.MAX_VALUE - 1, 0), 0);
		Candidate candidate = state.propose(nanos(100), true, true,
				AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS, AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS);
		assertTrue(candidate.reminderDue());
		assertEquals(Long.MAX_VALUE, candidate.snapshot().totalRuntimeMillis());
	}

	@Test
	void snapshotsRejectNegativeRuntimeInvalidSentinelAndFutureReminderMarker() {
		assertThrows(IllegalArgumentException.class, () -> new Snapshot(-1, -1));
		assertThrows(IllegalArgumentException.class, () -> new Snapshot(0, -2));
		assertThrows(IllegalArgumentException.class, () -> new Snapshot(0, 1));
		assertThrows(IllegalArgumentException.class, () -> new Snapshot(Long.MAX_VALUE - 1, Long.MAX_VALUE));
		assertEquals(new Snapshot(0, -1), fresh(0).snapshot());
		assertEquals(0, new Snapshot(0, 0).lastReminderRuntimeMillis());
		assertEquals(Long.MAX_VALUE, new Snapshot(Long.MAX_VALUE, Long.MAX_VALUE).totalRuntimeMillis());
	}

	@Test
	void intervalsRejectNonPositiveAndOutOfRangeValuesEvenWhileDisabled() {
		AutoCreateVoteSitesReminderState state = fresh(0);
		for (long invalid : new long[] { -1, 0, AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS + 1, Long.MAX_VALUE }) {
			assertThrows(IllegalArgumentException.class, () -> state.propose(0, false, false, invalid, 1));
			assertThrows(IllegalArgumentException.class, () -> state.propose(0, false, false, 1, invalid));
		}
		assertEquals(new Snapshot(0, -1), state.snapshot());
		assertFalse(state.propose(0, true, true, AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS,
				AutoCreateVoteSitesReminderState.MAX_INTERVAL_MILLIS).reminderDue());
	}

	@Test
	void nullSnapshotAndNullCandidateAreRejected() {
		assertThrows(NullPointerException.class, () -> new AutoCreateVoteSitesReminderState(null, 0));
		AutoCreateVoteSitesReminderState state = fresh(0);
		assertThrows(NullPointerException.class, () -> state.commit(null));
		assertEquals(new Snapshot(0, -1), state.snapshot());
	}

	private static AutoCreateVoteSitesReminderState fresh(long nowNanos) {
		return new AutoCreateVoteSitesReminderState(new Snapshot(0, AutoCreateVoteSitesReminderState.NEVER_REMINDED), nowNanos);
	}

	private static long nanos(long millis) {
		return Math.multiplyExact(millis, 1_000_000);
	}
}
