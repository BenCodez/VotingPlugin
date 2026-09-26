package com.bencodez.votingplugin.tests.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.VoteTaskAdmission;

class VoteTaskAdmissionTest {
	@Test
	void rejectedSubmissionIsReportedWithoutPropagating() {
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(executor).submit(any(Runnable.class));

		assertFalse(VoteTaskAdmission.trySubmit(executor, () -> { }));
	}

	@Test
	void rejectedScheduleIsReportedWithoutPropagating() {
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(executor).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));

		assertFalse(VoteTaskAdmission.trySchedule(executor, () -> { }, 5, TimeUnit.SECONDS));
	}

	@Test
	void admittedTasksAreReported() {
		ScheduledExecutorService executor = mock(ScheduledExecutorService.class);

		assertTrue(VoteTaskAdmission.trySubmit(executor, () -> { }));
		assertTrue(VoteTaskAdmission.trySchedule(executor, () -> { }, 5, TimeUnit.SECONDS));
	}

	@Test
	void admittedTaskOwnsVoteLaneOnlyWhileItRuns() throws Exception {
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		AtomicBoolean ownedInside = new AtomicBoolean();
		try {
			assertFalse(VoteTaskAdmission.isVoteTask());
			assertTrue(VoteTaskAdmission.trySubmit(executor,
					() -> ownedInside.set(VoteTaskAdmission.isVoteTask())));
			executor.shutdown();
			assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
			assertTrue(ownedInside.get());
			assertFalse(VoteTaskAdmission.isVoteTask());
		} finally {
			executor.shutdownNow();
		}
	}

	@Test
	void externallyScheduledOwnedTaskUsesTheSameScopedMarker() {
		AtomicBoolean ownedInside = new AtomicBoolean();

		VoteTaskAdmission.ownedTask(() -> ownedInside.set(VoteTaskAdmission.isVoteTask())).run();

		assertTrue(ownedInside.get());
		assertFalse(VoteTaskAdmission.isVoteTask());
	}
}
