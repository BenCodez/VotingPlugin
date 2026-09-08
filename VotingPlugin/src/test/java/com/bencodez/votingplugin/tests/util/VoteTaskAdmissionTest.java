package com.bencodez.votingplugin.tests.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

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
}
