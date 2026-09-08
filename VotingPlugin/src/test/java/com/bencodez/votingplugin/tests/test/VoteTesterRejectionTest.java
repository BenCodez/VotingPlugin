package com.bencodez.votingplugin.tests.test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.test.VoteTester;

class VoteTesterRejectionTest {
	private VotingPluginMain plugin;
	private ScheduledExecutorService voteTimer;
	private Logger logger;
	private VoteTester tester;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		voteTimer = mock(ScheduledExecutorService.class);
		logger = mock(Logger.class);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		when(plugin.getLogger()).thenReturn(logger);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));
		tester = new VoteTester(plugin);
	}

	@Test
	void rewardAndVoteTestsLogRejectionWithoutThrowing() {
		assertDoesNotThrow(() -> tester.testRewards(1, "Steve", "FirstVote"));
		assertDoesNotThrow(() -> tester.testVotes(1, "Steve", "example.org"));

		verify(logger, org.mockito.Mockito.times(2)).warning(any(String.class));
	}

	@Test
	void spamTestAsyncSubmissionDoesNotThrowWhenRejected() {
		BukkitScheduler bukkitScheduler = mock(BukkitScheduler.class);
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(1);
			task.run();
			return null;
		}).when(bukkitScheduler).runTaskAsynchronously(any(), any(Runnable.class));
		when(plugin.getBukkitScheduler()).thenReturn(bukkitScheduler);

		assertDoesNotThrow(() -> tester.testSpam(1, "Steve", "example.org"));

		verify(logger).warning(any(String.class));
	}
}
