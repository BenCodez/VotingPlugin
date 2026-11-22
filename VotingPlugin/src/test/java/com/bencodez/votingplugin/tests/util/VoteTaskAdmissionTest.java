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
	void manualVoteProducerMustObservePartyAdmissionBeforeDispatch() {
		var plugin = org.mockito.Mockito.mock(com.bencodez.votingplugin.VotingPluginMain.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		var timer = mock(ScheduledExecutorService.class);
		var scheduler = plugin.getBukkitScheduler();
		var manager = plugin.getServer().getPluginManager();
		org.mockito.Mockito.when(plugin.getVoteTimer()).thenReturn(timer);
		org.mockito.Mockito.when(plugin.getVoteParty().tryDispatchVoteEvent(any(Runnable.class))).thenReturn(false);
		org.mockito.Mockito.doAnswer(call -> {
			call.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTaskAsynchronously(org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class));
		org.mockito.Mockito.doAnswer(call -> {
			call.getArgument(0, Runnable.class).run();
			return null;
		}).when(timer).submit(any(Runnable.class));
		new com.bencodez.votingplugin.test.VoteTester(plugin).testSpam(3, "SyntheticVoter", "votes.example");
		org.mockito.Mockito.verify(manager, org.mockito.Mockito.never()).callEvent(
				any(com.bencodez.votingplugin.events.PlayerVoteEvent.class));
		org.mockito.Mockito.verify(plugin.getLogger(), org.mockito.Mockito.times(1)).warning(any(String.class));
	}

	@Test
	void manualPlatformRequestDefersEffectsAndSuccessUntilConfirmedDispatch() {
		var plugin = org.mockito.Mockito.mock(com.bencodez.votingplugin.VotingPluginMain.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		org.mockito.Mockito.when(plugin.getUserManager().getDataManager().isPlatformOwnedThread()).thenReturn(true);
		var timer = mock(ScheduledExecutorService.class);
		var scheduler = plugin.getBukkitScheduler();
		var admission = plugin.getVoteParty();
		var sender = mock(org.bukkit.command.CommandSender.class);
		var work = new java.util.concurrent.atomic.AtomicReference<Runnable>();
		var event = new com.bencodez.votingplugin.events.PlayerVoteEvent(null, "SyntheticVoter", "votes.example", false);
		org.mockito.Mockito.when(plugin.getVoteTimer()).thenReturn(timer);
		org.mockito.Mockito.doAnswer(call -> { work.set(call.getArgument(0)); return null; })
				.when(timer).submit(any(Runnable.class));
		org.mockito.Mockito.when(admission.tryDispatchVoteEvent(any(Runnable.class))).thenAnswer(call -> {
			call.getArgument(0, Runnable.class).run(); return true;
		});
		org.mockito.Mockito.doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
				.when(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class));
		VoteTaskAdmission.dispatchManualVote(plugin, event, sender, "Vote dispatched");
		org.mockito.Mockito.verify(plugin.getServer().getPluginManager(), org.mockito.Mockito.never()).callEvent(any());
		org.mockito.Mockito.verify(sender, org.mockito.Mockito.never()).sendMessage(any(String.class));
		org.junit.jupiter.api.Assertions.assertNotNull(work.get());
		work.get().run();
		org.mockito.Mockito.verify(plugin.getServer().getPluginManager()).callEvent(event);
		org.mockito.Mockito.verify(sender).sendMessage("Vote dispatched");
		org.mockito.Mockito.verify(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class));
	}

	@Test
	void refusedManualRequestReportsBusyInsteadOfSuccessWithoutDispatch() {
		var plugin = org.mockito.Mockito.mock(com.bencodez.votingplugin.VotingPluginMain.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		var sender = mock(org.bukkit.command.CommandSender.class);
		var scheduler = plugin.getBukkitScheduler();
		org.mockito.Mockito.doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
				.when(scheduler).runTask(org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class));
		var event = new com.bencodez.votingplugin.events.PlayerVoteEvent(null, "SyntheticVoter", "votes.example", false);
		VoteTaskAdmission.dispatchManualVote(plugin, event, sender, "Vote dispatched");
		org.mockito.Mockito.verify(plugin.getServer().getPluginManager(), org.mockito.Mockito.never()).callEvent(any());
		org.mockito.Mockito.verify(sender).sendMessage(org.mockito.ArgumentMatchers.contains("busy"));
		org.mockito.Mockito.verify(sender, org.mockito.Mockito.never()).sendMessage("Vote dispatched");
	}

	@Test
	void unavailableExecutorIsRejectionWithoutRunningTheTask() {
		var ran = new java.util.concurrent.atomic.AtomicBoolean();
		assertFalse(VoteTaskAdmission.trySubmit(null, () -> ran.set(true)));
		assertFalse(VoteTaskAdmission.trySchedule(null, () -> ran.set(true), 5, TimeUnit.SECONDS));
		assertFalse(ran.get());
	}
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
