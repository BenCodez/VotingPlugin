package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.votingplugin.proxy.IncomingVoteRuntimeResult;
import com.bencodez.votingplugin.proxy.bungee.VoteEventBungee;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.velocity.VoteEventVelocity;
import com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity;
import com.vexsoftware.votifier.model.Vote;

class ProxyVoteEventNullServiceTest {
	@Test
	void bungeeNullServiceSchedulesVoteWithCompatibilityName() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.processIncomingVote(any(String.class), any(String.class), any(UUID.class)))
				.thenReturn(IncomingVoteRuntimeResult.PROCESSED);
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> task = new AtomicReference<>();
		doAnswer(invocation -> {
			task.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runAsync(eq(plugin), any(Runnable.class));
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn(null);
		com.vexsoftware.votifier.bungee.events.VotifierEvent event =
				mock(com.vexsoftware.votifier.bungee.events.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventBungee(plugin).onVote(event);

		assertNotNull(task.get());
		assertEquals("Empty", field(task.get(), "service"));
	}

	@Test
	void bungeeVoteWaitsForFullReloadInsteadOfDropping() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.processIncomingVote(any(String.class), any(String.class), any(UUID.class)))
				.thenReturn(IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD,
						IncomingVoteRuntimeResult.PROCESSED);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("ProxyVoteEventNullServiceTest"));
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> task = new AtomicReference<>();
		doAnswer(invocation -> {
			task.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runAsync(eq(plugin), any(Runnable.class));
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn("Service");
		com.vexsoftware.votifier.bungee.events.VotifierEvent event =
				mock(com.vexsoftware.votifier.bungee.events.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventBungee(plugin).onVote(event);
		Runnable pending = task.get();
		assertNotNull(pending);
		pending.run();

		verify(scheduler).schedule(eq(plugin), eq(pending), eq(1L), eq(TimeUnit.SECONDS));
		assertEquals(0, field(pending, "attempts"), "reload waiting must not consume durable-storage attempts");
		UUID stableVoteId = (UUID) field(pending, "voteId");
		pending.run();
		ArgumentCaptor<UUID> ids = ArgumentCaptor.forClass(UUID.class);
		verify(plugin, times(2)).processIncomingVote(eq("Player"), eq("Service"), ids.capture());
		assertEquals(java.util.List.of(stableVoteId, stableVoteId), ids.getAllValues());
	}

	@Test
	void velocityVoteWaitsForFullReloadInsteadOfDropping() throws Exception {
		VotingPluginVelocity plugin = mock(VotingPluginVelocity.class);
		when(plugin.processIncomingVote(any(String.class), any(String.class), any(UUID.class)))
				.thenReturn(IncomingVoteRuntimeResult.RETRY_AFTER_RELOAD,
						IncomingVoteRuntimeResult.PROCESSED);
		when(plugin.getLogger()).thenReturn(mock(org.slf4j.Logger.class));
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(timer);
		AtomicReference<Runnable> task = new AtomicReference<>();
		doAnswer(invocation -> {
			task.set(invocation.getArgument(0));
			return null;
		}).when(timer).execute(any(Runnable.class));
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn("Service");
		com.vexsoftware.votifier.velocity.event.VotifierEvent event =
				mock(com.vexsoftware.votifier.velocity.event.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventVelocity(plugin).onVotifierEvent(event);
		Runnable pending = task.get();
		assertNotNull(pending);
		pending.run();

		verify(timer).schedule(eq(pending), eq(1L), eq(TimeUnit.SECONDS));
		assertEquals(0, field(pending, "attempts"), "reload waiting must not consume durable-storage attempts");
		UUID stableVoteId = (UUID) field(pending, "voteId");
		pending.run();
		ArgumentCaptor<UUID> ids = ArgumentCaptor.forClass(UUID.class);
		verify(plugin, times(2)).processIncomingVote(eq("Player"), eq("Service"), ids.capture());
		assertEquals(java.util.List.of(stableVoteId, stableVoteId), ids.getAllValues());
	}

	@Test
	void velocityNullServiceSchedulesVoteWithCompatibilityName() throws Exception {
		VotingPluginVelocity plugin = mock(VotingPluginVelocity.class);
		when(plugin.processIncomingVote(any(String.class), any(String.class), any(UUID.class)))
				.thenReturn(IncomingVoteRuntimeResult.PROCESSED);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(timer);
		AtomicReference<Runnable> task = new AtomicReference<>();
		doAnswer(invocation -> {
			task.set(invocation.getArgument(0));
			return null;
		}).when(timer).execute(any(Runnable.class));
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn(null);
		com.vexsoftware.votifier.velocity.event.VotifierEvent event =
				mock(com.vexsoftware.votifier.velocity.event.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventVelocity(plugin).onVotifierEvent(event);

		assertNotNull(task.get());
		assertEquals("Empty", field(task.get(), "service"));
	}

	@Test
	void terminalBungeeRuntimeFailureDoesNotInvokeDisposedRuntime() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		when(plugin.processIncomingVote(any(String.class), any(String.class), any(UUID.class)))
				.thenReturn(IncomingVoteRuntimeResult.RUNTIME_UNAVAILABLE);
		when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getLogger("ProxyVoteEventNullServiceTest"));
		net.md_5.bungee.api.ProxyServer proxy = mock(net.md_5.bungee.api.ProxyServer.class);
		net.md_5.bungee.api.scheduler.TaskScheduler scheduler =
				mock(net.md_5.bungee.api.scheduler.TaskScheduler.class);
		when(plugin.getProxy()).thenReturn(proxy);
		when(proxy.getScheduler()).thenReturn(scheduler);
		AtomicReference<Runnable> task = new AtomicReference<>();
		doAnswer(invocation -> {
			task.set(invocation.getArgument(1));
			return null;
		}).when(scheduler).runAsync(eq(plugin), any(Runnable.class));
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn("Service");
		com.vexsoftware.votifier.bungee.events.VotifierEvent event =
				mock(com.vexsoftware.votifier.bungee.events.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventBungee(plugin).onVote(event);
		task.get().run();

		verify(plugin, never()).getVotingPluginProxy();
		verify(scheduler, never()).schedule(eq(plugin), any(Runnable.class), eq(1L), eq(TimeUnit.SECONDS));
	}

	private static Object field(Object target, String name) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}
}
