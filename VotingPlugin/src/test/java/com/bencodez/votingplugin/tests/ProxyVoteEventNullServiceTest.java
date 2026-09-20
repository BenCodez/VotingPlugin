package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.bungee.VoteEventBungee;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.velocity.VoteEventVelocity;
import com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity;
import com.vexsoftware.votifier.model.Vote;

class ProxyVoteEventNullServiceTest {
	@Test
	void bungeeNullServiceSchedulesVoteWithCompatibilityName() throws Exception {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
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
	void velocityNullServiceSchedulesVoteWithCompatibilityName() throws Exception {
		VotingPluginVelocity plugin = mock(VotingPluginVelocity.class);
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

	private static Object field(Object target, String name) throws Exception {
		Field field = target.getClass().getDeclaredField(name);
		field.setAccessible(true);
		return field.get(target);
	}
}
