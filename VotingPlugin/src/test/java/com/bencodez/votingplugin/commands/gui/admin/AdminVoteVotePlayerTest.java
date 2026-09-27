package com.bencodez.votingplugin.commands.gui.admin;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.RejectedExecutionException;

import org.bukkit.Bukkit;
import org.bukkit.Server;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

class AdminVoteVotePlayerTest {
	@Test
	void dispatchesFromPlayerThreadBeforeVoteWorkerAdmission() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Server server = mock(Server.class);
		PluginManager pluginManager = mock(PluginManager.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player sender = mock(Player.class);
		Player target = mock(Player.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "example.org", false, false);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(pluginManager);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		ArgumentCaptor<Runnable> dispatched = ArgumentCaptor.forClass(Runnable.class);

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class)) {
			bukkit.when(() -> Bukkit.getPlayerExact("Steve")).thenReturn(target);
			new AdminVoteVotePlayer(plugin, sender, "Steve").dispatchVote(sender, event);
		}

		assertFalse(event.isAsynchronous());
		verify(scheduler).runTask(same(plugin), dispatched.capture(), same(target));
		verify(pluginManager, never()).callEvent(event);
		dispatched.getValue().run();
		verify(pluginManager).callEvent(event);
	}

	@Test
	void existingVoteEventConstructorRetainsItsAsynchronousContract() {
		assertTrue(new PlayerVoteEvent(null, "Steve", "example.org", false).isAsynchronous());
	}

	@Test
	void rejectedOwnerDispatchCompletesVoteAsFailed() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player target = mock(Player.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "example.org", false, false);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		doThrow(new RejectedExecutionException("disabled"))
				.when(scheduler).runTask(same(plugin), org.mockito.ArgumentMatchers.any(Runnable.class), same(target));
		doThrow(new RejectedExecutionException("disabled"))
				.when(scheduler).runTask(same(plugin), org.mockito.ArgumentMatchers.any(Runnable.class));

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class)) {
			bukkit.when(() -> Bukkit.getPlayerExact("Steve")).thenReturn(target);
			com.bencodez.votingplugin.util.BukkitVoteEventDispatcher.dispatch(plugin, event);
		}

		assertTrue(event.isProcessingFailed());
		assertTrue(event.getProcessingCompletion().toCompletableFuture().isDone());
	}

	@Test
	void eventDispatchFailureCompletesVoteAsFailed() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Server server = mock(Server.class);
		PluginManager pluginManager = mock(PluginManager.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "example.org", false, false);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(pluginManager);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		doThrow(new IllegalStateException("dispatch failed")).when(pluginManager).callEvent(event);
		ArgumentCaptor<Runnable> dispatched = ArgumentCaptor.forClass(Runnable.class);

		try (MockedStatic<Bukkit> bukkit = org.mockito.Mockito.mockStatic(Bukkit.class)) {
			bukkit.when(() -> Bukkit.getPlayerExact("Steve")).thenReturn(null);
			com.bencodez.votingplugin.util.BukkitVoteEventDispatcher.dispatch(plugin, event);
		}
		verify(scheduler).runTask(same(plugin), dispatched.capture());
		assertThrows(IllegalStateException.class, () -> dispatched.getValue().run());

		assertTrue(event.isProcessingFailed());
		assertTrue(event.getProcessingCompletion().toCompletableFuture().isDone());
	}
}
