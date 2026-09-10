package com.bencodez.votingplugin.commands;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.CompletableFuture;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.folialib.FoliaLib;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.folialib.impl.ServerImplementation;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.user.PointTransferResult;
import com.bencodez.votingplugin.user.VotingPluginUser;

class CommandLoaderSchedulingTest {
	@Test
	void playerCommandCompletionUsesTheSendersEntityLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		configureEntityScheduler(scheduler);
		Player sender = mock(Player.class);
		Runnable completion = () -> { };

		new CommandLoader(plugin).runForCommandSender(sender, completion);

		verify(scheduler).runTask(eq(plugin), any(Runnable.class), eq(sender));
		verify(scheduler, never()).runTask(eq(plugin), any(Runnable.class));
	}

	@Test
	void consoleCommandCompletionUsesTheGlobalLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		configureEntityScheduler(scheduler);
		CommandSender sender = mock(CommandSender.class);
		Runnable completion = () -> { };

		new CommandLoader(plugin).runForCommandSender(sender, completion);

		verify(scheduler).runTask(eq(plugin), any(Runnable.class));
		verify(scheduler, never()).runTask(eq(plugin), any(Runnable.class), any(Player.class));
	}

	@Test
	void onlineVotingUserCompletionUsesTheRecipientsEntityLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		configureEntityScheduler(scheduler);
		VotingPluginUser user = mock(VotingPluginUser.class);
		Player recipient = mock(Player.class);
		when(user.getPlayer()).thenReturn(recipient);
		Runnable completion = () -> { };

		new CommandLoader(plugin).runForVotingUser(user, completion);

		verify(scheduler).runTask(eq(plugin), any(Runnable.class), eq(recipient));
		verify(scheduler, never()).runTask(eq(plugin), any(Runnable.class));
	}

	@Test
	void offlineVotingUserCompletionUsesTheGlobalLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		VotingPluginUser user = mock(VotingPluginUser.class);
		Runnable completion = () -> { };

		new CommandLoader(plugin).runForVotingUser(user, completion);

		verify(scheduler).runTask(eq(plugin), any(Runnable.class));
		verify(scheduler, never()).runTask(eq(plugin), any(Runnable.class), any(Player.class));
	}

	@Test
	void transferFailureMessagesDoNotDiagnoseAvailabilityAsInsufficientPoints() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		when(plugin.getConfigFile()).thenReturn(config);
		when(config.getFormatCommandsVoteGivePointsNotEnoughPoints()).thenReturn("insufficient");
		when(config.getFormatCommandsVoteGivePointsUnavailable()).thenReturn("retry");
		when(config.getFormatCommandsVoteGivePointsPendingConfirmation()).thenReturn("pending; do not retry");
		CommandLoader loader = new CommandLoader(plugin);

		org.junit.jupiter.api.Assertions.assertEquals("insufficient",
				loader.transferFailureMessage(PointTransferResult.INSUFFICIENT_POINTS));
		org.junit.jupiter.api.Assertions.assertEquals("retry", loader.transferFailureMessage(PointTransferResult.CANCELLED));
		org.junit.jupiter.api.Assertions.assertEquals("retry", loader.transferFailureMessage(PointTransferResult.UNAVAILABLE));
		org.junit.jupiter.api.Assertions.assertEquals("pending; do not retry",
				loader.transferFailureMessage(PointTransferResult.PENDING_CONFIRMATION));
	}

	private static void configureEntityScheduler(BukkitScheduler scheduler) {
		FoliaLib folia = mock(FoliaLib.class);
		ServerImplementation entityScheduler = mock(ServerImplementation.class);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		when(entityScheduler.runAtEntityWithFallback(any(), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SUCCESS));
	}
}
