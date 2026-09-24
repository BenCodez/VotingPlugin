package com.bencodez.votingplugin.commands;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.UUID;

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
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

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

	@Test
	void durableClaimRecoveryRunsOnlyWhenTheGlobalSchedulerRejectsBeforeTaskStart() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		doThrow(new IllegalStateException("stopping")).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		AtomicBoolean taskRan = new AtomicBoolean();
		AtomicBoolean rejected = new AtomicBoolean();

		BukkitCompletionScheduler.run(plugin, null, () -> taskRan.set(true), () -> rejected.set(true));

		org.junit.jupiter.api.Assertions.assertFalse(taskRan.get());
		org.junit.jupiter.api.Assertions.assertTrue(rejected.get());
	}

	@Test
	void bulkStorageMutationStartsOffTheCommandThread() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		CommandSender sender = mock(CommandSender.class);
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(storage);
		AtomicReference<Runnable> worker = new AtomicReference<>();
		org.mockito.Mockito.doAnswer(call -> { worker.set(call.getArgument(0)); return null; })
				.when(storage).execute(any(Runnable.class));
		AtomicBoolean storageRan = new AtomicBoolean();

		new CommandLoader(plugin).runBulkStorageMutation(sender, () -> storageRan.set(true), () -> { });

		org.junit.jupiter.api.Assertions.assertFalse(storageRan.get());
		org.junit.jupiter.api.Assertions.assertNotNull(worker.get());
		verify(scheduler, never()).runTaskAsynchronously(eq(plugin), any(Runnable.class));
	}

	@Test
	void bulkUserPlayerLookupRunsOnPlatformBeforeStorageConsumer() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		CommandSender sender = mock(CommandSender.class);
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(storage);
		java.util.List<Runnable> storageTasks = new java.util.ArrayList<>();
		org.mockito.Mockito.doAnswer(call -> { storageTasks.add(call.getArgument(0)); return null; })
				.when(storage).execute(any(Runnable.class));
		UUID uuid = UUID.randomUUID();
		VotingPluginUser user = mock(VotingPluginUser.class);
		Player player = mock(Player.class);
		when(user.getPlayer()).thenReturn(player);
		when(plugin.getUserManager().getAllUUIDs())
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(uuid.toString())));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid)).thenReturn(user);
		AtomicReference<Runnable> platform = new AtomicReference<>();
		org.mockito.Mockito.doAnswer(call -> { platform.set(call.getArgument(1)); return null; })
				.when(scheduler).runTask(eq(plugin), any(Runnable.class));
		AtomicReference<CommandLoader.BulkVotingUsers> delivered = new AtomicReference<>();

		new CommandLoader(plugin).loadAllVotingUsersAsync(sender, delivered::set);
		storageTasks.get(0).run();
		verify(user, never()).getPlayer();
		platform.get().run();
		verify(user).getPlayer();
		storageTasks.get(1).run();

		org.junit.jupiter.api.Assertions.assertNotNull(delivered.get());
		org.junit.jupiter.api.Assertions.assertSame(player, delivered.get().player(user));
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
