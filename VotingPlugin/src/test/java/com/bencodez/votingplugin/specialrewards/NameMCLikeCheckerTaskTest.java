package com.bencodez.votingplugin.specialrewards;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class NameMCLikeCheckerTaskTest {
	private static final UUID PLAYER_UUID = UUID.fromString("d980dba3-10e2-49e0-8c98-6a7674f4e89c");
	private static final String REWARD_PATH = "NameMCLikeRewards";

	private VotingPluginMain plugin;
	private com.bencodez.advancedcore.api.user.UserManager coreUsers;
	private UserDataManager dataManager;
	private VotingPluginUser user;
	private RewardHandler rewardHandler;
	private SpecialRewardsConfig config;
	private YamlConfiguration rewards;
	private NameMCLikeCheckerTask task;
	private ArrayDeque<Runnable> storageWork;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class);
		coreUsers = mock(com.bencodez.advancedcore.api.user.UserManager.class);
		dataManager = mock(UserDataManager.class);
		UserManager votingUsers = mock(UserManager.class);
		AdvancedCoreUser resolved = mock(AdvancedCoreUser.class);
		user = mock(VotingPluginUser.class);
		rewardHandler = mock(RewardHandler.class);
		config = mock(SpecialRewardsConfig.class);
		rewards = new YamlConfiguration();
		storageWork = new ArrayDeque<>();

		when(plugin.getUserManager()).thenReturn(coreUsers);
		when(coreUsers.getDataManager()).thenReturn(dataManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
		when(votingUsers.getVotingPluginUser(resolved)).thenReturn(user);
		when(plugin.getSpecialRewardsConfig()).thenReturn(config);
		when(plugin.getRewardHandler()).thenReturn(rewardHandler);
		when(plugin.getLogger()).thenReturn(mock(Logger.class));
		when(plugin.isEnabled()).thenReturn(true);
		when(config.isNameMCLikeRewardEnabled()).thenReturn(true);
		when(config.getData()).thenReturn(rewards);
		when(config.getNameMCLikeRewardPath()).thenReturn(REWARD_PATH);
		when(config.getNameMCLikeRewardUrl()).thenReturn("example.minecraft.net");
		when(user.getPlugin()).thenReturn(plugin);
		when(user.getPlayerName()).thenReturn("Player");
		when(user.isOnline()).thenReturn(true);
		when(rewardHandler.giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class))).thenReturn(CompletableFuture.completedFuture(null));

		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(dataManager.getTimer()).thenReturn(timer);
		doAnswer(invocation -> {
			storageWork.add(invocation.getArgument(0));
			return null;
		}).when(timer).execute(any(Runnable.class));
		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			Consumer<AdvancedCoreUser> success = invocation.getArgument(1);
			success.accept(resolved);
			return null;
		}).when(coreUsers).getUserAsync(eq(PLAYER_UUID), any(), any());

		task = new NameMCLikeCheckerTask(plugin);
	}

	@Test
	void sharedStoragePersistsPendingBeforeDispatchAndClaimsOnlyAfterAsyncSuccess() {
		DeferredCheck deferred = captureSharedCheck();
		CompletableFuture<Void> delivery = new CompletableFuture<>();
		when(rewardHandler.giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class))).thenReturn(delivery);

		task.processUuid(PLAYER_UUID);
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(1)).getUserAsync(eq(PLAYER_UUID), any(), any());
		verify(user, never()).hasClaimedNameMCLikeReward();
		assertEquals(Boolean.TRUE, deferred.work.get());

		InOrder order = inOrder(user, rewardHandler);
		order.verify(user).cache();
		order.verify(user).hasClaimedNameMCLikeReward();
		order.verify(user).isNameMCLikeRewardPending();
		order.verify(user).setNameMCLikeRewardPending(true);
		order.verify(rewardHandler).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, never()).setClaimedNameMCLikeReward(true);
		assertTrue(storageWork.isEmpty());

		delivery.complete(null);
		assertEquals(1, storageWork.size());
		verify(user, never()).setClaimedNameMCLikeReward(true);
		storageWork.remove().run();
		InOrder completed = inOrder(user);
		completed.verify(user).setNameMCLikeRewardPending(true);
		completed.verify(user).setClaimedNameMCLikeReward(true);
		completed.verify(user).setNameMCLikeRewardPending(false);
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(2)).getUserAsync(eq(PLAYER_UUID), any(), any());
	}

	@Test
	void asyncFailureLeavesPendingWithoutFalseClaimOrDuplicateDelivery() {
		DeferredCheck deferred = captureSharedCheck();
		CompletableFuture<Void> delivery = new CompletableFuture<>();
		when(rewardHandler.giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class))).thenReturn(delivery);
		when(user.isNameMCLikeRewardPending()).thenReturn(false, true);
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());

		delivery.completeExceptionally(new IllegalStateException("partially delivered"));
		verify(user, never()).setClaimedNameMCLikeReward(true);
		verify(user, never()).setNameMCLikeRewardPending(false);
		assertTrue(storageWork.isEmpty());

		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(rewardHandler, times(1)).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, times(1)).setNameMCLikeRewardPending(true);
	}

	@Test
	void alreadyClaimedSharedUserDoesNotReceiveSecondReward() {
		DeferredCheck deferred = captureSharedCheck();
		when(user.hasClaimedNameMCLikeReward()).thenReturn(true);
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(user).cache();
		verify(rewardHandler, never()).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, never()).setNameMCLikeRewardPending(true);
	}

	@Test
	void preexistingPendingClaimRequiresReviewAndIsNotReplayed() {
		DeferredCheck deferred = captureSharedCheck();
		when(user.isNameMCLikeRewardPending()).thenReturn(true);
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(rewardHandler, never()).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, never()).setClaimedNameMCLikeReward(true);
	}

	@Test
	void failedCachePopulationCannotDispatchOrClaim() {
		DeferredCheck deferred = captureSharedCheck();
		doThrow(new IllegalStateException("cache unavailable")).when(user).cache();
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(user, never()).hasClaimedNameMCLikeReward();
		verify(user, never()).setNameMCLikeRewardPending(true);
		verify(rewardHandler, never()).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(2)).getUserAsync(eq(PLAYER_UUID), any(), any());
	}

	@Test
	void unavailableSharedStorageDoesNotFallBackToPlatformClaimRead() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(true);
		task.processUuid(PLAYER_UUID);
		verify(dataManager).deferSharedStorageResultFromPlatform(any(), any(), any());
		verify(user, never()).cache();
		verify(user, never()).hasClaimedNameMCLikeReward();
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(2)).getUserAsync(eq(PLAYER_UUID), any(), any());
	}

	@Test
	void nonSharedStorageAlsoDefersUserDataAndConfirmationToStorageWorker() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(false);
		task.processUuid(PLAYER_UUID);
		verify(dataManager, never()).deferSharedStorageResultFromPlatform(any(), any(), any());
		verify(user, never()).hasClaimedNameMCLikeReward();
		assertEquals(1, storageWork.size());
		storageWork.remove().run();
		verify(user).setNameMCLikeRewardPending(true);
		verify(user, never()).setClaimedNameMCLikeReward(true);
		assertEquals(1, storageWork.size());
		storageWork.remove().run();
		verify(user).setClaimedNameMCLikeReward(true);
		verify(user).setNameMCLikeRewardPending(false);
	}

	private DeferredCheck captureSharedCheck() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(true);
		DeferredCheck deferred = new DeferredCheck();
		doAnswer(invocation -> {
			deferred.work = invocation.getArgument(0);
			deferred.success = invocation.getArgument(1);
			deferred.failure = invocation.getArgument(2);
			return true;
		}).when(dataManager).deferSharedStorageResultFromPlatform(any(), any(), any());
		return deferred;
	}

	private static final class DeferredCheck {
		private Supplier<Boolean> work;
		private Consumer<Boolean> success;
		private Consumer<Throwable> failure;
	}
}
