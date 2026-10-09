package com.bencodez.votingplugin.specialrewards;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
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

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
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
		when(rewardHandler.giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class))).thenReturn(CompletableFuture.completedFuture(null));
		when(user.getPlayerName()).thenReturn("Player");
		when(user.isOnline()).thenReturn(true);

		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			Consumer<AdvancedCoreUser> success = invocation.getArgument(1);
			success.accept(resolved);
			return null;
		}).when(coreUsers).getUserAsync(eq(PLAYER_UUID), any(), any());

		task = new NameMCLikeCheckerTask(plugin);
	}

	@Test
	void sharedStorageDefersClaimReadRewardAndClaimWriteToWorker() {
		DeferredCheck deferred = captureSharedCheck();
		AtomicReference<Boolean> inStorage = new AtomicReference<>(false);
		doAnswer(invocation -> {
			assertTrue(inStorage.get(), "The claim must be checked on the storage worker");
			return false;
		}).when(user).hasClaimedNameMCLikeReward();
		doAnswer(invocation -> {
			assertTrue(inStorage.get(), "The claim must be stored on the storage worker");
			return null;
		}).when(user).setClaimedNameMCLikeReward(true);

		task.processUuid(PLAYER_UUID);
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(1)).getUserAsync(eq(PLAYER_UUID), any(), any());
		verify(user, never()).hasClaimedNameMCLikeReward();
		verify(user, never()).cache();

		inStorage.set(true);
		assertEquals(Boolean.TRUE, deferred.work.get());
		inStorage.set(false);

		InOrder order = inOrder(user, rewardHandler);
		order.verify(user).cache();
		order.verify(user).hasClaimedNameMCLikeReward();
		order.verify(rewardHandler).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		order.verify(user).setClaimedNameMCLikeReward(true);
		verify(user, times(1)).isOnline();
		// Cleanup belongs to the worker, not a platform callback that might be cancelled.
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(2)).getUserAsync(eq(PLAYER_UUID), any(), any());
	}

	@Test
	void claimedSharedUserDoesNotReceiveSecondReward() {
		DeferredCheck deferred = captureSharedCheck();
		when(user.hasClaimedNameMCLikeReward()).thenReturn(true);
		task.processUuid(PLAYER_UUID);

		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(user).cache();
		verify(user).hasClaimedNameMCLikeReward();
		verify(rewardHandler, never()).giveReward(eq(user), any(YamlConfiguration.class), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, never()).setClaimedNameMCLikeReward(true);
	}

	@Test
	void cacheFailureReleasesInFlightWithoutGrantingOrMarkingClaimed() {
		DeferredCheck deferred = captureSharedCheck();
		doThrow(new IllegalStateException("cache unavailable")).when(user).cache();
		task.processUuid(PLAYER_UUID);

		assertThrows(IllegalStateException.class, deferred.work::get);
		verify(user, never()).hasClaimedNameMCLikeReward();
		verify(user, never()).setClaimedNameMCLikeReward(true);
		verify(rewardHandler, never()).giveReward(eq(user), eq(rewards), eq(REWARD_PATH),
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
		verify(user, never()).setClaimedNameMCLikeReward(true);
		task.processUuid(PLAYER_UUID);
		verify(coreUsers, times(2)).getUserAsync(eq(PLAYER_UUID), any(), any());
	}


	@Test
	void asynchronousRewardFailureDoesNotReopenAmbiguousClaim() {
		DeferredCheck deferred = captureSharedCheck();
		CompletableFuture<Void> delivery = new CompletableFuture<>();
		when(rewardHandler.giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class))).thenReturn(delivery);
		when(user.hasClaimedNameMCLikeReward()).thenReturn(false, true);
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(user).setClaimedNameMCLikeReward(true);

		delivery.completeExceptionally(new IllegalStateException("partial reward"));
		task.processUuid(PLAYER_UUID);
		assertEquals(Boolean.TRUE, deferred.work.get());
		verify(rewardHandler, times(1)).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user, times(1)).setClaimedNameMCLikeReward(true);
	}

	@Test
	void nonSharedStoragePreservesExistingRewardBehavior() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(false);
		task.processUuid(PLAYER_UUID);
		verify(dataManager, never()).deferSharedStorageResultFromPlatform(any(), any(), any());
		verify(user, never()).cache();
		verify(user).hasClaimedNameMCLikeReward();
		verify(rewardHandler).giveRewardAsync(eq(user), eq(rewards), eq(REWARD_PATH),
				any(RewardOptions.class));
		verify(user).setClaimedNameMCLikeReward(true);
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
