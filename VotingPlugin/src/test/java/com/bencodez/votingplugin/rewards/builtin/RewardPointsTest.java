package com.bencodez.votingplugin.rewards.builtin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class RewardPointsTest {
	@Test
	void publishesStorageAwarePointTotalWithoutBlockingTheRewardLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(advancedUser)).thenReturn(user);
		when(user.addPointsStorageAware(5)).thenReturn(73);

		String result = new RewardPoints(plugin).onRewardRequest(mock(Reward.class), advancedUser, 5,
				new HashMap<>());

		assertEquals("73", result);
		verify(user).addPointsStorageAware(5);
		verify(user, never()).addPoints(5);
	}

	@Test
	void asyncRewardWaitsForCommittedPointTotal() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		CompletableFuture<Integer> committed = new CompletableFuture<>();
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(advancedUser)).thenReturn(user);
		when(user.addPointsStorageAwareAsync(5)).thenReturn(committed);
		RewardPoints points = new RewardPoints(plugin);

		CompletableFuture<String> result = points.onRewardRequestAsync(mock(Reward.class), advancedUser, 5,
				new HashMap<>()).toCompletableFuture();

		assertEquals(false, result.isDone());
		committed.complete(73);
		assertEquals("73", result.join());
		verify(user).addPointsStorageAwareAsync(5);
		verify(user, never()).addPointsStorageAware(5);
	}

	@Test
	void asyncRewardPropagatesPersistenceFailure() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(advancedUser)).thenReturn(user);
		when(user.addPointsStorageAwareAsync(5)).thenReturn(
				CompletableFuture.failedFuture(new IllegalStateException("write failed")));

		CompletableFuture<String> result = new RewardPoints(plugin)
				.onRewardRequestAsync(mock(Reward.class), advancedUser, 5, new HashMap<>()).toCompletableFuture();

		org.junit.jupiter.api.Assertions.assertThrows(CompletionException.class, result::join);
	}

	@Test
	void durableReplayCheckpointAcknowledgesTheMatchingPointOperation() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(advancedUser)).thenReturn(user);
		when(user.getUUID()).thenReturn("player-uuid");
		when(user.acknowledgeStorageAwarePointOperation(org.mockito.ArgumentMatchers.anyString()))
				.thenReturn(CompletableFuture.completedFuture(null));

		new RewardPoints(plugin).onReplayCheckpointPersisted(mock(Reward.class), advancedUser, "occurrence-1",
				"AsyncReward/0").toCompletableFuture().join();

		verify(user).acknowledgeStorageAwarePointOperation(
				"1d257d984bf6531c07e366b02a5373043961a6e44c23528f91d027c0d2c83f64");
	}
}
