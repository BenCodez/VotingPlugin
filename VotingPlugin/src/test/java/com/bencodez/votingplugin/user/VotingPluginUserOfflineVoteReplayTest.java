package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.bukkit.configuration.file.YamlConfiguration;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class VotingPluginUserOfflineVoteReplayTest {
	@Test
	void legacyEntryRetainsQueueAndPendingMarkerWhenRewardDeliveryFails() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.user.offVoteWithCapturedTopVoterIgnore(false);
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
		assertTrue(fixture.user.isOfflineVoteRewardReplayActive());
		fixture.anySiteRewards.completeExceptionally(new IllegalStateException("reward failed"));
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
		fixture.user.offVoteWithCapturedTopVoterIgnore(false);
		verify(fixture.user, never()).sendVoteEffects(false);
		verify(fixture.user, never()).playerVote(fixture.site, false, false);
	}
	@Test
	void asyncReplayRetainsOriginalVotesUntilRewardCompletionAndKeepsNewVotes() {
		AsyncReplayFixture fixture = asyncFixture();
		CompletableFuture<Void> confirmed = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();

		// The original votes must survive a crash or shutdown before reward completion.
		assertEquals(List.of("Site1"), fixture.pending.get());
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertFalse(confirmed.isDone());
		verify(fixture.user, never()).sendVoteEffects(false);

		// A vote arriving while rewards are in flight must not be erased by commit.
		fixture.queued.set(new ArrayList<>(List.of("Site1", "NewVote")));
		fixture.anySiteRewards.complete(null);
		confirmed.join();
		assertEquals(List.of("NewVote"), fixture.queued.get());
		assertTrue(fixture.pending.get().isEmpty());
		verify(fixture.site).giveRewardsAsync(fixture.user, false, false);
	}

	@Test
	void failedAsyncReplayLeavesOriginalVotesAndBlocksLegacyOrRepeatedGrants() {
		AsyncReplayFixture fixture = asyncFixture();
		CompletableFuture<Void> confirmed = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();
		fixture.anySiteRewards.completeExceptionally(new IllegalStateException("partial reward effects"));

		assertThrows(CompletionException.class, confirmed::join);
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
		assertThrows(CompletionException.class, () ->
				fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false).toCompletableFuture().join());

		// The legacy background and join paths must not replay this batch either.
		fixture.user.offVoteWithCapturedTopVoterIgnore(false);
		verify(fixture.user, never()).sendVoteEffects(false);
		verify(fixture.user, never()).playerVote(fixture.site, false, false);
		assertEquals(List.of("Site1"), fixture.queued.get());
	}

	@Test
	void changedOriginalQueueNeverClearsPendingRecoveryRecord() {
		AsyncReplayFixture fixture = asyncFixture();
		CompletableFuture<Void> confirmed = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();
		fixture.queued.set(new ArrayList<>(List.of("Unexpected")));
		fixture.anySiteRewards.complete(null);

		assertThrows(CompletionException.class, confirmed::join);
		assertEquals(List.of("Unexpected"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
	}

	@Test
	void confirmedDeliveredRecoveryRemovesOnlyReviewedPrefixAndUnblocksNewVotes() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.queued.set(new ArrayList<>(List.of("Site1", "NewVote")));

		fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"),
				List.of("Site1", "NewVote"), "delivered");

		assertTrue(fixture.pending.get().isEmpty());
		assertEquals(List.of("NewVote"), fixture.queued.get());
	}

	@Test
	void reviewedSafeRetryPreservesOriginalVotesWhileClearingPending() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"), List.of("Site1"), "retry");
		assertTrue(fixture.pending.get().isEmpty());
		assertEquals(List.of("Site1"), fixture.queued.get());
	}

	@Test
	void alreadyClearedRecoveryUnblocksWithoutDiscardingNewVotes() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.queued.set(new ArrayList<>(List.of("NewVote")));
		fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"),
				List.of("NewVote"), "already-cleared");
		assertTrue(fixture.pending.get().isEmpty());
		assertEquals(List.of("NewVote"), fixture.queued.get());
	}

	@Test
	void recoveryRejectsStalePreviewAndActiveDelivery() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.queued.set(new ArrayList<>(List.of("Site1", "NewVote")));
		assertThrows(IllegalStateException.class, () ->
				fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"), List.of("Site1"), "delivered"));
		assertEquals(List.of("Site1"), fixture.pending.get());

		fixture.pending.set(new ArrayList<>());
		fixture.queued.set(new ArrayList<>(List.of("Site1")));
		CompletableFuture<Void> delivery = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();
		assertTrue(fixture.user.isOfflineVoteRewardReplayActive());
		assertThrows(IllegalStateException.class, () -> fixture.user.reconcileOfflineVoteRewardBatch(
				List.of("Site1"), List.of("Site1"), "retry"));
		fixture.anySiteRewards.completeExceptionally(new IllegalStateException("partial effects"));
		assertThrows(CompletionException.class, delivery::join);
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
	}


	@Test
	void deliveredBatchCommitCannotLeaveAnOldPendingMarkerForIdenticalNewVotes() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"), List.of("Site1"), "delivered");
		assertTrue(fixture.pending.get().isEmpty());
		assertTrue(fixture.queued.get().isEmpty());
		verify(fixture.user.getUserData()).setValues(
				org.mockito.ArgumentMatchers.any(HashMap.class));

		// This later same-site vote is new, not the completed old batch.
		fixture.queued.set(new ArrayList<>(List.of("Site1")));
		assertTrue(fixture.pending.get().isEmpty());
		assertEquals(List.of("Site1"), fixture.queued.get());
	}

	@Test
	void transactionalBatchFailureDoesNotClearQueueOrPendingRecoveryState() {
		AsyncReplayFixture fixture = asyncFixture();
		// Evaluate the spy getter before starting Mockito's doThrow stubbing.
		UserData persistedUserData = fixture.user.getUserData();
		doThrow(new IllegalStateException("database write failed")).when(persistedUserData)
				.setValues(org.mockito.ArgumentMatchers.any(HashMap.class));
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		assertThrows(IllegalStateException.class, () ->
				fixture.user.reconcileOfflineVoteRewardBatch(List.of("Site1"), List.of("Site1"), "delivered"));
		assertEquals(List.of("Site1"), fixture.pending.get());
		assertEquals(List.of("Site1"), fixture.queued.get());
	}

	@Test
	void clearingUnresolvedBatchPreservesQueueAndRecoveryEvidence() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		assertThrows(IllegalStateException.class, fixture.user::clearOfflineVotes);
		assertThrows(IllegalStateException.class,
				() -> fixture.user.setOfflineVotes(new ArrayList<>(List.of("OtherSite"))));
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
		verify(fixture.user.getUserData(), never()).setStringList(eq("OfflineVotes"), org.mockito.ArgumentMatchers.any());
		verify(fixture.user, never()).setOfflineRewards(org.mockito.ArgumentMatchers.any(ArrayList.class));
	}

	@Test
	void pendingBatchStillAllowsNewVotesToBeAppended() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.pending.set(new ArrayList<>(List.of("Site1")));
		fixture.user.setOfflineVotes(new ArrayList<>(List.of("Site1", "NewVote")));
		verify(fixture.user.getUserData()).setStringList("OfflineVotes",
				new ArrayList<>(List.of("Site1", "NewVote")));
		assertEquals(List.of("Site1"), fixture.pending.get());
	}

	@Test
	void ordinaryClearStillWorksWithoutUnresolvedRewards() {
		AsyncReplayFixture fixture = asyncFixture();
		fixture.user.clearOfflineVotes();
		verify(fixture.user.getUserData()).setStringList("OfflineVotes", new ArrayList<>());
	}

	/** Mock persistence boundaries but exercise the real async user method. */
	private static AsyncReplayFixture asyncFixture() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getOptions().isProcessRewards()).thenReturn(true);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		UserData data = mock(UserData.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		// The spy must own the mocked UserData; the base-user mock alone does not
		// override AdvancedCoreUser.getUserData() on the wrapper instance.
		doReturn(data).when(user).getUserData();
		doReturn(false).when(user).isTopVoterIgnore();
		doNothing().when(user).cache();
		// AdvancedCore owns generic queued rewards; this fixture tests the vote queue.
		doNothing().when(user).setOfflineRewards(org.mockito.ArgumentMatchers.any(ArrayList.class));

		AtomicReference<ArrayList<String>> queued = new AtomicReference<>(
				new ArrayList<>(List.of("Site1")));
		AtomicReference<ArrayList<String>> pending = new AtomicReference<>(new ArrayList<>());
		doAnswer(call -> new ArrayList<>(queued.get())).when(user).getOfflineVotes();
		doAnswer(call -> new ArrayList<>(pending.get())).when(data).getStringList("OfflineVotesRewardPending");
		doAnswer(call -> {
			queued.set(new ArrayList<>(call.getArgument(1)));
			return null;
		}).when(data).setStringList(eq("OfflineVotes"), org.mockito.ArgumentMatchers.any(ArrayList.class), eq(false));
		doAnswer(call -> {
			pending.set(new ArrayList<>(call.getArgument(1)));
			return null;
		}).when(data).setStringList(eq("OfflineVotesRewardPending"),
				org.mockito.ArgumentMatchers.any(ArrayList.class), eq(false));
		// Transactionally write queue + pending flag in ONE AdvancedCore setValues.
		doAnswer(call -> {
			@SuppressWarnings("unchecked")
			HashMap<String, DataValue> values = call.getArgument(0);
			assertEquals(java.util.Set.of("OfflineVotes", "OfflineVotesRewardPending"), values.keySet());
			String updatedQueue = values.get("OfflineVotes").getString();
			String updatedPending = values.get("OfflineVotesRewardPending").getString();
			queued.set(updatedQueue.isEmpty() ? new ArrayList<>()
					: new ArrayList<>(List.of(updatedQueue.split("%line%"))));
			pending.set(updatedPending.isEmpty() ? new ArrayList<>()
					: new ArrayList<>(List.of(updatedPending.split("%line%"))));
			return null;
		}).when(data).setValues(org.mockito.ArgumentMatchers.any(HashMap.class));

		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
		doAnswer(call -> {
			call.getArgument(0, Runnable.class).run();
			return null;
		}).when(timer).execute(org.mockito.ArgumentMatchers.any(Runnable.class));

		RewardHandler handler = mock(RewardHandler.class);
		when(plugin.getRewardHandler()).thenReturn(handler);
		SpecialRewardsConfig config = mock(SpecialRewardsConfig.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getSpecialRewardsConfig()).thenReturn(config);
		when(config.getData()).thenReturn(yaml);
		when(config.getAnySiteRewardsPath()).thenReturn("AnySiteRewards");
		CompletableFuture<Void> anySiteRewards = new CompletableFuture<>();
		when(handler.giveRewardAsync(eq(user), eq(yaml), eq("AnySiteRewards"),
				org.mockito.ArgumentMatchers.any(RewardOptions.class))).thenReturn(anySiteRewards);

		VoteSiteManager sites = mock(VoteSiteManager.class);
		VoteSite site = mock(VoteSite.class);
		when(plugin.getVoteSiteManager()).thenReturn(sites);
		when(sites.resolveVoteSite("Site1", true)).thenReturn(site);
		when(site.giveRewardsAsync(user, false, false)).thenReturn(CompletableFuture.completedFuture(null));
		return new AsyncReplayFixture(plugin, user, site, queued, pending, anySiteRewards);
	}

	@Test
	void synchronousReplayCannotEnterBeforeAsyncPendingMarkerIsPublished() {
		AsyncReplayFixture fixture = asyncFixture();
		UserData data = fixture.user.getUserData();
		doAnswer(call -> {
			assertTrue(fixture.user.isOfflineVoteRewardReplayActive());
			assertTrue(fixture.pending.get().isEmpty());
			fixture.user.offVoteWithCapturedTopVoterIgnore(false);
			fixture.pending.set(new ArrayList<>(call.getArgument(1)));
			return null;
		}).when(data).setStringList(eq("OfflineVotesRewardPending"),
				org.mockito.ArgumentMatchers.any(ArrayList.class), eq(false));
		CompletableFuture<Void> delivery = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();
		verify(fixture.user, never()).sendVoteEffects(false);
		verify(fixture.user, never()).playerVote(fixture.site, false, false);
		fixture.anySiteRewards.complete(null);
		delivery.join();
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
	}

	@Test
	void asyncReplayCannotEnterWhileSynchronousOwnerReadsTheQueue() {
		AsyncReplayFixture fixture = asyncFixture();
		doAnswer(call -> {
			assertTrue(fixture.user.isOfflineVoteRewardReplayActive());
			assertThrows(CompletionException.class, () -> fixture.user
					.offVoteWithCapturedTopVoterIgnoreAsync(false).toCompletableFuture().join());
			return new ArrayList<String>();
		}).when(fixture.user).getOfflineVotes();
		fixture.user.offVoteWithCapturedTopVoterIgnore(false);
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
		verify(fixture.user, never()).sendVoteEffects(false);
	}

	@Test
	void bulkClearExcludesBothReplayAdmissionsWithoutSpendingRewardEffects() {
		AsyncReplayFixture fixture = asyncFixture();
		VotingPluginUser.beginOfflineVoteBulkClear();
		try {
			fixture.user.offVoteWithCapturedTopVoterIgnore(false);
			assertThrows(CompletionException.class, () -> fixture.user
					.offVoteWithCapturedTopVoterIgnoreAsync(false).toCompletableFuture().join());
			verify(fixture.user, never()).getOfflineVotes();
			verify(fixture.user, never()).sendVoteEffects(false);
		} finally {
			VotingPluginUser.endOfflineVoteBulkClear();
		}
		CompletableFuture<Void> delivery = fixture.user.offVoteWithCapturedTopVoterIgnoreAsync(false)
				.toCompletableFuture();
		assertThrows(IllegalStateException.class, VotingPluginUser::beginOfflineVoteBulkClear);
		fixture.anySiteRewards.complete(null);
		delivery.join();
	}

	@Test
	void legacyEntryDefersAllStorageAndRewardsUntilWorkerRuns() {
		AsyncReplayFixture fixture = asyncFixture();
		ScheduledExecutorService worker = fixture.plugin.getUserManager().getDataManager().getTimer();
		AtomicReference<Runnable> admitted = new AtomicReference<>();
		doAnswer(call -> { admitted.set(call.getArgument(0)); return null; })
				.when(worker).execute(org.mockito.ArgumentMatchers.any(Runnable.class));
		fixture.user.offVoteWithCapturedTopVoterIgnore(false);
		verify(fixture.user, never()).cache();
		verify(fixture.user, never()).getOfflineVotes();
		assertTrue(fixture.pending.get().isEmpty());
		admitted.get().run();
		assertEquals(List.of("Site1"), fixture.pending.get());
		fixture.anySiteRewards.complete(null);
		admitted.get().run();
		assertTrue(fixture.queued.get().isEmpty());
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
	}

	@Test
	void legacyContinuationWaitsForDurableReplayCompletion() {
		AsyncReplayFixture fixture = asyncFixture();
		AtomicReference<Boolean> continued = new AtomicReference<>(false);

		fixture.user.offVoteWithCapturedTopVoterIgnoreAndThen(false, () -> continued.set(true));
		assertFalse(continued.get(), "generic offline rewards must not overtake replay delivery");

		fixture.anySiteRewards.complete(null);
		assertTrue(continued.get(), "continuation must run after the queue/pending commit");
		assertTrue(fixture.queued.get().isEmpty());
		assertTrue(fixture.pending.get().isEmpty());
	}

	@Test
	void failedLegacyReplaySkipsContinuationAndReleasesReplayFence() {
		AsyncReplayFixture fixture = asyncFixture();
		AtomicReference<Boolean> continued = new AtomicReference<>(false);

		fixture.user.offVoteWithCapturedTopVoterIgnoreAndThen(false, () -> continued.set(true));
		fixture.anySiteRewards.completeExceptionally(new IllegalStateException("delivery failed"));

		assertFalse(continued.get(), "generic offline rewards must not run after failed replay");
		assertFalse(fixture.user.isOfflineVoteRewardReplayActive());
		assertEquals(List.of("Site1"), fixture.queued.get());
		assertEquals(List.of("Site1"), fixture.pending.get());
	}

	private record AsyncReplayFixture(VotingPluginMain plugin, VotingPluginUser user, VoteSite site,
			AtomicReference<ArrayList<String>> queued,
			AtomicReference<ArrayList<String>> pending,
			CompletableFuture<Void> anySiteRewards) { }

}
