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
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.bukkit.configuration.file.YamlConfiguration;
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class VotingPluginUserOfflineVoteReplayTest {
	@Test
	void clearsOfflineVotesBeforeStartingRewardsSoPermanentFailuresDoNotReplayForever() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getOptions().isProcessRewards()).thenReturn(true);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		UserData data = mock(UserData.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		VoteSiteManager manager = mock(VoteSiteManager.class);
		VoteSite site = mock(VoteSite.class);
		when(plugin.getVoteSiteManager()).thenReturn(manager);
		when(manager.hasVoteSite("Site1")).thenReturn(true);
		when(manager.getVoteSite("Site1", true)).thenReturn(site);
		doReturn(false).when(user).isTopVoterIgnore();
		doReturn(new ArrayList<>(List.of("Site1"))).when(user).getOfflineVotes();
		doNothing().when(user).sendVoteEffects(false);
		doNothing().when(user).setOfflineVotes(org.mockito.ArgumentMatchers.any());
		doThrow(new IllegalStateException("reward failed")).when(user).playerVote(site, false, false);

		assertThrows(IllegalStateException.class,
				() -> user.offVoteWithCapturedTopVoterIgnore(false));

		InOrder order = inOrder(user);
		order.verify(user).setOfflineVotes(org.mockito.ArgumentMatchers.argThat(List::isEmpty));
		order.verify(user).playerVote(site, false, false);
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
		doReturn(false).when(user).isTopVoterIgnore();
		doNothing().when(user).cache();

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
		return new AsyncReplayFixture(user, site, queued, pending, anySiteRewards);
	}

	private record AsyncReplayFixture(VotingPluginUser user, VoteSite site,
			AtomicReference<ArrayList<String>> queued,
			AtomicReference<ArrayList<String>> pending,
			CompletableFuture<Void> anySiteRewards) { }

}
