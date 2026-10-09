package com.bencodez.votingplugin.backendproxy.messaging;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache.Reservation;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.data.ServerData;

class BackendProxyMessageRouterTest {

	private static final UUID PLAYER_UUID = UUID.fromString("e5baec32-9b2c-4fc8-9aed-0e0285e3c33d");
	private static final long LAST_VOTE_TIME = 1_788_201_600_000L;

	private VotingPluginMain plugin;
	private VoteSiteManager voteSiteManager;
	private VotingPluginUser user;
	private com.bencodez.advancedcore.api.user.UserManager coreUserManager;
	private UserDataManager dataManager;
	private Logger logger;
	private BackendProxyMessageRouter router;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class);
		voteSiteManager = mock(VoteSiteManager.class);
		UserManager votingUserManager = mock(UserManager.class);
		coreUserManager = mock(com.bencodez.advancedcore.api.user.UserManager.class);
		dataManager = mock(UserDataManager.class);
		AdvancedCoreUser resolvedUser = mock(AdvancedCoreUser.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		user = mock(VotingPluginUser.class);
		logger = mock(Logger.class);

		when(plugin.getVoteSiteManager()).thenReturn(voteSiteManager);
		when(plugin.getVotingPluginUserManager()).thenReturn(votingUserManager);
		when(plugin.getUserManager()).thenReturn(coreUserManager);
		when(coreUserManager.getDataManager()).thenReturn(dataManager);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(plugin.getLogger()).thenReturn(logger);
		when(votingUserManager.getVotingPluginUser(resolvedUser)).thenReturn(user);
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(1);
			task.run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			Consumer<AdvancedCoreUser> success = invocation.getArgument(1);
			success.accept(resolvedUser);
			return null;
		}).when(coreUserManager).getUserAsync(eq(PLAYER_UUID), any(), any());

		router = new BackendProxyMessageRouter(plugin, mock(BackendPresenceManager.class),
				mock(BackendGlobalDataSync.class), mock(BackendVotePartySync.class),
				mock(ProcessedVoteCache.class));
	}

	@Test
	void ignoresLastVoteTimeForUnknownServiceSite() {
		router.handleVoteUpdate(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"unknown.example\nforged-entry", LAST_VOTE_TIME, ""));

		verify(user).cache();
		verify(user).offVote();
		verify(user, never()).setTime(any(), anyLong());
		verify(logger).warning("Ignoring VoteUpdate last vote time for unresolved or disabled service site: "
				+ "unknown.example?forged-entry");
		verify(plugin).setUpdate(true);
	}

	@Test
	void appliesLastVoteTimeForKnownServiceSite() {
		VoteSite voteSite = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(voteSite);

		router.handleVoteUpdate(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""));

		verify(user).cache();
		verify(user).offVote();
		verify(user).setTime(voteSite, LAST_VOTE_TIME);
		verify(logger, never()).warning(any(String.class));
		verify(plugin).setUpdate(true);
	}

	@Test
	void sharedVoteUpdateFinishesStorageWorkBeforePlatformCompletion() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		when(user.getPlayer()).thenReturn(player);
		when(player.isOnline()).thenReturn(true);
		when(player.hasPermission("VotingPlugin.TopVoter.Ignore")).thenReturn(true);
		BukkitScheduler scheduler = plugin.getBukkitScheduler();
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(1);
			task.run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		VoteSite site = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(site);
		java.util.concurrent.atomic.AtomicBoolean insideStorage = new java.util.concurrent.atomic.AtomicBoolean();
		doAnswer(invocation -> {
			assertTrue(insideStorage.get(), "LastVotes must be updated on the storage worker");
			return null;
		}).when(user).setTime(site, LAST_VOTE_TIME);

		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		assertEquals(null, outcome.get());
		verify(user, never()).cache();
		insideStorage.set(true);
		Boolean result = pending.work.get();
		insideStorage.set(false);
		org.mockito.InOrder order = org.mockito.Mockito.inOrder(user);
		order.verify(user).cache();
		order.verify(user).offVoteWithCapturedTopVoterIgnore(true);
		order.verify(user).setTime(site, LAST_VOTE_TIME);
		verify(user, never()).offVote();
		verify(plugin, never()).setUpdate(true);

		// A cache eviction after worker completion cannot make the platform
		// callback read LastVotes again or replay offline effects.
		pending.success.accept(result);
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(user, times(1)).setTime(site, LAST_VOTE_TIME);
		verify(user, times(1)).offVoteWithCapturedTopVoterIgnore(true);
		verify(plugin).setUpdate(true);
	}

	@Test
	void sharedVoteUpdateCacheFailureBeforeEffectsIsRetryable() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		doThrow(new IllegalStateException("cache failed")).when(user).cache();
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		IllegalStateException failure = assertThrows(IllegalStateException.class, pending.work::get);
		pending.failure.accept(failure);
		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());
		verify(user, never()).offVoteWithCapturedTopVoterIgnore(anyBoolean());
		verify(user, never()).setTime(any(), anyLong());
		verify(plugin, never()).setUpdate(true);
	}

	@Test
	void sharedVoteUpdateLookupFailureBeforeMutationsIsRetryable() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		when(voteSiteManager.getVoteSite("known.example", true))
				.thenThrow(new IllegalStateException("vote sites unavailable"));
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		IllegalStateException failure = assertThrows(IllegalStateException.class, pending.work::get);
		pending.failure.accept(failure);
		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());
		verify(user).cache();
		verify(user, never()).offVoteWithCapturedTopVoterIgnore(anyBoolean());
		verify(user, never()).setTime(any(), anyLong());
		verify(plugin, never()).setUpdate(true);
	}

	@Test
	void sharedVoteUpdateFailureAfterOfflineEffectsIsQuarantined() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		when(user.getPlayer()).thenReturn(player);
		when(player.isOnline()).thenReturn(true);
		BukkitScheduler scheduler = plugin.getBukkitScheduler();
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(1);
			task.run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(player));
		VoteSite site = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(site);
		doThrow(new IllegalStateException("timestamp failed")).when(user).setTime(site, LAST_VOTE_TIME);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		IllegalStateException failure = assertThrows(IllegalStateException.class, pending.work::get);
		pending.failure.accept(failure);
		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());
		verify(user, times(1)).offVoteWithCapturedTopVoterIgnore(false);
		verify(user, never()).offVote();
		verify(plugin, never()).setUpdate(true);
	}

	@Test
	void sharedVoteUpdateDoesNotRunOfflineRewardsForAbsentPlayer() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		VoteSite site = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(site);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		pending.success.accept(pending.work.get());
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(user, never()).offVoteWithCapturedTopVoterIgnore(anyBoolean());
		verify(user, never()).offVote();
		verify(user).setTime(site, LAST_VOTE_TIME);
	}

	@Test
	void sharedVoteUpdateEntityRetirementUsesGlobalFallbackWithoutRewards() {
		DeferredVoteUpdate pending = captureSharedVoteUpdate();
		org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
		when(user.getPlayer()).thenReturn(player);
		BukkitScheduler scheduler = plugin.getBukkitScheduler();
		doThrow(new IllegalStateException("entity retired")).when(scheduler)
				.runTask(eq(plugin), any(Runnable.class), eq(player));
		VoteSite site = mock(VoteSite.class);
		when(voteSiteManager.getVoteSite("known.example", true)).thenReturn(site);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		pending.success.accept(pending.work.get());
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(user).cache();
		verify(user, never()).offVoteWithCapturedTopVoterIgnore(anyBoolean());
		verify(user).setTime(site, LAST_VOTE_TIME);
	}

	@Test
	void sharedVoteUpdateDoesNotFallBackToPlatformStorageWhenSubmissionIsRejected() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(true);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), outcome::set);

		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());
		verify(user, never()).cache();
		verify(user, never()).offVote();
		verify(user, never()).setTime(any(), anyLong());
	}

	private DeferredVoteUpdate captureSharedVoteUpdate() {
		when(dataManager.hasSharedSqlBackend()).thenReturn(true);
		DeferredVoteUpdate pending = new DeferredVoteUpdate();
		doAnswer(invocation -> {
			pending.work = invocation.getArgument(0);
			pending.success = invocation.getArgument(1);
			pending.failure = invocation.getArgument(2);
			return true;
		}).when(dataManager).deferSharedStorageResultFromPlatform(any(), any(), any());
		return pending;
	}

	private static final class DeferredVoteUpdate {
		private java.util.function.Supplier<Boolean> work;
		private Consumer<Boolean> success;
		private Consumer<Throwable> failure;
	}

	@Test
	void releasesOrderedVoteLaneWhenUuidResolutionFails() {
		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			Consumer<Throwable> failure = invocation.getArgument(2);
			failure.accept(new IllegalStateException("missing"));
			return null;
		}).when(coreUserManager).getUserAsync(eq(PLAYER_UUID), any(), any());

		AtomicInteger completions = new AtomicInteger();
		router.handleVoteUpdate(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), completions::incrementAndGet);

		assertEquals(1, completions.get());
		verify(user, never()).offVote();
		verify(plugin, never()).setUpdate(true);
	}

	@Test
	void reportsTransientResolutionFailureWithoutCompletingDurableVoteUpdate() {
		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			Consumer<Throwable> failure = invocation.getArgument(2);
			failure.accept(new IllegalStateException("storage unavailable"));
			return null;
		}).when(coreUserManager).getUserAsync(eq(PLAYER_UUID), any(), any());

		AtomicReference<OrderedVoteOutcome> successful = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
				"known.example", LAST_VOTE_TIME, ""), successful::set);

		assertEquals(OrderedVoteOutcome.RETRY, successful.get());
		verify(user, never()).offVote();
		verify(plugin, never()).setUpdate(true);
	}

	@Test
	void invalidUuidWarningIsSingleLineAndTerminal() {
		AtomicReference<OrderedVoteOutcome> successful = new AtomicReference<>();
		router.handleOrderedVote(VotingPluginWire.voteUpdate("invalid\nforged", 1, 10,
				"known.example", LAST_VOTE_TIME, ""), successful::set);

		assertEquals(OrderedVoteOutcome.COMPLETE, successful.get());
		verify(logger).warning("Invalid UUID in VoteUpdate: invalid?forged");
		verify(user, never()).offVote();
	}

	@Test
	void voteUpdateFailureAfterOfflineEffectRequestsQuarantine() {
		doThrow(new IllegalStateException("after offline effect")).when(user).offVote();
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		assertThrows(IllegalStateException.class, () -> router.handleOrderedVote(
				VotingPluginWire.voteUpdate(PLAYER_UUID.toString(), 1, 10,
						"known.example", LAST_VOTE_TIME, ""), outcome::set));
		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());
	}

	@Test
	void voteRewardFailureRequestsQuarantineAfterVoteIdReservation() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED, Reservation.DUPLICATE);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player"))
				.thenReturn(user);
		doThrow(new IllegalStateException("partial reward")).when(user).bungeeVotePluginMessaging(
				any(), anyLong(), any(), anyBoolean(), anyBoolean(), anyBoolean(), anyInt(), anyBoolean(), anyBoolean(),
				anyBoolean(), any(), anyBoolean());
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		assertThrows(IllegalStateException.class, () -> voteRouter.handleOrderedVote(
				VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME,
						true, true, "", voteId, false, false, 1, 1), outcome::set));
		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());

		outcome.set(null);
		voteRouter.handleOrderedVote(
				VotingPluginWire.requestVoteDeliveryAcknowledgement(
						VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME,
								true, true, "", voteId, false, false, 1, 1)),
				outcome::set);
		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());
		verify(cache, never()).complete(voteId);
	}

	@Test
	void reliableVoteRetriesReceiptPersistenceWithoutRepeatingEffectsInProcess() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED, Reservation.DUPLICATE);
		when(cache.complete(voteId)).thenReturn(false, true);
		when(cache.hasCompletedEffects(voteId)).thenReturn(true);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player"))
				.thenReturn(user);
		when(plugin.getServerData()).thenReturn(mock(ServerData.class));
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		JsonEnvelope vote = VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME,
						true, true, "", voteId, false, false, 1, 1, true));

		voteRouter.handleOrderedVote(vote, outcome::set);
		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());

		voteRouter.handleOrderedVote(vote, outcome::set);
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(cache, times(2)).complete(voteId);
		verify(user, times(1)).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), eq(false), eq(true), eq(true), eq(voteId), eq(false));
	}

	@Test
	void stableVoteIdAloneDoesNotBypassBackendDelayValidation() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserve(voteId)).thenReturn(true);
		when(cache.complete(voteId)).thenReturn(true);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player"))
				.thenReturn(user);
		when(plugin.getServerData()).thenReturn(mock(ServerData.class));
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);

		voteRouter.handleOrderedVote(VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example",
				LAST_VOTE_TIME, true, true, "", voteId, false, false, 1, 1), ignored -> { });

		verify(user).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), eq(false), eq(false), eq(true), eq(voteId), eq(false));
	}

	@Test
	void voteOnlineMarksTheDeliveryAsTargetedToThisBackend() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED);
		when(cache.complete(voteId)).thenReturn(true);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player")).thenReturn(user);
		when(plugin.getServerData()).thenReturn(mock(ServerData.class));
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);

		voteRouter.handleOrderedVote(VotingPluginWire.voteOnline("Player", PLAYER_UUID.toString(), "known.example",
				LAST_VOTE_TIME, true, true, "", voteId, false, false, 1, 1), ignored -> { });

		verify(user).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), eq(voteId), eq(true));
	}

	@Test
	void malformedReliableVoteIsQuarantinedWithoutCompletionReceipt() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();
		JsonEnvelope vote = VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.vote("Player", "invalid-uuid", "known.example", LAST_VOTE_TIME,
						true, true, "", voteId, false, false, 1, 1));

		voteRouter.handleOrderedVote(vote, outcome::set);

		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());
		verify(cache, never()).complete(voteId);
		verify(user, never()).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), any(), anyBoolean());
		verify(cache).cancelReservation(voteId);
	}

	@Test
	void saturatedReliableVoteRequestsRetryWithoutRunningEffects() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.SATURATED);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME,
						true, true, "", voteId, false, false, 1, 1)), outcome::set);

		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());
		verify(user, never()).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), any(), anyBoolean());
		verify(cache, never()).complete(voteId);
	}

	@Test
	void duplicateRetriesDoNotResetSaturationWarningSuppression() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.SATURATED, Reservation.DUPLICATE,
				Reservation.SATURATED);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		JsonEnvelope vote = VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.vote("Player", PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME,
						true, true, "", voteId, false, false, 1, 1));

		voteRouter.handleOrderedVote(vote, ignored -> { });
		voteRouter.handleOrderedVote(vote, ignored -> { });
		voteRouter.handleOrderedVote(vote, ignored -> { });

		verify(logger, times(1)).warning("Backend vote replay cache is full; retaining votes for retry");
	}

	@Test
	void unknownReceiptReleaseIsValidAndAcknowledgesDurableTombstone() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.releaseCompletedReceipt(voteId)).thenReturn(true);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		when(options.getServer()).thenReturn("survival");
		when(plugin.getOptions()).thenReturn(options);
		GlobalMessageHandler messages = mock(GlobalMessageHandler.class);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		voteRouter.register(messages, BungeeMethod.REDIS);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		JsonEnvelope release = VotingPluginWire.voteDeliveryReceiptRelease(
				"survival", voteId, VotingPluginWire.SUB_VOTE);
		assertTrue(voteRouter.isValidReceiptRelease(release));
		voteRouter.handleOrderedVote(release, outcome::set);

		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(cache).releaseCompletedReceipt(voteId);
		org.mockito.ArgumentCaptor<JsonEnvelope> acknowledgement = org.mockito.ArgumentCaptor
				.forClass(JsonEnvelope.class);
		verify(messages).sendMessage(acknowledgement.capture());
		assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK,
				acknowledgement.getValue().getSubChannel());
	}

	@Test
	void delayRejectionReceiptReleaseIsValid() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.hasDurableReceipt(voteId)).thenReturn(true);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		when(options.getServer()).thenReturn("survival");
		when(plugin.getOptions()).thenReturn(options);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		JsonEnvelope release = VotingPluginWire.voteDeliveryReceiptRelease(
				"survival", voteId, VotingPluginWire.SUB_VOTE_DELAY_REJECTED);

		assertTrue(voteRouter.isValidReceiptRelease(release));
		assertTrue(voteRouter.hasDurableReceiptForRelease(release));
	}

	@Test
	void receiptReleaseBypassesOrderingOnlyAfterItsReceiptIsDurable() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		when(options.getServer()).thenReturn("survival");
		when(plugin.getOptions()).thenReturn(options);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		JsonEnvelope release = VotingPluginWire.voteDeliveryReceiptRelease(
				"survival", voteId, VotingPluginWire.SUB_VOTE);
		when(cache.hasDurableReceipt(voteId)).thenReturn(false, true);

		assertFalse(voteRouter.hasDurableReceiptForRelease(release));
		assertTrue(voteRouter.hasDurableReceiptForRelease(release));
	}

	@Test
	void reliableDelayRejectionExecutesOnceAndPersistsItsVoteId() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED, Reservation.DUPLICATE);
		when(cache.hasCompletedEffects(voteId)).thenReturn(true);
		when(cache.complete(voteId)).thenReturn(true);
		VoteSite site = configureDelayRejection(true);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		JsonEnvelope rejection = VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.voteDelayRejected("Player", PLAYER_UUID.toString(),
						"known.example", true, voteId));
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(rejection, outcome::set);
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		voteRouter.handleOrderedVote(rejection, outcome::set);

		verify(site, times(1)).giveWaitUntilVoteDelayRewards(user, false, true);
		verify(cache, times(2)).reserveWithOutcome(voteId);
		verify(cache, times(2)).complete(voteId);
	}

	@Test
	void explicitQueuedDeliveryIsPassedSeparatelyFromDelayValidation() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.RESERVED);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player")).thenReturn(user);
		when(plugin.getServerData()).thenReturn(mock(ServerData.class));
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);

		JsonEnvelope queued = VotingPluginWire.queuedDelivery(VotingPluginWire.voteOnline("Player",
				PLAYER_UUID.toString(), "known.example", LAST_VOTE_TIME, true, true, "", voteId,
				false, false, 1, 1, true));
		voteRouter.handleOrderedVote(queued, ignored -> { });

		verify(user).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt(), eq(true), eq(true), eq(true), eq(voteId), eq(true));
	}

	@Test
	void saturatedReliableDelayRejectionRetriesWithoutRunningRewards() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserveWithOutcome(voteId)).thenReturn(Reservation.SATURATED);
		VoteSite site = configureDelayRejection(true);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.voteDelayRejected("Player", PLAYER_UUID.toString(),
						"known.example", true, voteId)), outcome::set);

		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());
		verify(site, never()).giveWaitUntilVoteDelayRewards(any(), anyBoolean(), anyBoolean());
		verify(cache, never()).complete(voteId);
	}

	@Test
	void legacyDelayRejectionPreservesProxyAuthoritativeRewardDecision() {
		VoteSite site = configureDelayRejection(false);
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(VotingPluginWire.voteDelayRejected("Player", PLAYER_UUID.toString(),
				"known.example", true), outcome::set);

		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(site).giveWaitUntilVoteDelayRewards(user, false, true);
		verify(user, never()).canVoteSite(site);
		verify(cache, never()).reserve(any());
	}

	@Test
	void capableProxyDelayRejectionWithoutVoteIdIsQuarantined() {
		configureDelayRejection(true);
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.voteDelayRejected("Player", PLAYER_UUID.toString(),
						"known.example", true)), outcome::set);

		assertEquals(OrderedVoteOutcome.QUARANTINE, outcome.get());
		verify(cache, never()).reserve(any());
	}

	@Test
	void delayRejectionRechecksCurrentSiteAndDelayState() {
		UUID voteId = UUID.randomUUID();
		VoteSite site = configureDelayRejection(false);
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.complete(voteId)).thenReturn(true);
		BackendProxyMessageRouter voteRouter = new BackendProxyMessageRouter(plugin,
				mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
				mock(BackendVotePartySync.class), cache);
		AtomicReference<OrderedVoteOutcome> outcome = new AtomicReference<>();

		voteRouter.handleOrderedVote(VotingPluginWire.requestVoteDeliveryAcknowledgement(
				VotingPluginWire.voteDelayRejected("Player", PLAYER_UUID.toString(),
						"known.example", true, voteId)), outcome::set);

		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(site, never()).giveWaitUntilVoteDelayRewards(any(), anyBoolean(), anyBoolean());
		verify(cache, never()).reserve(voteId);
		verify(cache).complete(voteId);
	}

	private VoteSite configureDelayRejection(boolean stillDelayed) {
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		when(options.isProcessRewards()).thenReturn(true);
		when(plugin.getOptions()).thenReturn(options);
		VoteSite site = mock(VoteSite.class);
		when(voteSiteManager.getVoteSiteName(true, "known.example")).thenReturn("known");
		when(voteSiteManager.getVoteSite("known", true)).thenReturn(site);
		when(site.isEnabled()).thenReturn(true);
		when(site.isWaitUntilVoteDelay()).thenReturn(true);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player")).thenReturn(user);
		when(user.canVoteSite(site)).thenReturn(!stillDelayed);
		return site;
	}

}
