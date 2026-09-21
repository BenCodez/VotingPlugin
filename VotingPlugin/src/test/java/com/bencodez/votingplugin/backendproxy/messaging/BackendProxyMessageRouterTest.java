package com.bencodez.votingplugin.backendproxy.messaging;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
		when(cache.reserve(voteId)).thenReturn(true, false);
		when(plugin.getBungeeSettings()).thenReturn(mock(BungeeSettings.class));
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, "Player"))
				.thenReturn(user);
		doThrow(new IllegalStateException("partial reward")).when(user).bungeeVotePluginMessaging(
				any(), anyLong(), any(), anyBoolean(), anyBoolean(), anyBoolean(), anyInt());
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
		when(cache.reserve(voteId)).thenReturn(true, false);
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
						true, true, "", voteId, false, false, 1, 1));

		voteRouter.handleOrderedVote(vote, outcome::set);
		assertEquals(OrderedVoteOutcome.RETRY, outcome.get());

		voteRouter.handleOrderedVote(vote, outcome::set);
		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(cache, times(2)).complete(voteId);
		verify(user, times(1)).bungeeVotePluginMessaging(any(), anyLong(), any(), anyBoolean(), anyBoolean(),
				anyBoolean(), anyInt());
	}

	@Test
	void malformedReliableVoteIsQuarantinedWithoutCompletionReceipt() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.reserve(voteId)).thenReturn(true);
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
				anyBoolean(), anyInt());
	}

	@Test
	void receiptReleaseRunsInOrderedLaneAndAcknowledgesDurableRemoval() {
		UUID voteId = UUID.randomUUID();
		ProcessedVoteCache cache = mock(ProcessedVoteCache.class);
		when(cache.hasDurableReceipt(voteId)).thenReturn(true);
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
		assertTrue(voteRouter.hasDurableReceiptForRelease(release));
		voteRouter.handleOrderedVote(release, outcome::set);

		assertEquals(OrderedVoteOutcome.COMPLETE, outcome.get());
		verify(cache).releaseCompletedReceipt(voteId);
		org.mockito.ArgumentCaptor<JsonEnvelope> acknowledgement = org.mockito.ArgumentCaptor
				.forClass(JsonEnvelope.class);
		verify(messages).sendMessage(acknowledgement.capture());
		assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK,
				acknowledgement.getValue().getSubChannel());
	}

}
