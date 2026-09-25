package com.bencodez.votingplugin.tests.listeners;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Logger;

import org.bukkit.Server;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.listeners.VotiferEvent;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.votesites.VoteSiteManager;
import com.vexsoftware.votifier.model.Vote;

/**
 * Tests Votifier ingestion when a received service belongs to a disabled
 * configured vote site.
 */
public class VotiferEventDisabledVoteSiteTest {

	private static final String SERVICE_SITE = "disabled.example.com";

	private VotingPluginMain plugin;

	private ConfigVoteSites configVoteSites;

	private VoteSiteManager voteSiteManager;

	private ScheduledExecutorService voteTimer;

	private PluginManager pluginManager;

	private VotiferEvent listener;

	private VotifierVoteOverflowQueue overflowQueue;

	@BeforeEach
	public void setUp() {
		plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		configVoteSites = mock(ConfigVoteSites.class);
		voteSiteManager = mock(VoteSiteManager.class);
		voteTimer = mock(ScheduledExecutorService.class);
		overflowQueue = mock(VotifierVoteOverflowQueue.class);

		Server server = mock(Server.class);
		pluginManager = mock(PluginManager.class);

		when(plugin.getLogger()).thenReturn(Logger.getLogger("VotiferEventDisabledVoteSiteTest"));
		when(plugin.getConfigVoteSites()).thenReturn(configVoteSites);
		when(plugin.getVoteSiteManager()).thenReturn(voteSiteManager);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflowQueue);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(pluginManager);

		when(plugin.getOptions().getBedrockPlayerPrefix()).thenReturn(".");
		when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(false);
		when(plugin.getConfigFile().isAdvancedServiceSiteHandling()).thenReturn(false);
		when(plugin.getTimeChecker().isActiveProcessing()).thenReturn(false);

		// Execute submitted vote work immediately so assertions do not need a real
		// executor thread.
		doAnswer(invocation -> {
			Runnable task = invocation.getArgument(0);
			task.run();
			return CompletableFuture.completedFuture(null);
		}).when(voteTimer).submit(any(Runnable.class));

		listener = new VotiferEvent(plugin);
	}

	/**
	 * Creates a mocked NuVotifier event.
	 *
	 * @param serviceSite service site supplied by NuVotifier
	 * @return the event
	 */
	private com.vexsoftware.votifier.model.VotifierEvent createVoteEvent(String serviceSite) {
		Vote vote = mock(Vote.class);
		when(vote.getServiceName()).thenReturn(serviceSite);
		when(vote.getAddress()).thenReturn("127.0.0.1");
		when(vote.getUsername()).thenReturn("Steve");

		com.vexsoftware.votifier.model.VotifierEvent event =
				mock(com.vexsoftware.votifier.model.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);
		return event;
	}

	@Test
	public void testDisabledConfiguredSiteIsNotGeneratedByVotifierPath() {
		when(plugin.getConfigFile().isAutoCreateVoteSites()).thenReturn(true);

		when(voteSiteManager.getVoteSiteName(false, SERVICE_SITE, "")).thenReturn("DisabledSite");
		when(voteSiteManager.hasVoteSite("DisabledSite")).thenReturn(false);
		when(voteSiteManager.hasConfiguredVoteSite("DisabledSite")).thenReturn(true);

		when(voteSiteManager.getVoteSiteName(true, SERVICE_SITE, "")).thenReturn(SERVICE_SITE);
		when(voteSiteManager.getVoteSite(SERVICE_SITE, true)).thenReturn(null);

		listener.onVotiferEvent(createVoteEvent(SERVICE_SITE));

		verify(configVoteSites, never()).tryAutoGenerateVoteSite(anyString());

		// Proves the submitted task continued through vote resolution rather than
		// passing only because processing stopped before the generation decision.
		verify(pluginManager).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	public void testUnknownSiteStillAttemptsGenerationByVotifierPath() {
		when(plugin.getConfigFile().isAutoCreateVoteSites()).thenReturn(true);

		when(voteSiteManager.getVoteSiteName(false, SERVICE_SITE, "")).thenReturn(SERVICE_SITE);
		when(voteSiteManager.hasVoteSite(SERVICE_SITE)).thenReturn(false);
		when(voteSiteManager.hasConfiguredVoteSite(SERVICE_SITE)).thenReturn(false);

		// Return false to avoid depending on reload behavior. This test only needs to
		// prove that a genuinely unknown site still attempts generation.
		when(configVoteSites.tryAutoGenerateVoteSite(SERVICE_SITE)).thenReturn(false);

		when(voteSiteManager.getVoteSiteName(true, SERVICE_SITE, "")).thenReturn(SERVICE_SITE);
		when(voteSiteManager.getVoteSite(SERVICE_SITE, true)).thenReturn(null);

		listener.onVotiferEvent(createVoteEvent(SERVICE_SITE));

		verify(configVoteSites).tryAutoGenerateVoteSite(SERVICE_SITE);
		verify(pluginManager).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	public void testVoteIsQueuedWhenBoundedExecutorRejectsIt() {
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));
		when(overflowQueue.enqueueDurably(org.mockito.ArgumentMatchers.eq("Steve"),
				org.mockito.ArgumentMatchers.eq(SERVICE_SITE), any())).thenReturn(true);

		listener.onVotiferEvent(createVoteEvent(SERVICE_SITE));

		verify(overflowQueue).enqueueDurably(org.mockito.ArgumentMatchers.eq("Steve"),
				org.mockito.ArgumentMatchers.eq(SERVICE_SITE), any());
		verify(pluginManager, never()).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	void timeChangeQueueReceivesTheOriginalVoteId() {
		TimeQueueHandler timeQueue = mock(TimeQueueHandler.class);
		UUID voteId = UUID.randomUUID();
		when(plugin.getTimeQueueHandler()).thenReturn(timeQueue);
		when(plugin.getTimeChecker().isActiveProcessing()).thenReturn(true);
		when(plugin.getConfigFile().isQueueVotesDuringTimeChange()).thenReturn(true);
		when(timeQueue.addVoteDurably(voteId, "Steve", SERVICE_SITE)).thenReturn(true);

		listener.processVote(SERVICE_SITE, "Steve", voteId);

		verify(timeQueue).addVoteDurably(voteId, "Steve", SERVICE_SITE);
		verify(pluginManager, never()).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	void queuedVoteIsRetriedWhenTimeQueueHandoffIsNotDurable() {
		TimeQueueHandler timeQueue = mock(TimeQueueHandler.class);
		UUID voteId = UUID.randomUUID();
		when(plugin.getTimeQueueHandler()).thenReturn(timeQueue);
		when(plugin.getTimeChecker().isActiveProcessing()).thenReturn(true);
		when(plugin.getConfigFile().isQueueVotesDuringTimeChange()).thenReturn(true);

		assertEquals(VotifierVoteOverflowQueue.VoteOutcome.RETRY,
				listener.processQueuedVoteOutcome(SERVICE_SITE, "Steve", voteId));

		verify(timeQueue).addVoteDurably(voteId, "Steve", SERVICE_SITE);
		verify(pluginManager, never()).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	void queuedVoteIsNotAcknowledgedWhenPostAdmissionProcessingFails() {
		when(voteSiteManager.getVoteSiteName(false, SERVICE_SITE, "")).thenReturn(SERVICE_SITE);
		when(voteSiteManager.getVoteSiteName(true, SERVICE_SITE, "")).thenReturn(SERVICE_SITE);
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setProcessingFailed(true);
			event.setReplayUnsafe(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		assertFalse(listener.processQueuedVote(SERVICE_SITE, "Steve", UUID.randomUUID()));
		assertEquals(VotifierVoteOverflowQueue.VoteOutcome.QUARANTINE,
				listener.processQueuedVoteOutcome(SERVICE_SITE, "Steve", UUID.randomUUID()));
	}
}
