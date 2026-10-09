package com.bencodez.votingplugin.tests.listeners;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
    public void cancelledCapturesAreBoundedAndTransferredOnceBeforeShutdown() {
        var tasks = new java.util.ArrayList<Runnable>();
        doAnswer(call -> { tasks.add(call.getArgument(0)); return CompletableFuture.completedFuture(null); })
                .when(voteTimer).submit(any(Runnable.class));
        doAnswer(call -> { ((java.util.function.Consumer<Boolean>) call.getArgument(5)).accept(true); return null; })
                .when(overflowQueue).enqueueAfterInitialization(anyString(), anyString(), org.mockito.ArgumentMatchers.anyLong(), any(java.util.UUID.class), any(java.util.function.BooleanSupplier.class), any());
        for (int index = 0; index < 257; index++) listener.onVotiferEvent(createVoteEvent(SERVICE_SITE));
        org.junit.jupiter.api.Assertions.assertEquals(256, listener.getPendingCaptureCount());
        org.junit.jupiter.api.Assertions.assertEquals(256, tasks.size());
        listener.stop(); listener.stop();
        org.junit.jupiter.api.Assertions.assertEquals(0, listener.getPendingCaptureCount());
        // Cancelled callbacks that later run cannot transfer or process again.
        tasks.forEach(Runnable::run);
        verify(overflowQueue, org.mockito.Mockito.times(256)).enqueueAfterInitialization(anyString(), anyString(),
                org.mockito.ArgumentMatchers.anyLong(), any(java.util.UUID.class), any(java.util.function.BooleanSupplier.class), any());
        verify(pluginManager, never()).callEvent(any(PlayerVoteEvent.class));
    }

	@Test
	public void testVoteIsQueuedWhenBoundedExecutorRejectsIt() {
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));
        doAnswer(call -> { ((java.util.function.Consumer<Boolean>) call.getArgument(5)).accept(true); return null; })
                .when(overflowQueue).enqueueAfterInitialization(org.mockito.ArgumentMatchers.eq("Steve"),
                        org.mockito.ArgumentMatchers.eq(SERVICE_SITE), org.mockito.ArgumentMatchers.anyLong(), any(java.util.UUID.class), any(java.util.function.BooleanSupplier.class), any());

		listener.onVotiferEvent(createVoteEvent(SERVICE_SITE));

        verify(overflowQueue).enqueueAfterInitialization(org.mockito.ArgumentMatchers.eq("Steve"),
                org.mockito.ArgumentMatchers.eq(SERVICE_SITE), org.mockito.ArgumentMatchers.anyLong(), any(java.util.UUID.class), any(java.util.function.BooleanSupplier.class), any());
		verify(pluginManager, never()).callEvent(any(PlayerVoteEvent.class));
	}
}
