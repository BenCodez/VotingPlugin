package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import org.bukkit.Server;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class MultiProxyCooldownCompatibilityTest {
    @TempDir
    Path temporaryDirectory;

    @TestFactory
    Stream<DynamicTest> reliableForwardingKeepsNormalCooldownTimeLocalToReceiver() {
        return Stream.of(100L, System.currentTimeMillis() + 86_400_000L)
                .map(senderTime -> DynamicTest.dynamicTest("sender time " + senderTime,
                        () -> assertReceiverCooldownTime(senderTime)));
    }

    private void assertReceiverCooldownTime(long senderTime) throws Exception {
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        VotingPluginProxyTestImpl proxy = spy(new VotingPluginProxyTestImpl());
        proxy.setDataFolder(temporaryDirectory.toFile());
        VoteCacheHandler voteCache = mock(VoteCacheHandler.class);
        when(voteCache.markMultiProxyVoteCompletedDurably(any())).thenReturn(true);
        when(voteCache.getTimeChangeQueue()).thenReturn(new ConcurrentLinkedQueue<>());
        doReturn(voteCache).when(proxy).getVoteCacheHandler();
        doNothing().when(proxy).addVoteParty();
        proxy.setMethod(BungeeMethod.PLUGINMESSAGING);
        proxy.setGlobalMessageProxyHandlerForTest(new GlobalMessageProxyHandler() {
            @Override public void sendMessage(String server, int delay, JsonEnvelope envelope) { }
        });

        // Install the production callback while transport startup is disabled. Only
        // the transport acknowledgement is stubbed; decoding and vote production run.
        proxy.loadMultiProxySupport();
        MultiProxyHandler handler = spy(proxy.getMultiProxyHandler());
        doNothing().when(handler).acknowledgeMultiProxyVote(any(), anyString());
        proxy.setMultiProxyHandler(handler);
        when(proxy.getConfig().getMultiProxySupport()).thenReturn(true);
        when(proxy.getConfig().getPrimaryServer()).thenReturn(false);
        when(proxy.getConfig().getOnlineMode()).thenReturn(true);
        when(proxy.getConfig().getBungeeManageTotals()).thenReturn(true);
        when(proxy.getConfig().getSendVotesToAllServers()).thenReturn(false);
        Field legacyServers = VotingPluginProxy.class.getDeclaredField("legacyVoteDeliveryServers");
        legacyServers.setAccessible(true);
        @SuppressWarnings("unchecked")
        java.util.Set<String> legacy = (java.util.Set<String>) legacyServers.get(proxy);
        legacy.add("server1");
        legacy.add("server2");

        Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
        handleEnvelope.setAccessible(true);
        long beforeReceive = System.currentTimeMillis();
        handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVote("Player", playerId.toString(),
                "Service", senderTime, true, true, "", voteId, false, false, 1, 1, "Primary", true));
        long afterReceive = System.currentTimeMillis();

        JsonEnvelope emitted = proxy.getLastVoteEnvelope();
        assertNotNull(emitted, "the actual receiver must produce a backend vote");
        VotingPluginWire.Vote forwarded = VotingPluginWire.readVote(emitted);
        assertTrue(forwarded.time >= beforeReceive && forwarded.time <= afterReceive,
                "ordinary backend LastVotes time must use the receiver clock");
        assertEquals(voteId, forwarded.voteId);
        assertEquals(playerId.toString(), forwarded.uuid);
        assertTrue(forwarded.delayValidationKnown);
        assertTrue(forwarded.delayValidated);
        assertTrue(forwarded.queuedDeliveryKnown);

        // Exercise the real backend router and event adapter. No guide manager is
        // installed: provenance must not change ordinary vote/cooldown accounting.
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        UserManager users = mock(UserManager.class);
        VotingPluginUser user = mock(VotingPluginUser.class);
        Field userPlugin = VotingPluginUser.class.getDeclaredField("plugin");
        userPlugin.setAccessible(true);
        userPlugin.set(user, plugin);
        when(user.getPlayerName()).thenReturn("Player");
        doCallRealMethod().when(user).bungeeVotePluginMessaging(anyString(), anyLong(),
                any(VoteTotalsSnapshot.class), anyBoolean(), anyBoolean(), anyBoolean(), anyInt(),
                anyBoolean(), anyBoolean(), anyBoolean(), any(UUID.class), anyBoolean(), anyBoolean());
        BungeeSettings settings = mock(BungeeSettings.class);
        when(settings.isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings()).thenReturn(settings);
        when(plugin.getVotingPluginUserManager()).thenReturn(users);
        when(users.getVotingPluginUser(playerId, "Player")).thenReturn(user);
        ServerData serverData = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(serverData);
        VoteSiteManager sites = mock(VoteSiteManager.class);
        VoteSite site = mock(VoteSite.class);
        when(plugin.getVoteSiteManager()).thenReturn(sites);
        when(sites.getVoteSite("Service", true)).thenReturn(site);
        Server server = mock(Server.class);
        PluginManager events = mock(PluginManager.class);
        when(plugin.getServer()).thenReturn(server);
        when(server.getPluginManager()).thenReturn(events);
        ProcessedVoteCache processed = mock(ProcessedVoteCache.class);
        when(processed.reserveWithOutcome(voteId)).thenReturn(ProcessedVoteCache.Reservation.RESERVED);
        when(processed.complete(voteId)).thenReturn(true);
        BackendProxyMessageRouter router = new BackendProxyMessageRouter(plugin,
                mock(BackendPresenceManager.class), mock(BackendGlobalDataSync.class),
                mock(BackendVotePartySync.class), processed);
        // Check completion afterwards so a fixture error retains its original
        // exception instead of being replaced by the quarantine callback assertion.
        AtomicReference<BackendProxyMessageRouter.OrderedVoteOutcome> outcome = new AtomicReference<>();
        router.handleOrderedVote(emitted, outcome::set);
        assertEquals(BackendProxyMessageRouter.OrderedVoteOutcome.COMPLETE, outcome.get());
        ArgumentCaptor<PlayerVoteEvent> event = ArgumentCaptor.forClass(PlayerVoteEvent.class);
        verify(events).callEvent(event.capture());
        assertEquals(forwarded.time, event.getValue().getTime());
        assertEquals(voteId, event.getValue().getProxyVoteId());
        assertEquals(!forwarded.manageTotals, event.getValue().isAddTotals());
        assertTrue(event.getValue().isUnconfirmedProxySessionDelivery());
        assertTrue(event.getValue().isProxyDelayValidationKnown());
        assertTrue(event.getValue().isProxyQueueClassificationKnown());
        assertEquals(forwarded.queuedDelivery, event.getValue().isQueuedProxyVote());

        assertNormalAccountingTime(event.getValue(), site, user);
    }

    private void assertNormalAccountingTime(PlayerVoteEvent event, VoteSite site, VotingPluginUser user) {
        @SuppressWarnings("unchecked")
        SharedVoteProcessor.Operations<VoteSite, VotingPluginUser> operations = mock(SharedVoteProcessor.Operations.class);
        when(operations.enabled()).thenReturn(true);
        when(operations.incomingName()).thenReturn("Player");
        when(operations.properName("Player")).thenReturn("Player");
        when(operations.validate("Player", false)).thenReturn(
                new SharedVoteProcessor.Validation(true, "Player", "CACHE", "ok", false));
        when(operations.resolveSite()).thenReturn(site);
        when(operations.siteEnabled(site)).thenReturn(true);
        when(operations.resolveUser("Player")).thenReturn(user);
        when(operations.proxyVote()).thenReturn(event.isBungee());
        when(operations.forceProxyRouting()).thenReturn(event.isForceBungee());
        when(operations.wasOnline()).thenReturn(event.isWasOnline());
        when(operations.realVote()).thenReturn(event.isRealVote());
        when(operations.addTotals()).thenReturn(event.isAddTotals());
        when(operations.proxyVoteId()).thenReturn(event.getProxyVoteId());
        when(operations.proxyQueueClassificationKnown()).thenReturn(event.isProxyQueueClassificationKnown());
        when(operations.proxyDelayValidationKnown()).thenReturn(event.isProxyDelayValidationKnown());
        when(operations.incomingTime()).thenReturn(event.getTime());
        when(operations.countingPolicy()).thenReturn(new SharedVotePolicy(false, true, false, false, false));
        SharedVoteProcessor.process(operations);
        verify(operations).setTime(user, site, event.getTime());
        verify(operations).cooldown(user, site);
    }
}
