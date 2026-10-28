package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import java.nio.file.Path;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.bukkit.configuration.file.YamlConfiguration;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import com.bencodez.votingplugin.user.VotingPluginUser;
class DateVoteIngressOrderingTest {
    @TempDir Path root;
    @Test void firstTransportDeliveryFindsDefinitionsAndRewardHandlesAlreadyPublished() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        String prefix = "DateVoteMilestones.startup.";
        config.set(prefix + "Enabled", true); config.set(prefix + "Timezone", "UTC");
        config.set(prefix + "Start", "2020-01-01T00:00:00"); config.set(prefix + "End", "2099-01-01T00:00:00");
        config.set(prefix + "AccountingServer", "backend");
        config.set(prefix + "Milestones.1.Rewards.Messages.Player", "Thanks");
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings().getServer()).thenReturn("backend");
        DateVoteMilestones events = new DateVoteMilestones(plugin);
        when(plugin.getDateVoteMilestones()).thenReturn(events);
        VotingPluginUser user = mock(VotingPluginUser.class); when(user.getJavaUUID()).thenReturn(UUID.randomUUID());
        Runnable ingress = () -> {
            verify(plugin).addDirectlyDefinedRewards(any());
            events.accepted(user, "a", UUID.randomUUID(), System.currentTimeMillis(), true, true, true, false, false);
        };
        ready(plugin);
        DateVoteIngressAccess.callRealInitialization(plugin);
        DateVoteIngressAccess.initializeDateVoteIngress(plugin, ingress);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        assertTrue(java.nio.file.Files.exists(root.resolve("date-vote-milestones")));
    }
    private static final String[] HANDLERS = {"voteMilestonesManager", "voteStreakHandler", "voteParty", "specialRewards", "topVoterHandler", "voteShopManager", "placeholders"};
    private static void ready(VotingPluginMain plugin) throws Exception {
        for (String name : HANDLERS) {
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, mock(field.getType()));
        }
    }
    @Test void eachMissingDownstreamHandlerKeepsIngressClosed() throws Exception {
        for (String name : HANDLERS) {
            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); ready(plugin);
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, null);
            DateVoteIngressAccess.callRealInitialization(plugin); Runnable ingress = mock(Runnable.class);
            assertThrows(IllegalStateException.class, () -> DateVoteIngressAccess.initializeDateVoteIngress(plugin, ingress)); verify(ingress, never()).run();
            verify(plugin.getDateVoteMilestones(), never()).reload();
            verify(plugin.getServer().getPluginManager(), never()).registerEvents(any(), eq(plugin));
        }
    }

    @Test void earlyCaptureIsRegisteredOnceAndConsumersOpenOnlyAfterDefinitionsAndHandlers() throws Exception {
        for (boolean proxyEnabled : new boolean[] {false, true}) {
            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            when(plugin.isVotifierLoaded()).thenReturn(true);
            when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(proxyEnabled);
            var timer = mock(java.util.concurrent.ScheduledExecutorService.class);
            when(plugin.getVoteTimer()).thenReturn(timer);
            var registered = new java.util.ArrayList<org.bukkit.event.Listener>();
            var manager = plugin.getServer().getPluginManager();
            doAnswer(call -> { registered.add(call.getArgument(0)); return null; }).when(manager).registerEvents(any(), eq(plugin));
            DateVoteIngressAccess.callRealRegistration(plugin);
            DateVoteIngressAccess.callRealInitialization(plugin);
            try (var queues = mockConstruction(com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.class)) {
                DateVoteIngressAccess.registerEarlyVotifierIngress(plugin); DateVoteIngressAccess.registerEarlyVotifierIngress(plugin);
                assertEquals(1, registered.size());
                assertInstanceOf(com.bencodez.votingplugin.listeners.VotiferEvent.class, registered.getFirst());
                verify(plugin.getDateVoteMilestones(), never()).reload();
                assertEquals(1, queues.constructed().size(), "capture publishes its independent queue owner immediately");
                var queueField = VotingPluginMain.class.getDeclaredField("votifierVoteOverflowQueue");
                queueField.setAccessible(true);
                assertSame(queues.constructed().getFirst(), queueField.get(plugin));
                verifyNoInteractions(timer); // Loading cannot be lost with vote-executor cancellation.
                verify(queues.constructed().getFirst(), never()).start();
                ready(plugin);
                Runnable proxy = () -> {
                    verify(plugin.getDateVoteMilestones()).reload();
                    assertEquals(3, registered.size());
                    assertInstanceOf(com.bencodez.votingplugin.listeners.PlayerVoteListener.class, registered.get(1));
                    assertInstanceOf(com.bencodez.votingplugin.timequeue.TimeQueueHandler.class, registered.get(2));
                };
                DateVoteIngressAccess.initializeDateVoteIngress(plugin, proxy); DateVoteIngressAccess.initializeDateVoteIngress(plugin, proxy);
                assertEquals(3, registered.size());
                verify(plugin.getDateVoteMilestones()).reload(); verify(queues.constructed().getFirst()).start();
            }
        }
    }

    @Test void persistedReplayProducerIsConstructedOnlyAfterItsAcceptedVoteConsumer() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); ready(plugin);
        DateVoteIngressAccess.callRealInitialization(plugin);
        var manager = plugin.getServer().getPluginManager();
        var registered = new java.util.ArrayList<org.bukkit.event.Listener>();
        doAnswer(call -> { registered.add(call.getArgument(0)); return null; }).when(manager).registerEvents(any(), eq(plugin));
        try (var producers = mockConstruction(com.bencodez.votingplugin.timequeue.TimeQueueHandler.class, (handler, context) -> {
            // The real constructor schedules replay immediately; this hook runs at that boundary.
            assertEquals(1, registered.size()); assertInstanceOf(com.bencodez.votingplugin.listeners.PlayerVoteListener.class, registered.getFirst());
            verify(plugin.getDateVoteMilestones()).reload();
        })) {
            DateVoteIngressAccess.initializeDateVoteIngress(plugin, () -> fail("Local startup does not open proxy ingress"));
            assertEquals(1, producers.constructed().size()); assertSame(producers.constructed().getFirst(), registered.get(1));
            var field = VotingPluginMain.class.getDeclaredField("timeQueueHandler"); field.setAccessible(true);
            assertSame(producers.constructed().getFirst(), field.get(plugin));
        }
    }

    @Test void failedProxyOpeningRetriesWithoutDuplicatingAcceptedConsumers() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        ready(plugin); when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        DateVoteIngressAccess.callRealInitialization(plugin);
        var registered = new java.util.ArrayList<org.bukkit.event.Listener>();
        var eventManager = plugin.getServer().getPluginManager();
        doAnswer(call -> { registered.add(call.getArgument(0)); return null; })
                .when(eventManager).registerEvents(any(), eq(plugin));
        assertThrows(IllegalStateException.class, () -> DateVoteIngressAccess.initializeDateVoteIngress(plugin, () -> { throw new IllegalStateException("transport unavailable"); }));
        assertEquals(2, registered.size());
        DateVoteIngressAccess.initializeDateVoteIngress(plugin, () -> { }); DateVoteIngressAccess.initializeDateVoteIngress(plugin, () -> fail("Already ready"));
        assertEquals(2, registered.size()); verify(plugin.getDateVoteMilestones(), times(2)).reload();
    }

    @Test void reenableCreatesNewIngressAndFencesOldReadiness() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.isVotifierLoaded()).thenReturn(true);
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings().isGloblalDataEnabled()).thenReturn(true);
        DateVoteIngressAccess.callRealRegistration(plugin);
        DateVoteIngressAccess.callRealInitialization(plugin);
        var listenerField = VotingPluginMain.class.getDeclaredField("localVotifierListener");
        listenerField.setAccessible(true);
        var readyField = com.bencodez.votingplugin.listeners.VotiferEvent.class.getDeclaredField("ready");
        readyField.setAccessible(true);
        var closedField = VotingPluginMain.class.getDeclaredField("localVotifierIngressClosed");
        closedField.setAccessible(true);
        var steps = VotingPluginMain.class.getDeclaredField("completedUnloadSteps"); steps.setAccessible(true);
        var reset = VotingPluginMain.class.getDeclaredMethod("resetDateVoteIngressForEnable"); reset.setAccessible(true);
        try (var queues = mockConstruction(com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.class)) {
            DateVoteIngressAccess.registerEarlyVotifierIngress(plugin);
            Object first = listenerField.get(plugin);
            var oldReady = (java.util.function.BooleanSupplier) readyField.get(first);
            closedField.setBoolean(plugin, true);
            steps.set(plugin, new java.util.HashSet<>(java.util.Set.of("local Votifier ingress", "event handlers")));
            reset.invoke(plugin);
            assertFalse(oldReady.getAsBoolean());
            DateVoteIngressAccess.registerEarlyVotifierIngress(plugin);
            Object second = listenerField.get(plugin);
            assertNotSame(first, second);
            ready(plugin);
            DateVoteIngressAccess.initializeDateVoteIngress(plugin, () -> { });
            assertTrue(((java.util.function.BooleanSupplier) readyField.get(second)).getAsBoolean());
            assertFalse(oldReady.getAsBoolean(), "a retired generation must not become ready again");
            verify(plugin.getServer().getPluginManager(), times(3)).registerEvents(any(), eq(plugin));
        }
    }

    @Test void reenableRejectsIncompletePriorIngressRetirement() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        var listener = VotingPluginMain.class.getDeclaredField("localVotifierListener"); listener.setAccessible(true);
        listener.set(plugin, mock(com.bencodez.votingplugin.listeners.VotiferEvent.class));
        var steps = VotingPluginMain.class.getDeclaredField("completedUnloadSteps"); steps.setAccessible(true);
        steps.set(plugin, new java.util.HashSet<>(java.util.Set.of("event handlers")));
        var reset = VotingPluginMain.class.getDeclaredMethod("resetDateVoteIngressForEnable"); reset.setAccessible(true);
        var failure = assertThrows(java.lang.reflect.InvocationTargetException.class, () -> reset.invoke(plugin));
        assertInstanceOf(IllegalStateException.class, failure.getCause());
        assertNotNull(listener.get(plugin));
    }

}
