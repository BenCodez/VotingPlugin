package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Logger;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.Listener;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.advancedcore.api.user.validation.*;
import com.bencodez.votingplugin.events.*;
import com.bencodez.votingplugin.listeners.*;
import com.bencodez.votingplugin.specialrewards.datemilestones.*;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BoundedScheduledExecutor;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.vexsoftware.votifier.model.Vote;

/** Genuine NuVotifier input traverses actual startup capture, bounded workers and the accepted consumer. */
class DateVotifierStartupCaptureTest {
    @TempDir Path root;
    private static final String SERVICE = "startup.example";
    private static final String[] HANDLERS = { "voteMilestonesManager", "voteStreakHandler", "voteParty", "specialRewards", "topVoterHandler", "voteShopManager", "placeholders" };
    static void ready(VotingPluginMain plugin) throws Exception {
        for (String name : HANDLERS) {
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, mock(field.getType()));
        }
    }
    private final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final BoundedScheduledExecutor timer = new BoundedScheduledExecutor(1, 256);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final VoteSite site = mock(VoteSite.class);
        final YamlConfiguration config = new YamlConfiguration();
        final List<Listener> registered = new CopyOnWriteArrayList<>();
        final List<PlayerVoteEvent> accepted = new CopyOnWriteArrayList<>();
        final List<PlayerPostVoteEvent> posted = new CopyOnWriteArrayList<>();
        final CountDownLatch delivered;
        final DateVoteMilestones dates;
        volatile Runnable afterAccepted = () -> { };
        Fixture(Path directory, int expectedVotes) throws Exception { this(directory, expectedVotes, UUID.randomUUID()); }
        Fixture(Path directory, int expectedVotes, UUID playerId) throws Exception {
            delivered = new CountDownLatch(expectedVotes);
            timer.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
            timer.setThreadFactory(work -> new Thread(work, "date-startup-vote-owner"));
            when(plugin.isEnabled()).thenReturn(true); when(plugin.isVotifierLoaded()).thenReturn(true);
            when(plugin.getDataFolder()).thenReturn(directory.toFile()); when(plugin.getVoteTimer()).thenReturn(timer);
            when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
            doCallRealMethod().when(plugin).getVotifierVoteOverflowQueue();
            doCallRealMethod().when(plugin).registerEarlyVotifierIngress();
            doCallRealMethod().when(plugin).initializeDateVoteIngress(any());
            when(plugin.getOptions().getBedrockPlayerPrefix()).thenReturn(".");
            when(plugin.getOptions().isProcessRewards()).thenReturn(true);
            when(plugin.getConfigFile().isAddTotals()).thenReturn(true);
            when(plugin.getUserManager().getProperName("Steve")).thenReturn("Steve");
            when(plugin.getUserManager().getValidationService().validate("Steve", false)).thenReturn(
                    new UserValidationResult(ValidationStatus.VALID, "Steve", ValidationSource.STORAGE, "fixture", false));
            when(user.getPlayerName()).thenReturn("Steve"); when(user.getJavaUUID()).thenReturn(playerId);
            when(user.getUUID()).thenReturn(playerId.toString()); when(user.isOnline()).thenReturn(true);
            when(site.isEnabled()).thenReturn(true); when(site.getKey()).thenReturn("a"); when(site.getServiceSite()).thenReturn(SERVICE);
            when(plugin.getVoteSiteManager().getVoteSiteName(false, SERVICE, "")).thenReturn("a");
            when(plugin.getVoteSiteManager().hasVoteSite("a")).thenReturn(true);
            when(plugin.getVoteSiteManager().getVoteSiteName(true, SERVICE, "")).thenReturn("a");
            when(plugin.getVoteSiteManager().getVoteSite("a", true)).thenReturn(site);
            String prefix = "DateVoteMilestones.startup.";
            config.set(prefix + "Enabled", true); config.set(prefix + "Start", "2020-01-01T00:00:00");
            config.set(prefix + "End", "2099-01-01T00:00:00"); config.set(prefix + "Timezone", "UTC");
            config.set(prefix + "Milestones.1.Rewards.Messages.Player", "Thanks");
            when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
            dates = new DateVoteMilestones(plugin); when(plugin.getDateVoteMilestones()).thenReturn(dates);
            var manager = plugin.getServer().getPluginManager();
            doAnswer(call -> { registered.add(call.getArgument(0)); return null; }).when(manager).registerEvents(any(), eq(plugin));
            doAnswer(call -> {
                Object event = call.getArgument(0);
                if (event instanceof PlayerVoteEvent vote) {
                    assertEquals("date-startup-vote-owner", Thread.currentThread().getName());
                    vote.setVotingPluginUser(user); accepted.add(vote);
                    var consumer = registered.stream().filter(PlayerVoteListener.class::isInstance).map(PlayerVoteListener.class::cast).findFirst().orElseThrow();
                    consumer.onplayerVote(vote);
                    afterAccepted.run();
                } else if (event instanceof PlayerPostVoteEvent post) { posted.add(post); delivered.countDown(); }
                return null;
            }).when(manager).callEvent(any());
        }
        VotiferEvent capture() { return registered.stream().filter(VotiferEvent.class::isInstance).map(VotiferEvent.class::cast).findFirst().orElseThrow(); }
        void vote() { capture().onVotiferEvent(new com.vexsoftware.votifier.model.VotifierEvent(new Vote(SERVICE, "Steve", "127.0.0.1", "1"))); }
        void drain() throws Exception {
            timer.submit(() -> { }).get(3, TimeUnit.SECONDS);
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
            while ((!plugin.getVotifierVoteOverflowQueue().isInitializationComplete() || capture().getPendingCaptureCount() != 0)
                    && System.nanoTime() < deadline) Thread.sleep(5);
            assertTrue(plugin.getVotifierVoteOverflowQueue().isInitializationComplete(), "queue initialization did not finish");
            assertEquals(0, capture().getPendingCaptureCount(), "capture transfers did not finish");
            timer.submit(() -> { }).get(3, TimeUnit.SECONDS);
        }
        void open() throws Exception { ready(plugin); plugin.initializeDateVoteIngress(() -> fail("Standalone does not open proxy ingress")); }
        void received() throws Exception { assertTrue(delivered.await(3, TimeUnit.SECONDS)); drain(); }
        DateVoteLedger.Progress progress() throws Exception {
            var event = DateVoteMilestones.parse("startup", config.getConfigurationSection("DateVoteMilestones.startup"));
            return new DateVoteLedger(plugin.getDataFolder().toPath().resolve("date-vote-milestones")).progress(event, user.getJavaUUID());
        }
        public void close() {
            capture().stop(); timer.shutdownNow();
            try { assertTrue(timer.awaitTermination(3, TimeUnit.SECONDS)); } catch (InterruptedException failure) { throw new AssertionError(failure); }
            var queue = plugin.getVotifierVoteOverflowQueue(); if (queue != null) queue.close();
        }
    }

    @Test void actualExternalEventsAreCapturedBeforeConsumersAndDeliveredOnceAfterReadiness() throws Exception {
        try (var fixture = new Fixture(root, 2)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.plugin.registerEarlyVotifierIngress(); fixture.drain();
            assertEquals(1, fixture.registered.size());
            long before = System.currentTimeMillis(); fixture.vote(); fixture.vote(); long after = System.currentTimeMillis();
            fixture.drain();
            assertEquals(2, fixture.plugin.getVotifierVoteOverflowQueue().size()); assertTrue(fixture.accepted.isEmpty());
            verify(fixture.plugin.getServerData(), never()).addServiceSite(anyString());
            verify(fixture.user, never()).cache(); verify(fixture.user, never()).playerVote(any(), anyBoolean(), anyBoolean());
            verify(fixture.plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
            long openedAt = System.currentTimeMillis(); fixture.open(); fixture.plugin.initializeDateVoteIngress(() -> fail("Repeated startup cannot reopen transports"));
            fixture.received(); fixture.vote(); fixture.drain();
            assertEquals(3, fixture.registered.size()); assertEquals(3, fixture.accepted.size()); assertEquals(3, fixture.posted.size());
            assertEquals(0, fixture.plugin.getVotifierVoteOverflowQueue().size());
            for (int index = 0; index < 2; index++) {
                long occurrence = fixture.accepted.get(index).getCanonicalOccurrenceTime();
                assertTrue(occurrence >= before && occurrence <= after);
                assertEquals(0L, fixture.accepted.get(index).getTime());
                assertEquals(occurrence, fixture.posted.get(index).getCanonicalOccurrenceTime());
                assertTrue(fixture.posted.get(index).getVoteTime() >= openedAt);
            }
            assertEquals(3, fixture.accepted.stream().map(PlayerVoteEvent::getLocalOccurrenceId).distinct().count());
            assertTrue(fixture.accepted.stream().allMatch(vote -> vote.getLocalOccurrenceId() != null && vote.getProxyVoteId() == null));
            for (int index = 0; index < 3; index++) assertEquals(fixture.accepted.get(index).getLocalOccurrenceId(), fixture.posted.get(index).getVoteUUID());
            assertEquals(3, fixture.progress().votes());
            verify(fixture.user, times(3)).setTime(fixture.site);
            verify(fixture.user, never()).setTime(eq(fixture.site), anyLong());
            verify(fixture.user, times(3)).addTotal(); verify(fixture.user, times(3)).playerVote(fixture.site, true, false);
            verify(fixture.plugin.getRewardHandler(), times(1)).giveReward(eq(fixture.user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        }
    }

    @Test void failedReadinessRetainsCaptureAndUsesCurrentDefinitionsWhenStartupRetries() throws Exception {
        try (var fixture = new Fixture(root, 1)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.vote(); fixture.drain();
            assertThrows(IllegalStateException.class, () -> fixture.plugin.initializeDateVoteIngress(() -> fail("Not ready")));
            assertEquals(1, fixture.registered.size()); assertEquals(1, fixture.plugin.getVotifierVoteOverflowQueue().size());
            // Reload/edit before release: a stale enabled snapshot must not award a buffered vote.
            fixture.config.set("DateVoteMilestones.startup.Enabled", false);
            fixture.plugin.registerEarlyVotifierIngress(); fixture.open(); fixture.received();
            assertEquals(1, fixture.accepted.size()); assertEquals(0, fixture.progress().votes());
            verify(fixture.user).addTotal(); verify(fixture.user).playerVote(fixture.site, true, false);
            verify(fixture.plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        }
    }

    @Test void preReadyCaptureSurvivesRestartWithItsSeparateOccurrence() throws Exception {
        long original;
        try (var fixture = new Fixture(root, 0)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.vote(); fixture.drain();
            assertEquals(1, fixture.plugin.getVotifierVoteOverflowQueue().size()); assertTrue(fixture.accepted.isEmpty());
            // Closing the real overflow writes the captured receipt; no consumer was opened.
            fixture.plugin.getVotifierVoteOverflowQueue().close();
            var yaml = YamlConfiguration.loadConfiguration(root.resolve("VotifierVoteQueue.yml").toFile());
            original = ((Number) yaml.getMapList("Votes").getFirst().get("Time")).longValue();
        }
        try (var restarted = new Fixture(root, 1)) {
            restarted.plugin.registerEarlyVotifierIngress(); restarted.drain();
            assertEquals(1, restarted.plugin.getVotifierVoteOverflowQueue().size()); assertTrue(restarted.accepted.isEmpty());
            restarted.open(); restarted.received();
            assertEquals(1, restarted.accepted.size()); assertEquals(original, restarted.accepted.getFirst().getCanonicalOccurrenceTime());
            assertEquals(0L, restarted.accepted.getFirst().getTime()); assertEquals(original, restarted.posted.getFirst().getCanonicalOccurrenceTime());
            assertEquals(1, restarted.progress().votes()); verify(restarted.user).addTotal();
        }
    }

    @Test void crashAfterLedgerBeforeOverflowAckReplaysTheSameOccurrenceWithoutAnotherAward() throws Exception {
        UUID playerId = UUID.randomUUID();
        UUID original;
        long originalTime;
        var committed = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var fixture = new Fixture(root, 1, playerId)) {
            // Recounting a replay would unlock a distinct second reward.
            fixture.config.set("DateVoteMilestones.startup.Milestones.2.Rewards.Messages.Player", "Second");
            fixture.afterAccepted = () -> {
                committed.countDown();
                try { assertTrue(release.await(3, TimeUnit.SECONDS)); }
                catch (InterruptedException failure) { throw new AssertionError(failure); }
            };
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.vote(); fixture.drain();
            fixture.open();
            assertTrue(committed.await(3, TimeUnit.SECONDS));
            try {
                assertEquals(1, fixture.progress().votes());
                original = fixture.accepted.getFirst().getLocalOccurrenceId();
                originalTime = fixture.accepted.getFirst().getCanonicalOccurrenceTime();
                // The native callback has not returned, so existing shutdown retains
                // precisely the same durable receipt a process crash would leave.
                fixture.plugin.getVotifierVoteOverflowQueue().close();
                var disk = YamlConfiguration.loadConfiguration(root.resolve("VotifierVoteQueue.yml").toFile());
                assertEquals(original.toString(), disk.getMapList("Votes").getFirst().get("LocalOccurrenceId"));
                verify(fixture.plugin.getRewardHandler(), times(1)).giveReward(eq(fixture.user), any(), startsWith("DateVoteMilestonesRuntime."), any());
            } finally { release.countDown(); }
            fixture.drain();
        }
        try (var restarted = new Fixture(root, 1, playerId)) {
            restarted.config.set("DateVoteMilestones.startup.Milestones.2.Rewards.Messages.Player", "Second");
            restarted.plugin.registerEarlyVotifierIngress(); restarted.drain(); restarted.open(); restarted.received();
            assertEquals(original, restarted.accepted.getFirst().getLocalOccurrenceId());
            assertEquals(original, restarted.posted.getFirst().getVoteUUID());
            assertNull(restarted.accepted.getFirst().getProxyVoteId());
            assertFalse(restarted.accepted.getFirst().isBungee());
            assertEquals(originalTime, restarted.accepted.getFirst().getCanonicalOccurrenceTime());
            assertEquals(0L, restarted.accepted.getFirst().getTime());
            assertEquals(1, restarted.progress().votes());
            verify(restarted.plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
            // Ordinary effects retain their existing at-least-once replay semantics.
            verify(restarted.user).setTime(restarted.site);
            verify(restarted.user).addTotal(); verify(restarted.user).playerVote(restarted.site, true, false);
        }
    }

    @Test void blockedExecutorCannotLoseEarlyCaptureOrQueueSetupOnCancellation() throws Exception {
        capturedAdmissionSurvivesCancellation(false);
    }

    @Test void blockedExecutorCannotLoseReadyCaptureOnCancellation() throws Exception {
        capturedAdmissionSurvivesCancellation(true);
    }

    private void capturedAdmissionSurvivesCancellation(boolean readyFirst) throws Exception {
        UUID playerId = UUID.randomUUID();
        UUID original;
        long originalTime;
        try (var fixture = new Fixture(root, 0, playerId)) {
            if (readyFirst) { fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.open(); }
            var entered = new CountDownLatch(1);
            var release = new CountDownLatch(1);
            fixture.timer.submit(() -> {
                entered.countDown();
                boolean interrupted = false;
                try {
                    while (release.getCount() != 0) {
                        try { assertTrue(release.await(3, TimeUnit.SECONDS)); }
                        catch (InterruptedException expected) { interrupted = true; }
                    }
                } finally { if (interrupted) Thread.currentThread().interrupt(); }
            });
            assertTrue(entered.await(3, TimeUnit.SECONDS));
            // In the early case, even registration/setup happens behind the held
            // vote worker. Queue construction and disk load must be independent.
            if (!readyFirst) fixture.plugin.registerEarlyVotifierIngress();
            assertNotNull(fixture.plugin.getVotifierVoteOverflowQueue());
            long before = System.currentTimeMillis(); fixture.vote(); long after = System.currentTimeMillis();
            assertEquals(1, fixture.capture().getPendingCaptureCount());
            assertTrue(fixture.accepted.isEmpty());
            var registryField = VotiferEvent.class.getDeclaredField("captured"); registryField.setAccessible(true);
            var registry = (Map<?, ?>) registryField.get(fixture.capture());
            original = (UUID) registry.keySet().iterator().next();
            var captureTime = registry.values().iterator().next().getClass().getDeclaredField("occurredAt"); captureTime.setAccessible(true);
            originalTime = captureTime.getLong(registry.values().iterator().next());
            try {
                fixture.capture().stop();
                assertFalse(fixture.timer.shutdownNow().isEmpty(), "the capture callback never executed");
            } finally { release.countDown(); }
            assertTrue(fixture.timer.awaitTermination(3, TimeUnit.SECONDS));
            fixture.plugin.getVotifierVoteOverflowQueue().close();
            assertEquals(0, fixture.capture().getPendingCaptureCount());
            var disk = YamlConfiguration.loadConfiguration(root.resolve("VotifierVoteQueue.yml").toFile()).getMapList("Votes");
            assertEquals(1, disk.size());
            assertEquals(original, UUID.fromString((String) disk.getFirst().get("LocalOccurrenceId")));
            assertEquals(originalTime, ((Number) disk.getFirst().get("Time")).longValue());
            assertTrue(originalTime >= before && originalTime <= after);
            verify(fixture.user, never()).addTotal();
            verify(fixture.plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        }
        try (var restarted = new Fixture(root, 1, playerId)) {
            restarted.plugin.registerEarlyVotifierIngress(); restarted.drain(); restarted.open(); restarted.received();
            assertEquals(original, restarted.accepted.getFirst().getLocalOccurrenceId());
            assertEquals(original, restarted.posted.getFirst().getVoteUUID());
            assertEquals(originalTime, restarted.accepted.getFirst().getCanonicalOccurrenceTime());
            assertEquals(0L, restarted.accepted.getFirst().getTime());
            assertEquals(1, restarted.progress().votes());
            verify(restarted.user).addTotal(); verify(restarted.user).setTime(restarted.site);
            verify(restarted.plugin.getRewardHandler(), times(1)).giveReward(eq(restarted.user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        }
        try (var secondRestart = new Fixture(root, 0, playerId)) {
            secondRestart.plugin.registerEarlyVotifierIngress(); secondRestart.drain(); secondRestart.open(); secondRestart.drain();
            assertTrue(secondRestart.accepted.isEmpty()); assertEquals(1, secondRestart.progress().votes());
        }
    }

    @Test void retiredCaptureCannotReopenAnOldStartupGeneration() throws Exception {
        try (var fixture = new Fixture(root, 0)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.vote(); fixture.drain();
            fixture.capture().stop();
            var field = VotingPluginMain.class.getDeclaredField("localVotifierIngressClosed"); field.setAccessible(true); field.setBoolean(fixture.plugin, true);
            fixture.vote(); fixture.drain();
            assertEquals(1, fixture.plugin.getVotifierVoteOverflowQueue().size());
            assertThrows(IllegalStateException.class, () -> fixture.plugin.initializeDateVoteIngress(() -> fail("Retired")));
            assertTrue(fixture.accepted.isEmpty()); assertEquals(1, fixture.registered.size());
        }
    }

    @Test void startedNativeCallbackBlockedBeforeConsumerSurvivesShutdownAndRewardsOnceAfterRestart() throws Exception {
        UUID playerId = UUID.randomUUID(), original; long originalTime;
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        try (var fixture = new Fixture(root, 0, playerId)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.open();
            var serverData = fixture.plugin.getServerData();
            doAnswer(call -> {
                entered.countDown();
                try { if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test storage timeout"); }
                catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt(); throw new IllegalStateException("native receipt storage interrupted before consumer", interrupted);
                }
                return null;
            }).when(serverData).addServiceSite(SERVICE);
            fixture.vote(); assertTrue(entered.await(3, TimeUnit.SECONDS));
            assertEquals(1, fixture.capture().getPendingCaptureCount()); assertTrue(fixture.accepted.isEmpty());
            var registryField = VotiferEvent.class.getDeclaredField("captured"); registryField.setAccessible(true);
            var registry = (Map<?, ?>) registryField.get(fixture.capture()); original = (UUID) registry.keySet().iterator().next();
            var captureTime = registry.values().iterator().next().getClass().getDeclaredField("occurredAt"); captureTime.setAccessible(true);
            originalTime = captureTime.getLong(registry.values().iterator().next());
            // Use the native teardown fence, then the real listener/worker stop order.
            var closed = VotingPluginMain.class.getDeclaredField("localVotifierIngressClosed"); closed.setAccessible(true); closed.setBoolean(fixture.plugin, true);
            var readiness = VotingPluginMain.class.getDeclaredField("localVotifierIngressReady"); readiness.setAccessible(true); readiness.setBoolean(fixture.plugin, false);
            fixture.capture().stop(); fixture.timer.shutdownNow(); assertTrue(fixture.timer.awaitTermination(3, TimeUnit.SECONDS));
            fixture.plugin.getVotifierVoteOverflowQueue().close(); assertEquals(0, fixture.capture().getPendingCaptureCount());
            var disk = YamlConfiguration.loadConfiguration(root.resolve("VotifierVoteQueue.yml").toFile()).getMapList("Votes");
            assertEquals(1, disk.size()); assertEquals(original.toString(), disk.getFirst().get("LocalOccurrenceId"));
            assertEquals(originalTime, ((Number) disk.getFirst().get("Time")).longValue());
            verify(fixture.user, never()).cache(); verify(fixture.user, never()).addTotal();
            verify(fixture.user, never()).playerVote(any(), anyBoolean(), anyBoolean());
            verify(fixture.plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        } finally { release.countDown(); }
        try (var restarted = new Fixture(root, 1, playerId)) {
            restarted.plugin.registerEarlyVotifierIngress(); restarted.drain(); restarted.open(); restarted.received();
            assertEquals(original, restarted.accepted.getFirst().getLocalOccurrenceId());
            assertEquals(originalTime, restarted.accepted.getFirst().getCanonicalOccurrenceTime()); assertEquals(1, restarted.progress().votes());
            verify(restarted.user).addTotal(); verify(restarted.user).playerVote(restarted.site, true, false);
            verify(restarted.plugin.getRewardHandler(), times(1)).giveReward(eq(restarted.user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        }
        try (var again = new Fixture(root, 0, playerId)) {
            again.plugin.registerEarlyVotifierIngress(); again.drain(); again.open(); again.drain();
            assertTrue(again.accepted.isEmpty()); assertEquals(1, again.progress().votes()); verify(again.user, never()).addTotal();
        }
    }

    @Test void admittedOverflowCallbackCannotRewardAfterStartupGenerationRetires() throws Exception {
        try (var fixture = new Fixture(root, 0)) {
            fixture.plugin.registerEarlyVotifierIngress(); fixture.drain(); fixture.vote(); fixture.drain();
            var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
            fixture.timer.submit(() -> {
                entered.countDown();
                try { assertTrue(release.await(3, TimeUnit.SECONDS)); } catch (InterruptedException failure) { throw new AssertionError(failure); }
            });
            assertTrue(entered.await(3, TimeUnit.SECONDS));
            try {
                fixture.open();
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(3);
                // The constructor's delayed replay occupies one slot; overflow is another.
                while (fixture.timer.getQueue().size() < 2 && System.nanoTime() < deadline) Thread.sleep(5);
                assertTrue(fixture.timer.getQueue().size() >= 2);
                fixture.capture().stop();
                var field = VotingPluginMain.class.getDeclaredField("localVotifierIngressClosed");
                field.setAccessible(true); field.setBoolean(fixture.plugin, true);
            } finally { release.countDown(); }
            fixture.drain();
            assertTrue(fixture.accepted.isEmpty()); assertEquals(1, fixture.plugin.getVotifierVoteOverflowQueue().size());
            verify(fixture.user, never()).addTotal(); verify(fixture.user, never()).playerVote(any(), anyBoolean(), anyBoolean());
        }
    }

}
