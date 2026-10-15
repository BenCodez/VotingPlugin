package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.nio.file.Files;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/** Real ServerData/YAML writes and restart loading; mock only Bukkit services and its file adapter. */
class TimeQueueRecoveryCheckpointTest {
    @TempDir Path directory;

    @Test
    void stoppingDuringPreparationRestoresTheUndispatchedVoteForActualRestartRecovery() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("aborted-preparation.yml"));
        fixture.handler.addVote("First", "site");
        fixture.handler.addVote("Later", "site");
        fixture.handler.save();
        var entered = new java.util.concurrent.CountDownLatch(1);
        var release = new java.util.concurrent.CountDownLatch(1);
        doAnswer(call -> {
            call.callRealMethod();
            entered.countDown();
            assertTrue(release.await(10, java.util.concurrent.TimeUnit.SECONDS));
            return null;
        }).when(fixture.yaml).save(fixture.path.toFile());
        var callers = java.util.concurrent.Executors.newFixedThreadPool(2);
        try {
            var delivery = callers.submit(fixture.handler::processQueue);
            assertTrue(entered.await(5, java.util.concurrent.TimeUnit.SECONDS));
            callers.submit(fixture.handler::saveForShutdown).get(2, java.util.concurrent.TimeUnit.SECONDS);
            release.countDown();
            delivery.get(5, java.util.concurrent.TimeUnit.SECONDS);
            fixture.handler.saveForShutdown();
            verify(fixture.plugin.getServer().getPluginManager(), never()).callEvent(any());
            assertEquals(List.of("First", "Later"), new Fixture(fixture.path).names());
            assertTrue(fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).isEmpty());
            assertEquals("keep", fixture.stored().getString("Adjacent.Value"));
        } finally {
            release.countDown();
            callers.shutdownNow();
            assertTrue(callers.awaitTermination(5, java.util.concurrent.TimeUnit.SECONDS));
            fixture.handler.saveForShutdown();
        }
    }

    @Test
    void failedLateAdmissionRemainsRetryableAfterTheEarlierShutdownSaveSucceeded() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("late-admission.yml"));
        fixture.handler.addVote("First", "site");
        fixture.handler.saveForShutdown();
        Path saved = fixture.blockWrites();
        assertThrows(java.io.UncheckedIOException.class, () -> fixture.handler.addVote("Late", "site"));
        assertEquals(List.of("Late"), fixture.names());
        fixture.repair(saved);
        fixture.handler.saveForShutdown();
        assertTrue(fixture.names().isEmpty());
        assertEquals(List.of("First", "Late"), new Fixture(fixture.path).names());
        assertEquals("keep", fixture.stored().getString("Adjacent.Value"));
    }

    @Test
    void abortedPreparationWriteFailurePreservesBothWrappersUntilRepairAndRetry() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("abort-retry.yml"));
        VoteTimeQueue first = new VoteTimeQueue("First", "site", 123L);
        VoteTimeQueue later = new VoteTimeQueue("Later", "site", 456L);
        UUID id = new UUID(1, 2);
        fixture.data.prepareTimedVoteDispatch(id, first, List.of(later));
        Path saved = fixture.blockWrites();
        assertThrows(java.io.UncheckedIOException.class,
                () -> fixture.data.abortTimedVoteDispatch(id, List.of(first, later)));
        assertEquals("Later", fixture.yaml.getString("VotingPlugin.TimedVoteCache.0.Name"));
        assertEquals("First", fixture.yaml.getString("VotingPlugin.UncertainTimedVotes." + id + ".Name"));
        fixture.repair(saved);
        fixture.data.abortTimedVoteDispatch(id, List.of(first, later));
        assertEquals(List.of("First", "Later"), new Fixture(fixture.path).names());
        assertTrue(fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).isEmpty());
        assertEquals("keep", fixture.stored().getString("Adjacent.Value"));
    }

    @Test
    void cancelledDispatchKeepsSuffixRecoverableWhenNextPreparationFails() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("First", "site");
        fixture.handler.addVote("Later", "site");
        fixture.handler.save();
        assertEquals(2, fixture.handler.getTimeChangeQueue().size());
        AtomicInteger deliveries = new AtomicInteger();
        ServerData data = spy(fixture.data);
        when(fixture.plugin.getServerData()).thenReturn(data);
        AtomicInteger preparations = new AtomicInteger();
        doAnswer(call -> {
            if (preparations.incrementAndGet() == 2) throw new IllegalStateException("synthetic next-checkpoint failure");
            return call.callRealMethod();
        }).when(data).prepareTimedVoteDispatch(any(), any(), any());
        var events = fixture.plugin.getServer().getPluginManager();
        doAnswer(call -> {
            PlayerVoteEvent event = call.getArgument(0);
            YamlConfiguration during = fixture.stored();
            assertEquals("Later", during.getString("VotingPlugin.TimedVoteCache.0.Name"));
            assertEquals(1, during.getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).size());
            deliveries.incrementAndGet();
            fixture.handler.addVote("LateAdmission", "site");
            event.setCancelled(true);
            return null;
        }).when(events).callEvent(any());
        assertThrows(IllegalStateException.class, fixture.handler::processQueue);
        assertEquals(2, preparations.get(), "cancellation must continue to the next pending occurrence");
        assertEquals(1, deliveries.get());
        assertEquals(List.of("Later", "LateAdmission"), fixture.handler.getTimeChangeQueue().stream().map(v -> v.getName()).toList());
        YamlConfiguration after = fixture.stored();
        assertEquals("Later", after.getString("VotingPlugin.TimedVoteCache.0.Name"));
        assertTrue(after.getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).isEmpty());
        Fixture restarted = new Fixture(fixture.path);
        assertEquals(List.of("Later", "LateAdmission"), restarted.handler.getTimeChangeQueue().stream().map(v -> v.getName()).toList());
        assertEquals("keep", after.getString("Adjacent.Value"));
    }

    @Test
    void cancelledDispatchContinuesThroughLateAdmissionsWithRealDurableCheckpoints() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("cancel-and-drain.yml"));
        fixture.handler.addVote("Cancelled", "site");
        fixture.handler.addVote("Accepted", "site");
        fixture.handler.save();
        var delivered = new java.util.ArrayList<String>();
        var events = fixture.plugin.getServer().getPluginManager();
        doAnswer(call -> {
            PlayerVoteEvent event = call.getArgument(0);
            delivered.add(event.getPlayer());
            var uncertain = fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes");
            assertEquals(1, uncertain.getKeys(false).size());
            String id = uncertain.getKeys(false).iterator().next();
            assertEquals(event.getPlayer(), uncertain.getString(id + ".Name"));
            if (event.getPlayer().equals("Cancelled")) {
                fixture.handler.addVote("LateAdmission", "site");
                event.setCancelled(true);
            }
            return null;
        }).when(events).callEvent(any());
        fixture.handler.processQueue();
        assertEquals(List.of("Cancelled", "Accepted", "LateAdmission"), delivered);
        assertTrue(fixture.names().isEmpty());
        assertTrue(new Fixture(fixture.path).names().isEmpty(), "completed entries must not return after restart");
        assertTrue(fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).isEmpty());
        assertEquals("keep", fixture.stored().getString("Adjacent.Value"));
        fixture.handler.processQueue();
        assertEquals(3, delivered.size(), "another drain must not repeat a completed occurrence");
    }

    @Test
    void preparationWriteFailureKeepsTheOriginalLiveAndDiskSuffixWithoutDispatch() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("First", "site");
        fixture.handler.addVote("Later", "site");
        fixture.handler.save();
        Path saved = fixture.blockWrites();
        assertThrows(java.io.UncheckedIOException.class, fixture.handler::processQueue);
        verify(fixture.plugin.getServer().getPluginManager(), never()).callEvent(any());
        assertEquals(List.of("First", "Later"), fixture.names());
        assertEquals("First", fixture.yaml.getString("VotingPlugin.TimedVoteCache.0.Name"));
        UUID originalId = fixture.handler.getTimeChangeQueue().peek().getVoteId();
        fixture.repair(saved);
        var events = fixture.plugin.getServer().getPluginManager();
        var seen = new java.util.ArrayList<UUID>();
        doAnswer(call -> {
            seen.addAll(fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes")
                    .getKeys(false).stream().map(UUID::fromString).toList());
            return null;
        }).when(events).callEvent(any());
        fixture.handler.processQueue();
        assertEquals(originalId, seen.getFirst());
        assertEquals(2, seen.size());
        assertTrue(fixture.names().isEmpty());
        assertTrue(fixture.stored().getConfigurationSection("VotingPlugin.TimedVoteCache").getKeys(false).isEmpty());
    }

    @Test
    void successfullyQuarantinedExceptionAndErrorEachAllowTheUnaffectedSuffixToResume() throws Exception {
        for (Throwable original : List.of(new IllegalStateException("synthetic event failure"),
                new AssertionError("synthetic event failure"))) {
            Fixture fixture = new Fixture(directory.resolve(original.getClass().getSimpleName() + ".yml"));
            fixture.handler.addVote("Ambiguous", "site");
            fixture.handler.addVote("Later", "site");
            AtomicInteger calls = new AtomicInteger();
            var events = fixture.plugin.getServer().getPluginManager();
            doAnswer(call -> {
                if (calls.incrementAndGet() == 1) { fixture.handler.addVote("LateAdmission", "site"); throw original; }
                return null;
            })
                    .when(events).callEvent(any());
            assertSame(original, assertThrows(original.getClass(), fixture.handler::processQueue));
            assertEquals(List.of("Later", "LateAdmission"), fixture.names());
            Fixture restarted = new Fixture(fixture.path);
            assertEquals(List.of("Later", "LateAdmission"), restarted.names(), "ambiguous occurrence must not be replayable");
            fixture.handler.processQueue();
            assertEquals(3, calls.get());
            assertTrue(fixture.names().isEmpty());
            assertEquals(1, fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes")
                    .getKeys(false).size());
        }
    }

    @Test
    void preparationErrorAfterTheFileWasWrittenDoesNotAuthorizeAnEventOrLoseItsIdentity() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("First", "site");
        fixture.handler.addVote("Later", "site");
        fixture.handler.save();
        doAnswer(call -> {
            call.callRealMethod();
            throw new java.io.IOException("synthetic failure after file publication");
        }).when(fixture.yaml).save(fixture.path.toFile());
        assertThrows(java.io.UncheckedIOException.class, fixture.handler::processQueue);
        verify(fixture.plugin.getServer().getPluginManager(), never()).callEvent(any());
        UUID id = fixture.handler.getTimeChangeQueue().peek().getVoteId();
        assertEquals(List.of("First", "Later"), fixture.names());
        assertEquals("First", fixture.yaml.getString("VotingPlugin.TimedVoteCache.0.Name"));
        assertEquals("First", fixture.stored().getString("VotingPlugin.UncertainTimedVotes." + id + ".Name"));
        assertEquals(List.of("Later"), new Fixture(fixture.path).names());
        doCallRealMethod().when(fixture.yaml).save(fixture.path.toFile());
        var events = fixture.plugin.getServer().getPluginManager();
        AtomicInteger calls = new AtomicInteger();
        doAnswer(call -> {
            if (calls.incrementAndGet() == 1)
                assertEquals("First", fixture.stored().getString("VotingPlugin.UncertainTimedVotes." + id + ".Name"));
            return null;
        }).when(events).callEvent(any());
        fixture.handler.processQueue();
        assertEquals(2, calls.get());
        assertTrue(fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false).isEmpty());
    }

    @Test
    void failedQuarantineRetriesItsSameIdentityWithoutRepeatingTheEvent() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("Ambiguous", "site");
        fixture.handler.addVote("Later", "site");
        AtomicInteger calls = new AtomicInteger();
        var original = new IllegalStateException("synthetic event failure");
        var saved = new java.util.concurrent.atomic.AtomicReference<Path>();
        var events = fixture.plugin.getServer().getPluginManager();
        doAnswer(call -> {
            if (calls.incrementAndGet() == 1) { saved.set(fixture.blockWrites()); throw original; }
            return null;
        }).when(events).callEvent(any());
        assertSame(original, assertThrows(IllegalStateException.class, fixture.handler::processQueue));
        assertEquals(1, original.getSuppressed().length);
        assertEquals(List.of("Ambiguous", "Later"), fixture.names());
        UUID id = fixture.handler.getTimeChangeQueue().peek().getVoteId();
        fixture.repair(saved.get());
        fixture.handler.processQueue();
        assertEquals(2, calls.get());
        assertTrue(fixture.names().isEmpty());
        assertEquals(java.util.Set.of(id.toString()), fixture.stored()
                .getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false));
        assertTrue(new Fixture(fixture.path).names().isEmpty());
    }

    @Test
    void failedCompletionPublicationKeepsTheDeliveredOccurrenceOutOfAutomaticRecovery() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("Delivered", "site");
        fixture.handler.addVote("Later", "site");
        AtomicInteger calls = new AtomicInteger();
        var saved = new java.util.concurrent.atomic.AtomicReference<Path>();
        var events = fixture.plugin.getServer().getPluginManager();
        doAnswer(call -> { if (calls.incrementAndGet() == 1) saved.set(fixture.blockWrites()); return null; })
                .when(events).callEvent(any());
        assertThrows(java.io.UncheckedIOException.class, fixture.handler::processQueue);
        UUID id = fixture.handler.getTimeChangeQueue().peek().getVoteId();
        assertEquals("Delivered", fixture.yaml.getString("VotingPlugin.UncertainTimedVotes." + id + ".Name"));
        fixture.repair(saved.get());
        assertEquals(List.of("Later"), new Fixture(fixture.path).names());
        fixture.handler.processQueue();
        assertEquals(2, calls.get(), "successful event must not run again after its completion save failed");
        assertTrue(fixture.names().isEmpty());
        assertEquals(java.util.Set.of(id.toString()), fixture.stored()
                .getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false));
    }

    @Test
    void shutdownDuringARealInFlightDispatchRetainsTheSuffixWithoutWaitingForTheEvent() throws Exception {
        Fixture fixture = new Fixture(directory.resolve("ServerData.yml"));
        fixture.handler.addVote("InFlight", "site");
        fixture.handler.addVote("Later", "site");
        var entered = new java.util.concurrent.CountDownLatch(1);
        var release = new java.util.concurrent.CountDownLatch(1);
        var events = fixture.plugin.getServer().getPluginManager();
        doAnswer(call -> { entered.countDown(); assertTrue(release.await(5, java.util.concurrent.TimeUnit.SECONDS)); return null; })
                .when(events).callEvent(any());
        var worker = java.util.concurrent.Executors.newSingleThreadExecutor();
        try {
            var delivery = worker.submit(fixture.handler::processQueue);
            assertTrue(entered.await(5, java.util.concurrent.TimeUnit.SECONDS));
            fixture.handler.saveForShutdown();
            assertEquals(List.of("Later"), new Fixture(fixture.path).names());
            assertEquals(1, fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes")
                    .getKeys(false).size());
            assertFalse(delivery.isDone());
            release.countDown(); delivery.get(5, java.util.concurrent.TimeUnit.SECONDS);
            verify(events, times(1)).callEvent(any());
            assertEquals(1, fixture.stored().getConfigurationSection("VotingPlugin.UncertainTimedVotes")
                    .getKeys(false).size(), "late success must preserve the conservative shutdown record");
        } finally {
            release.countDown(); worker.shutdownNow();
            assertTrue(worker.awaitTermination(5, java.util.concurrent.TimeUnit.SECONDS));
        }
    }

    @Test
    void completeYamlWrappersKeepLiteralUnicodeAndAdjacentRecordsAcrossLfAndCrLf() throws Exception {
        for (String newline : List.of("\n", "\r\n")) {
            Path path = directory.resolve(newline.length() == 1 ? "lf.yml" : "crlf.yml");
            UUID other = new UUID(1, 1), current = new UUID(1, 2);
            Files.writeString(path, ("Adjacent: {Value: 'keep'}\nVotingPlugin:\n"
                    + "  Other: {Literal: '%player% [Javascript=1] 🎉'}\n"
                    + "  TimedVoteCache:\n    legacy: {Name: 'First', Service: 'Fête 🎉', Time: 123}\n"
                    + "  UncertainTimedVotes:\n    '" + other + "': {Name: 'Old', Service: 'site', Time: 1}\n")
                    .replace("\n", newline));
            Fixture fixture = new Fixture(path);
            var first = new VoteTimeQueue(current, "First", "%player% [Javascript=1] 🎉", 123L);
            var later = new VoteTimeQueue("Later", "Fête 🎉", 456L);
            fixture.data.prepareTimedVoteDispatch(current, first, List.of(later));
            YamlConfiguration stored = fixture.stored();
            assertEquals(first.getService(), stored.getString("VotingPlugin.UncertainTimedVotes." + current + ".Service"));
            assertEquals("Fête 🎉", stored.getString("VotingPlugin.TimedVoteCache.0.Service"));
            assertEquals(456L, stored.getLong("VotingPlugin.TimedVoteCache.0.Time"));
            fixture.data.completeTimedVoteDispatch(current);
            stored = fixture.stored();
            assertEquals(java.util.Set.of(other.toString()), stored.getConfigurationSection("VotingPlugin.UncertainTimedVotes").getKeys(false));
            assertEquals("%player% [Javascript=1] 🎉", stored.getString("VotingPlugin.Other.Literal"));
            assertEquals("keep", stored.getString("Adjacent.Value"));
        }
    }

    private static final class Fixture {
        final Path path;
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final YamlConfiguration yaml;
        final ServerData data;
        final TimeQueueHandler handler;

        Fixture(Path path) throws Exception {
            this.path = path;
            yaml = spy(java.nio.file.Files.exists(path) ? YamlConfiguration.loadConfiguration(path.toFile())
                    : new YamlConfiguration());
            yaml.set("Adjacent.Value", "keep");
            var file = mock(com.bencodez.advancedcore.data.ServerData.class);
            when(file.getData()).thenReturn(yaml);
            when(file.getdFile()).thenReturn(path.toFile());
            when(plugin.getServerDataFile()).thenReturn(file);
            data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            handler = new TimeQueueHandler(plugin);
        }

        YamlConfiguration stored() {
            return YamlConfiguration.loadConfiguration(path.toFile());
        }

        List<String> names() {
            return handler.getTimeChangeQueue().stream().map(v -> v.getName()).toList();
        }

        Path blockWrites() throws Exception {
            Path saved = path.resolveSibling(path.getFileName() + ".saved");
            Files.move(path, saved);
            Files.createDirectory(path);
            return saved;
        }

        void repair(Path saved) throws Exception {
            Files.delete(path);
            Files.move(saved, path);
        }
    }
}
