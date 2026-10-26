package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Logger;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class GenerationFencedServerDataFinalPublicationTest {
    @TempDir Path directory;

    @Test void ordinaryServiceProducerCannotRaceTheActualSDKListSnapshot() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var snapshotEntered = new java.util.concurrent.CountDownLatch(1);
            var releaseSnapshot = new java.util.concurrent.CountDownLatch(1);
            var producerStarted = new java.util.concurrent.CountDownLatch(1);
            var mutatedLiveList = new java.util.concurrent.CountDownLatch(1);
            var firstSnapshot = new java.util.concurrent.atomic.AtomicBoolean(true);
            var sites = new java.util.ArrayList<String>() {
                @Override public boolean add(String value) {
                    boolean added = super.add(value);
                    if (value.equals("SyntheticLateSite")) mutatedLiveList.countDown();
                    return added;
                }
                @Override public java.util.stream.Stream<String> stream() {
                    var stream = super.stream();
                    if (!firstSnapshot.compareAndSet(true, false)) return stream;
                    return stream.peek(value -> {
                        snapshotEntered.countDown();
                        try { assertTrue(releaseSnapshot.await(5, TimeUnit.SECONDS)); }
                        catch (InterruptedException failure) { throw new AssertionError(failure); }
                    });
                }
            };
            sites.add("SyntheticExistingSite");
            backing.getData().set("VotingPlugin.GottenServiceSites", sites);
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            var workers = java.util.concurrent.Executors.newFixedThreadPool(2);
            try {
                var publication = workers.submit(() -> facade.mutateFinalData(snapshot ->
                        snapshot.set("Final.Value", "confirmed")).toCompletableFuture().join());
                assertTrue(snapshotEntered.await(5, TimeUnit.SECONDS));
                var producer = workers.submit(() -> {
                    producerStarted.countDown();
                    data.addServiceSite("SyntheticLateSite");
                });
                assertTrue(producerStarted.await(5, TimeUnit.SECONDS));
                boolean overlapped = mutatedLiveList.await(200, TimeUnit.MILLISECONDS);
                releaseSnapshot.countDown();
                assertDoesNotThrow(() -> publication.get(5, TimeUnit.SECONDS),
                        "ordinary producers must not invalidate the real SDK final snapshot");
                producer.get(5, TimeUnit.SECONDS);
                assertFalse(overlapped, "live service history changed while the SDK owned its snapshot");
                var disk = YamlConfiguration.loadConfiguration(target.toFile());
                assertEquals(java.util.Set.of("SyntheticExistingSite", "SyntheticLateSite"),
                        new java.util.HashSet<>(disk.getStringList("VotingPlugin.GottenServiceSites")));
                assertEquals("confirmed", disk.getString("Final.Value"));
            } finally {
                releaseSnapshot.countDown();
                workers.shutdownNow();
                assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        }
    }

    @Test void ordinaryWriterFamiliesMutateOnlyUnderTheActualYamlOwner() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            var yaml = new YamlConfiguration() {
                @Override public void set(String path, Object value) {
                    assertTrue(Thread.holdsLock(backing), "YAML mutation escaped its actual owner: " + path);
                    super.set(path, value);
                }
            };
            backing.setData(yaml);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            try {
                data.addServiceSite("SyntheticSite");
                data.addAutoCachedPlaceholder("SyntheticPlaceholder");
                data.addVoteShopPurchase("SyntheticItem");
                data.setBungeeVotePartyCurrent(3);
                data.setBungeeVotePartyRequired(9);
                data.setVotePartyExtraRequired(2);
                data.setShopConverted(true);
                data.saveDisabledReminders(new java.util.ArrayList<>());
                data.updatePlaceholders();
                var party = new com.bencodez.votingplugin.specialrewards.voteparty.VotePartyState(plugin);
                party.setTotalVotes(4);
                var users = new java.util.ArrayList<>(java.util.List.of("SyntheticParticipant"));
                party.setVotedUsers(users);
                users.add("UnpublishedParticipant");
                assertEquals(java.util.List.of("SyntheticParticipant"), party.getVotedUsers());
                // Suppress web refresh only; exercise the real handler and its actual YAML writer.
                var services = new com.bencodez.votingplugin.servicesites.ServiceSiteHandler(plugin) {
                    @Override public void refreshFromWebOnce(boolean logMore) { }
                };
                services.saveToServerData();
                var world = mock(org.bukkit.World.class);
                when(world.getName()).thenAnswer(call -> {
                    assertFalse(Thread.holdsLock(backing), "platform lookup ran under YAML ownership");
                    return "SyntheticWorld";
                });
                var location = new org.bukkit.Location(world, -1.5, 64, 2.5);
                data.setSign("0", location, location, "SyntheticSign", 1);
                data.setSkullLocation("0", location);
                assertEquals(-1, data.getData().getInt("Signs.0.X"), "retain legacy truncation");
                data.removeSign("0");
                var disk = YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile());
                assertEquals(4, disk.getInt("VotingPlugin.VoteParty.Total"));
                assertEquals(java.util.List.of("SyntheticParticipant"), disk.getStringList("VotingPlugin.VoteParty.Voted"));
                assertEquals(1, disk.getInt("VotingPlugin.VoteShopPurchases.SyntheticItem"));
                assertFalse(disk.contains("VotingPlugin.Signs.0"));
            } finally { fence.retire(); }
        }
    }

    @Test void ordinaryMutationFamiliesWaitBeforeChangingAnOwnedSnapshot() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            data.getData(); // Section already exists: test mutation, not lazy section creation.
            var party = new com.bencodez.votingplugin.specialrewards.voteparty.VotePartyState(plugin);
            var services = new com.bencodez.votingplugin.servicesites.ServiceSiteHandler(plugin) {
                @Override public void refreshFromWebOnce(boolean logMore) { }
            };
            var operations = java.util.List.<Runnable>of(
                    () -> data.setBungeeVotePartyCurrent(3),
                    () -> data.addServiceSite("SyntheticSite"),
                    () -> data.setServiceSites(new java.util.ArrayList<>(java.util.List.of("SyntheticReplacement"))),
                    () -> data.addVoteShopPurchase("SyntheticItem"),
                    () -> data.addAutoCachedPlaceholder("SyntheticPlaceholder"),
                    () -> data.updatePlaceholders(),
                    () -> data.removeSign("0"),
                    () -> party.setTotalVotes(4),
                    () -> party.setVotedUsers(java.util.List.of("SyntheticParticipant")),
                    services::saveToServerData);
            var worker = java.util.concurrent.Executors.newSingleThreadExecutor();
            try {
                for (Runnable operation : operations) {
                    var started = new java.util.concurrent.CountDownLatch(1);
                    java.util.concurrent.Future<?> future;
                    synchronized (backing) {
                        String before = backing.getData().saveToString();
                        future = worker.submit(() -> { started.countDown(); operation.run(); });
                        assertTrue(started.await(5, TimeUnit.SECONDS));
                        assertThrows(java.util.concurrent.TimeoutException.class,
                                () -> future.get(100, TimeUnit.MILLISECONDS));
                        assertEquals(before, backing.getData().saveToString(),
                                "ordinary mutation changed YAML before acquiring the actual snapshot owner");
                    }
                    future.get(5, TimeUnit.SECONDS);
                }
            } finally {
                worker.shutdownNow();
                assertTrue(worker.awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        }
    }

    @Test void serviceListInputsAndOutputsCannotMutatePublishedYamlByAlias() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            try {
                var supplied = new java.util.ArrayList<>(java.util.List.of("SyntheticExistingSite"));
                data.setServiceSites(supplied);
                supplied.add("SyntheticInputAlias");
                data.getServiceSites().add("SyntheticOutputAlias");
                assertEquals(java.util.List.of("SyntheticExistingSite"), data.getServiceSitesReadOnly());
                data.saveData();
                var disk = YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile());
                assertEquals(java.util.List.of("SyntheticExistingSite"), disk.getStringList("VotingPlugin.GottenServiceSites"));
                data.setServiceSites(null);
                assertTrue(data.getServiceSites().isEmpty(), "retain legacy null-as-clear setter behavior");
            } finally { fence.retire(); }
        }
    }

    @Test void finalMutationConfirmsTheActualSDKOwnersPublication() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            checkFinalMutation();
        }
    }

    private void checkFinalMutation() throws Exception {
        var plugin = mock(VotingPluginMain.class);
        when(plugin.getDataFolder()).thenReturn(directory.toFile());
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
        backing.setData(new YamlConfiguration());
        backing.getData().set("Adjacent.Value", "preserved");
        var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
        try {
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            facade.mutateFinalData(data -> data.set("Final.Value", "confirmed"))
                    .toCompletableFuture().get(5, TimeUnit.SECONDS);
            assertSame(backing.getData(), facade.getData());
            var disk = YamlConfiguration.loadConfiguration(directory.resolve("ServerData.yml").toFile());
            assertEquals("confirmed", disk.getString("Final.Value"));
            assertEquals("preserved", disk.getString("Adjacent.Value"));
        } finally {
            fence.retire();
        }
    }

    @Test void asynchronousOwnerRetainsAdmissionUntilItsActualCompletion() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            var backing = mock(com.bencodez.advancedcore.data.ServerData.class);
            var published = new CompletableFuture<Void>();
            when(backing.mutateFinalData(any())).thenReturn(published);
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            var observation = facade.mutateFinalData(data -> {}).toCompletableFuture();
            fence.retire();
            assertFalse(observation.isDone());
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            observation.cancel(false);
            assertFalse(published.isCancelled());
            assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            published.complete(null);
            var successor = ServerDataPublicationFence.beginGeneration(target);
            successor.retire();
        }
    }

    @Test void failedOrCancelledPublicationCannotReleaseAdmission() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            for (boolean cancelled : new boolean[] { false, true }) {
                var plugin = mock(VotingPluginMain.class);
                when(plugin.getDataFolder()).thenReturn(directory.toFile());
                var backing = mock(com.bencodez.advancedcore.data.ServerData.class);
                var published = new CompletableFuture<Void>();
                when(backing.mutateFinalData(any())).thenReturn(published);
                var target = directory.resolve(cancelled ? "Cancelled.yml" : "Failed.yml");
                var fence = ServerDataPublicationFence.beginGeneration(target);
                var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
                var observation = facade.mutateFinalData(data -> {}).toCompletableFuture();
                fence.retire();
                if (cancelled) published.cancel(false);
                else published.completeExceptionally(new java.io.IOException("synthetic publication failure"));
                assertThrows(java.util.concurrent.ExecutionException.class,
                        () -> observation.get(5, TimeUnit.SECONDS));
                assertThrows(IllegalStateException.class, () -> ServerDataPublicationFence.beginGeneration(target));
            }
        }
    }

    @Test void ordinaryMutatorsAndCheckedTransitionsUseTheSameDelegateOwner() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            try {
                var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
                assertSame(backing, facade.publicationMonitor());
                facade.setPrevDay(12);
                facade.setPrevMonth("OCTOBER");
                facade.setPrevWeekDay(41);
                facade.setIgnoreTime(true);
                facade.setData("Adjacent.Value", "ordinary");
                var transition = facade.beginTimeChangeTransition(
                        com.bencodez.advancedcore.api.time.TimeType.DAY, "synthetic-day", "13");
                assertEquals(transition, backing.getPendingTimeChangeTransition(
                        com.bencodez.advancedcore.api.time.TimeType.DAY));
                facade.mutateFinalData(data -> data.set("Final.Value", "confirmed"))
                        .toCompletableFuture().get(5, TimeUnit.SECONDS);
                facade.completeTimeChangeTransition(transition);
                assertFalse(backing.isPendingTimeChangeTransition(transition));
                assertEquals(13, backing.getPrevDay());
                var disk = YamlConfiguration.loadConfiguration(target.toFile());
                assertEquals("ordinary", disk.getString("Adjacent.Value"));
                assertEquals("confirmed", disk.getString("Final.Value"));
                assertEquals(13, disk.getInt("PrevDay"));
            } finally { fence.retire(); }
        }
    }

    @Test void timedCheckpointCannotOvertakeTheNativeFinalSnapshot() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            backing.setData(new YamlConfiguration());
            var target = directory.resolve("ServerData.yml");
            var fence = ServerDataPublicationFence.beginGeneration(target);
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var data = new com.bencodez.votingplugin.data.ServerData(plugin);
            var entered = new java.util.concurrent.CountDownLatch(1);
            var release = new java.util.concurrent.CountDownLatch(1);
            var checkpointStarted = new java.util.concurrent.CountDownLatch(1);
            var checkpointFinished = new java.util.concurrent.CountDownLatch(1);
            var workers = java.util.concurrent.Executors.newFixedThreadPool(2);
            try {
                var publication = workers.submit(() -> facade.mutateFinalData(snapshot -> {
                    snapshot.set("Final.Value", "confirmed");
                    entered.countDown();
                    try { assertTrue(release.await(5, TimeUnit.SECONDS)); }
                    catch (InterruptedException failure) { throw new AssertionError(failure); }
                }).toCompletableFuture().join());
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                var checkpoint = workers.submit(() -> {
                    checkpointStarted.countDown();
                    data.addTimeVoted(0, new com.bencodez.votingplugin.timequeue.VoteTimeQueue(
                            "Synthetic", "site", 123L));
                    checkpointFinished.countDown();
                });
                assertTrue(checkpointStarted.await(5, TimeUnit.SECONDS));
                assertFalse(checkpointFinished.await(150, TimeUnit.MILLISECONDS));
                release.countDown();
                publication.get(5, TimeUnit.SECONDS);
                checkpoint.get(5, TimeUnit.SECONDS);
                var disk = YamlConfiguration.loadConfiguration(target.toFile());
                assertEquals("confirmed", disk.getString("Final.Value"));
                assertEquals("Synthetic", disk.getString("VotingPlugin.TimedVoteCache.0.Name"));
                assertEquals(123L, disk.getLong("VotingPlugin.TimedVoteCache.0.Time"));
            } finally {
                release.countDown();
                workers.shutdownNow();
                assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        }
    }
}
