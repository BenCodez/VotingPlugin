package com.bencodez.votingplugin.tests.placeholders;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.GenerationFencedServerDataFile;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.specialrewards.voteparty.VoteParty;
import com.bencodez.votingplugin.util.ServerDataPublicationFence;

class VotePartyPublicationReadTest {
    @TempDir Path directory;

    @Test void allVotePartyPlaceholdersReturnWhilePublicationIsStalled() throws Exception {
        var previousPlugin = VotingPluginMain.plugin;
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            VotingPluginMain.plugin = plugin;
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.NONE);
            when(plugin.getSpecialRewardsConfig().getVotePartyVotesRequired()).thenReturn(20);
            var entered = new CountDownLatch(1);
            var release = new CountDownLatch(1);
            var armed = new AtomicBoolean();
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin) {
                @Override public void reloadData() {
                    getData().set("VotingPlugin.VoteParty.Total", 31);
                    getData().set("VotingPlugin.VotePartyExtraRequired", 4);
                }

                @Override public void saveData() {
                    assertTrue(Thread.holdsLock(publicationMonitor()));
                    if (armed.get()) {
                        entered.countDown();
                        try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                        catch (InterruptedException failure) { throw new AssertionError(failure); }
                    }
                }
            };
            var yaml = new YamlConfiguration();
            yaml.set("VotingPlugin.VoteParty.Total", 7);
            yaml.set("VotingPlugin.VotePartyExtraRequired", 3);
            backing.setData(yaml);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            var exposed = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataFile()).thenReturn(exposed);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            when(plugin.getVoteParty()).thenReturn(new VoteParty(plugin));
            data.saveData();
            var placeholders = new PlaceHolders(plugin);
            placeholders.load();
            when(plugin.getSpecialRewardsConfig().isVotePartyEnabled()).thenReturn(true);
            when(plugin.getConfigFile().getFormatCommandsVoteParty())
                    .thenReturn(new java.util.ArrayList<>(java.util.List.of("%votes%/%votesrequired%/%neededvotes%")));
            var sender = mock(org.bukkit.command.CommandSender.class);
            var workers = Executors.newFixedThreadPool(2);
            try {
                armed.set(true);
                var saving = workers.submit(() -> data.setInt("VoteParty.Total", 9));
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                var reading = workers.submit(() -> {
                    assertEquals("9", value(placeholders, "VotePartyVotesCurrent"));
                    assertEquals("23", value(placeholders, "VotePartyVotesRequired"));
                    assertEquals("14", value(placeholders, "VotePartyVotesNeeded"));
                    plugin.getVoteParty().commandVoteParty(sender);
                    verify(sender).sendMessage(new String[] { "9/23/14" });
                });
                reading.get(2, TimeUnit.SECONDS);
                assertFalse(saving.isDone(), "the read must finish before the disk writer resumes");
                release.countDown();
                saving.get(5, TimeUnit.SECONDS);
                assertEquals(9, plugin.getVoteParty().getTotalVotes(), "accounting still reads authoritative YAML");
                data.setVotePartyExtraRequired(-2);
                assertEquals("20", value(placeholders, "VotePartyVotesRequired"));
                assertEquals("11", value(placeholders, "VotePartyVotesNeeded"));
                data.setVotePartyExtraRequired(6);
                assertEquals("26", value(placeholders, "VotePartyVotesRequired"));
                assertEquals("17", value(placeholders, "VotePartyVotesNeeded"));
                exposed.reloadData();
                assertEquals("31", value(placeholders, "VotePartyVotesCurrent"));
                assertEquals("24", value(placeholders, "VotePartyVotesRequired"));
                assertEquals("-7", value(placeholders, "VotePartyVotesNeeded"));
            } finally {
                release.countDown();
                workers.shutdownNow();
                assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
                fence.retire();
            }
        } finally { VotingPluginMain.plugin = previousPlugin; }
    }

    @Test void preLoadConstructionRemainsLazyUntilTheSdkCreatesItsFile() {
        var plugin = mock(VotingPluginMain.class);
        var data = new ServerData(plugin);
        assertEquals(0, data.getVotePartyDisplay().total());
        verify(plugin, never()).getServerDataFile();
    }

    @Test void startupReloadAndFailedSaveRetainLiveDisplaySemantics() {
        var plugin = mock(VotingPluginMain.class);
        var backing = mock(com.bencodez.advancedcore.data.ServerData.class);
        doCallRealMethod().when(backing).mutateOrdinaryData(any());
        when(backing.publicationMonitor()).thenReturn(new Object());
        var initial = new YamlConfiguration();
        initial.set("VotingPlugin.VoteParty.Total", 11);
        initial.set("VotingPlugin.VotePartyExtraRequired", 4);
        when(backing.getData()).thenReturn(initial);
        when(plugin.getServerDataFile()).thenReturn(backing);
        var data = new ServerData(plugin);
        data.refreshVotePartyDisplay();
        var first = data.getVotePartyDisplay();
        assertEquals(11, first.total());
        assertEquals(24, first.required(20));
        verify(backing, never()).saveData();

        var reloaded = new YamlConfiguration();
        reloaded.set("VotingPlugin.VoteParty.Total", 13);
        doAnswer(invocation -> { when(backing.getData()).thenReturn(reloaded); return null; })
                .when(backing).reloadData();
        data.reloadData();
        assertEquals(13, data.getVotePartyDisplay().total());
        assertEquals(20, data.getVotePartyDisplay().required(20));
        assertEquals(11, first.total(), "previously published snapshots remain immutable");

        doThrow(new IllegalStateException("Synthetic publication failure")).when(backing).saveData();
        assertThrows(IllegalStateException.class, () -> data.setInt("VoteParty.Total", 17));
        assertEquals(17, data.getVotePartyDisplay().total(), "display follows live YAML, not a durability claim");
        assertEquals(17, data.getData().getInt("VoteParty.Total"));
    }

    @Test void sdkSetDataPublishesItsLiveDisplayBeforeDiskIo() throws Exception {
        sdkMutationAndSaveEntryPoint("setData");
    }

    @Test void sdkSetValuePublishesItsLiveDisplayBeforeDiskIo() throws Exception {
        sdkMutationAndSaveEntryPoint("setValue");
    }

    @Test void sdkSaveDataPublishesItsLiveDisplayBeforeDiskIo() throws Exception {
        sdkMutationAndSaveEntryPoint("saveData");
    }

    @Test void sdkCheckedSavePublishesItsLiveDisplayBeforeDiskIo() throws Exception {
        sdkMutationAndSaveEntryPoint("saveDataChecked");
    }

    private void sdkMutationAndSaveEntryPoint(String operation) throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            var yaml = spy(new YamlConfiguration());
            yaml.set("VotingPlugin.VoteParty.Total", 7);
            yaml.set("VotingPlugin.VotePartyExtraRequired", 3);
            backing.setData(yaml);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var exposed = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            when(plugin.getServerDataFile()).thenReturn(exposed);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            data.refreshVotePartyDisplay();
            var saves = new java.util.concurrent.atomic.AtomicInteger();
            doAnswer(call -> {
                assertTrue(Thread.holdsLock(backing), "the actual SDK owner must hold live YAML");
                assertEquals(17, yaml.getInt("VotingPlugin.VoteParty.Total"));
                assertEquals(17, data.getVotePartyDisplay().total(),
                        "display must be refreshed before the blocking backing save");
                assertEquals(3, data.getVotePartyDisplay().extraRequired());
                saves.incrementAndGet();
                return call.callRealMethod();
            }).when(yaml).save(any(java.io.File.class));
            try {
                switch (operation) {
                    case "setData" -> exposed.setData("VotingPlugin.VoteParty.Total", 17);
                    case "setValue" -> exposed.setValue("VotingPlugin.VoteParty.Total", 17);
                    case "saveData" -> {
                        synchronized (exposed.publicationMonitor()) {
                            exposed.getData().set("VotingPlugin.VoteParty.Total", 17);
                            exposed.saveData();
                        }
                    }
                    case "saveDataChecked" -> {
                        synchronized (exposed.publicationMonitor()) {
                            exposed.getData().set("VotingPlugin.VoteParty.Total", 17);
                            exposed.saveDataChecked();
                        }
                    }
                    default -> throw new AssertionError(operation);
                }
                assertEquals(1, saves.get(), "one real backing save must be exercised");
                assertEquals(17, YamlConfiguration.loadConfiguration(backing.getdFile())
                        .getInt("VotingPlugin.VoteParty.Total"));
                assertEquals(17, data.getVotePartyDisplay().total());
            } finally { fence.retire(); }
        }
    }

    @Test void sdkBulkMergeKeepsItsNoSaveSemanticsAndRefreshesTheLiveDisplay() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            var original = new YamlConfiguration();
            backing.setData(original);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var exposed = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            when(plugin.getServerDataFile()).thenReturn(exposed);
            var data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            data.refreshVotePartyDisplay();
            var replacement = new YamlConfiguration();
            replacement.set("VotingPlugin.VoteParty.Total", 19);
            replacement.set("VotingPlugin.VotePartyExtraRequired", 4);
            try {
                exposed.setData(replacement);
                assertSame(original, exposed.getData(), "bulk merge must retain the SDK's existing live YAML identity");
                assertFalse(java.nio.file.Files.exists(backing.getdFile().toPath()), "bulk merge must not save");
                assertEquals(19, data.getVotePartyDisplay().total());
                assertEquals(4, data.getVotePartyDisplay().extraRequired());
            } finally { fence.retire(); }
        }
    }

    @Test void nativeFinalPublicationRefreshesOnlyAfterItsLiveCommit() throws Exception {
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void saveDataChecked() {
                        assertTrue(Thread.holdsLock(publicationMonitor()));
                        entered.countDown();
                        try { assertTrue(release.await(10, TimeUnit.SECONDS)); }
                        catch (InterruptedException failure) { throw new AssertionError(failure); }
                        super.saveDataChecked();
                    }
                })) {
            var workers = Executors.newFixedThreadPool(2);
            var original = fixture.data.getVotePartyDisplay();
            try {
                var publication = workers.submit(() -> fixture.exposed.mutateFinalData(candidate ->
                        candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture().get(5, TimeUnit.SECONDS));
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                var reading = workers.submit(() -> fixture.data.getVotePartyDisplay());
                assertEquals(7, reading.get(2, TimeUnit.SECONDS).total(),
                        "an unpublished final candidate must not replace the live projection");
                assertFalse(publication.isDone());
                release.countDown();
                publication.get(5, TimeUnit.SECONDS);
                assertSame(fixture.yaml, fixture.backing.getData());
                assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
                assertEquals(17, fixture.data.getVotePartyDisplay().total(), "confirmed native commit must refresh display");
                assertEquals(7, original.total(), "prior published records remain immutable");
                assertEquals(17, YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                        .getInt("VotingPlugin.VoteParty.Total"));
            } finally {
                release.countDown();
                workers.shutdownNow();
                assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS));
            }
        }
    }

    @Test void failedNativeFinalCandidateKeepsTheLiveProjectionAndRetainsAdmission() throws Exception {
        var failure = new IllegalStateException("Synthetic checked publication failure");
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void saveDataChecked() { throw failure; }
                })) {
            var observation = fixture.exposed.mutateFinalData(candidate ->
                    candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            var observed = assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> observation.get(5, TimeUnit.SECONDS));
            assertSame(failure, observed.getCause());
            assertEquals(7, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(7, fixture.data.getVotePartyDisplay().total());
            assertFalse(java.nio.file.Files.exists(fixture.backing.getdFile().toPath()));
            fixture.fence.retire();
            assertThrows(IllegalStateException.class, () ->
                    ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void legacyFinalMutationRefreshesBeforeSaveWithoutClaimingDurableAcknowledgement() throws Exception {
        var display = new java.util.concurrent.atomic.AtomicReference<ServerData>();
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void saveData() {
                        assertTrue(Thread.holdsLock(publicationMonitor()));
                        assertEquals(17, display.get().getVotePartyDisplay().total(),
                                "a legacy owner's live mutation must be visible before its disk save");
                        super.saveData();
                    }
                })) {
            display.set(fixture.data);
            var observation = fixture.exposed.mutateFinalData(live ->
                    live.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            var failure = assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> observation.get(5, TimeUnit.SECONDS));
            assertInstanceOf(UnsupportedOperationException.class, failure.getCause(),
                    "the existing SDK legacy-save contract must not become a durable-success acknowledgement");
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertEquals(17, YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                    .getInt("VotingPlugin.VoteParty.Total"));
            fixture.fence.retire();
            assertThrows(IllegalStateException.class, () ->
                    ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void cancelledObservationStillRefreshesAfterTheAsynchronousOwnersActualCommit() throws Exception {
        class DeferredOwner extends com.bencodez.advancedcore.data.ServerData {
            final java.util.concurrent.CompletableFuture<Void> publication = new java.util.concurrent.CompletableFuture<>();
            java.util.function.Consumer<org.bukkit.configuration.file.FileConfiguration> mutation;
            DeferredOwner(VotingPluginMain plugin) { super(plugin); }
            @Override public java.util.concurrent.CompletionStage<Void> mutateFinalData(
                    java.util.function.Consumer<org.bukkit.configuration.file.FileConfiguration> operation) {
                mutation = operation;
                return publication;
            }
            void publish() {
                // Defer the real native mutation and durable publication, rather than mocking their result.
                super.mutateFinalData(mutation).whenComplete((ignored, failure) -> {
                    if (failure == null) publication.complete(null);
                    else publication.completeExceptionally(failure);
                });
            }
        }
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(DeferredOwner::new)) {
            var owner = (DeferredOwner) fixture.backing;
            var observation = fixture.exposed.mutateFinalData(candidate ->
                    candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            assertEquals(7, fixture.data.getVotePartyDisplay().total());
            assertFalse(owner.publication.isDone());
            assertTrue(observation.cancel(false));
            assertFalse(owner.publication.isCancelled());
            fixture.fence.retire();
            assertThrows(IllegalStateException.class, () ->
                    ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath()));
            owner.publish();
            owner.publication.get(5, TimeUnit.SECONDS);
            assertTrue(observation.isCancelled());
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertEquals(17, YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                    .getInt("VotingPlugin.VoteParty.Total"));
            var successor = ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath());
            successor.retire();
        }
    }

    @Test void finalPublicationWithoutADisplayWrapperDoesNotAddYamlObservation() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            var backing = mock(com.bencodez.advancedcore.data.ServerData.class);
            var publication = new java.util.concurrent.CompletableFuture<Void>();
            when(backing.mutateFinalData(any())).thenReturn(publication);
            when(backing.publicationMonitor()).thenThrow(new AssertionError("No display owner to observe"));
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            try {
                var observation = facade.mutateFinalData(data -> {}).toCompletableFuture();
                fence.retire();
                assertFalse(observation.isDone());
                assertThrows(IllegalStateException.class, () ->
                        ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml")));
                publication.complete(null);
                observation.get(5, TimeUnit.SECONDS);
                verify(backing, never()).publicationMonitor();
                verify(backing, never()).getData();
                var successor = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
                successor.retire();
            } finally { fence.retire(); }
        }
    }

    @Test void publicOrdinaryMutationOwnsYamlAndRefreshesBeforeItsSave() throws Exception {
        var display = new java.util.concurrent.atomic.AtomicReference<ServerData>();
        var saves = new java.util.concurrent.atomic.AtomicInteger();
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void saveData() {
                        assertTrue(Thread.holdsLock(publicationMonitor()));
                        assertEquals(17, display.get().getVotePartyDisplay().total());
                        saves.incrementAndGet();
                        super.saveData();
                    }
                })) {
            display.set(fixture.data);
            fixture.exposed.mutateOrdinaryData(live -> {
                assertTrue(Thread.holdsLock(fixture.backing.publicationMonitor()),
                        "the newly public inherited entry must acquire the actual YAML owner before mutation");
                assertSame(fixture.yaml, live);
                live.set("VotingPlugin.VoteParty.Total", 17);
            });
            assertEquals(1, saves.get());
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertSame(fixture.yaml, fixture.backing.getData());
            assertEquals(17, YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                    .getInt("VotingPlugin.VoteParty.Total"));
        }
    }

    @Test void ordinaryCallbackFailureKeepsLiveDisplayAndOriginalFailureWithoutSaving() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(com.bencodez.advancedcore.data.ServerData::new)) {
            var failure = new IllegalArgumentException("Synthetic ordinary callback failure");
            assertSame(failure, assertThrows(IllegalArgumentException.class, () ->
                    fixture.exposed.mutateOrdinaryData(live -> {
                        live.set("VotingPlugin.VoteParty.Total", 17);
                        throw failure;
                    })));
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertFalse(java.nio.file.Files.exists(fixture.backing.getdFile().toPath()),
                    "a failed callback must not run an ordinary save or claim rollback");
        }
    }

    @Test void sdkNullRemovalAndSectionCreationRefreshTheSameLiveYaml() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(com.bencodez.advancedcore.data.ServerData::new)) {
            fixture.exposed.setData("VotingPlugin.VoteParty.Total", null);
            assertFalse(fixture.yaml.contains("VotingPlugin.VoteParty.Total"));
            assertEquals(0, fixture.data.getVotePartyDisplay().total());
            fixture.exposed.setValue("VotingPlugin.VoteParty.Total", 11);
            assertEquals(11, fixture.data.getVotePartyDisplay().total());
            fixture.exposed.createSection("VotingPlugin.VoteParty");
            assertNotNull(fixture.yaml.getConfigurationSection("VotingPlugin.VoteParty"));
            assertEquals(0, fixture.data.getVotePartyDisplay().total());
            assertEquals(3, fixture.data.getVotePartyDisplay().extraRequired());
            assertSame(fixture.yaml, fixture.backing.getData());
            assertFalse(YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                    .contains("VotingPlugin.VoteParty.Total"));
        }
    }

    @Test void retiredOrdinaryEntryRejectsBeforeInvokingTheCallback() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(com.bencodez.advancedcore.data.ServerData::new)) {
            fixture.fence.retire();
            var successor = ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath());
            try {
                var called = new AtomicBoolean();
                assertThrows(IllegalStateException.class, () ->
                        fixture.exposed.mutateOrdinaryData(live -> called.set(true)));
                assertFalse(called.get(), "a stale facade must reject before caller effects");
                assertEquals(7, fixture.yaml.getInt("VotingPlugin.VoteParty.Total"));
                assertEquals(7, fixture.data.getVotePartyDisplay().total());
            } finally { successor.retire(); }
        }
    }

    @Test void sdkSetupReloadsTheLiveDisplayFromTheExistingFile() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(com.bencodez.advancedcore.data.ServerData::new)) {
            var replacement = new YamlConfiguration();
            replacement.set("VotingPlugin.VoteParty.Total", 29);
            replacement.set("VotingPlugin.VotePartyExtraRequired", 4);
            replacement.save(fixture.backing.getdFile());
            byte[] before = java.nio.file.Files.readAllBytes(fixture.backing.getdFile().toPath());
            fixture.exposed.setup();
            assertEquals(29, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(29, fixture.data.getVotePartyDisplay().total());
            assertEquals(4, fixture.data.getVotePartyDisplay().extraRequired());
            assertArrayEquals(before, java.nio.file.Files.readAllBytes(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void sdkCaseModeChangesRefreshWithoutSavingOrRewritingKeys() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(com.bencodez.advancedcore.data.ServerData::new)) {
            synchronized (fixture.backing.publicationMonitor()) {
                fixture.yaml.set("VotingPlugin", null);
                fixture.yaml.set("votingplugin.VoteParty.Total", 17);
                fixture.yaml.set("votingplugin.VotePartyExtraRequired", 4);
            }
            fixture.data.refreshVotePartyDisplay();
            assertEquals(0, fixture.data.getVotePartyDisplay().total());
            String original = fixture.yaml.saveToString();
            fixture.exposed.setIgnoreCase(true);
            assertEquals(17, fixture.backing.getData().getConfigurationSection("VotingPlugin")
                    .getInt("VoteParty.Total"), "verify the actual SDK section contract before its display");
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertEquals(4, fixture.data.getVotePartyDisplay().extraRequired());
            fixture.exposed.setIgnoreCase(false);
            assertEquals(0, fixture.data.getVotePartyDisplay().total());
            assertSame(fixture.yaml, fixture.backing.getData());
            assertEquals(original, fixture.yaml.saveToString());
            assertFalse(java.nio.file.Files.exists(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void sdkLoadValuesRefreshesWithoutAnOrdinarySave() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void loadValues() { getData().set("VotingPlugin.VoteParty.Total", 17); }
                })) {
            fixture.exposed.loadValues();
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertFalse(java.nio.file.Files.exists(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void sdkFailedLoadRefreshesLiveValuesAndPreservesFailure() throws Exception {
        var failure = new IllegalArgumentException("Synthetic load failure after live mutation");
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void loadValues() {
                        getData().set("VotingPlugin.VoteParty.Total", 17);
                        throw failure;
                    }
                })) {
            assertSame(failure, assertThrows(IllegalArgumentException.class, fixture.exposed::loadValues));
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            assertFalse(java.nio.file.Files.exists(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void sdkLifecycleHooksRemainLazyWhileTheFileIsUninitialized() throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            var backing = new com.bencodez.advancedcore.data.ServerData(plugin);
            var fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            var facade = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            when(plugin.getServerDataFile()).thenReturn(facade);
            var display = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(display);
            try {
                facade.loadValues();
                facade.onFileCreation();
                facade.setIgnoreCase(true);
                assertNull(backing.getData());
                assertEquals(0, display.getVotePartyDisplay().total());
                assertFalse(java.nio.file.Files.exists(backing.getdFile().toPath()));
                verify(plugin, never()).getServerDataFile();
            } finally { fence.retire(); }
        }
    }

    @Test void sdkFailedLoadKeepsItsPrimaryWhenObservationAlsoFails() throws Exception {
        var primary = new IllegalArgumentException("Synthetic primary load failure");
        var secondary = new IllegalStateException("Synthetic unavailable live observation");
        var unavailable = new AtomicBoolean();
        var observations = new java.util.concurrent.atomic.AtomicInteger();
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public org.bukkit.configuration.file.FileConfiguration getData() {
                        if (unavailable.get()) { observations.incrementAndGet(); throw secondary; }
                        return super.getData();
                    }
                    @Override public void loadValues() {
                        super.getData().set("VotingPlugin.VoteParty.Total", 17);
                        unavailable.set(true);
                        throw primary;
                    }
                })) {
            try {
                assertSame(primary, assertThrows(IllegalArgumentException.class, fixture.exposed::loadValues));
                if (observations.get() > 0) {
                    assertTrue(java.util.Arrays.asList(primary.getSuppressed()).contains(secondary),
                            "an attempted observer failure must remain attached to the original load error");
                }
            } finally { unavailable.set(false); }
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            fixture.data.refreshVotePartyDisplay();
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
        }
    }

    @Test void wrapperOrdinarySavesKeepOneOwnerCallDetachedListsAndConditionalNoOp() throws Exception {
        var saves = new java.util.concurrent.atomic.AtomicInteger();
        var display = new java.util.concurrent.atomic.AtomicReference<ServerData>();
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public void saveData() {
                        assertTrue(Thread.holdsLock(publicationMonitor()));
                        assertEquals(getData().getInt("VotingPlugin.VoteParty.Total"),
                                display.get().getVotePartyDisplay().total());
                        saves.incrementAndGet();
                        super.saveData();
                    }
                })) {
            display.set(fixture.data);
            fixture.data.setInt("VoteParty.Total", 17);
            assertEquals(1, saves.get());
            var values = new java.util.ArrayList<>(java.util.List.of("FIRST", "SECOND"));
            fixture.data.setStringList("AutoCachePlaceholders", values);
            values.add("later");
            assertEquals(2, saves.get());
            assertEquals(java.util.List.of("FIRST", "SECOND"), fixture.data.getAutoCachedPlaceholder());
            fixture.data.updatePlaceholders();
            assertEquals(3, saves.get(), "a compound ordinary update saves only after its complete admitted mutation");
            assertEquals(java.util.List.of("first", "second"), fixture.data.getAutoCachedPlaceholder());
            assertTrue(fixture.data.getData().getBoolean("AutoCacheUpdated"));
            fixture.data.updatePlaceholders();
            assertEquals(3, saves.get(), "the established already-updated no-op must not save");
            fixture.data.setVotePartyExtraRequired(5);
            assertEquals(4, saves.get());
            assertEquals(5, fixture.data.getVotePartyDisplay().extraRequired());
            assertEquals(17, YamlConfiguration.loadConfiguration(fixture.backing.getdFile())
                    .getInt("VotingPlugin.VoteParty.Total"));
        }
    }

    @Test void wrapperIntRejectsBeforeMutationAfterUnconfirmedNativeCommit() throws Exception {
        assertWrapperRejectsAfterUnconfirmedCommit(data -> data.setInt("VoteParty.Total", 23));
    }

    @Test void wrapperListRejectsBeforeMutationAfterUnconfirmedNativeCommit() throws Exception {
        assertWrapperRejectsAfterUnconfirmedCommit(data -> data.setStringList("SyntheticList", java.util.List.of("entry")));
    }

    @Test void wrapperExtraRejectsBeforeMutationAfterUnconfirmedNativeCommit() throws Exception {
        assertWrapperRejectsAfterUnconfirmedCommit(data -> data.setVotePartyExtraRequired(9));
    }

    @Test void wrapperShopConversionRejectsBeforeMutationAfterUnconfirmedNativeCommit() throws Exception {
        assertWrapperRejectsAfterUnconfirmedCommit(data -> data.setShopConverted(true));
    }

    @Test void wrapperShopPurchaseRejectsBeforeMutationAfterUnconfirmedNativeCommit() throws Exception {
        assertWrapperRejectsAfterUnconfirmedCommit(data -> data.setVoteShopPurchases("synthetic", 4));
    }

    private void assertWrapperRejectsAfterUnconfirmedCommit(java.util.function.Consumer<ServerData> mutation)
            throws Exception {
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override protected void forceDirectory(Path path) throws java.io.IOException {
                        if (java.nio.file.Files.exists(getdFile().toPath()))
                            throw new java.io.IOException("Synthetic post-replacement directory-force failure");
                        super.forceDirectory(path);
                    }
                })) {
            var publication = fixture.exposed.mutateFinalData(candidate ->
                    candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            var failure = assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> publication.get(5, TimeUnit.SECONDS));
            assertInstanceOf(java.io.UncheckedIOException.class, failure.getCause());
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"),
                    "the real replacement became visible before durability remained unconfirmed");
            assertEquals(17, fixture.data.getVotePartyDisplay().total());
            var liveBefore = fixture.backing.getData().saveToString();
            var diskBefore = java.nio.file.Files.readAllBytes(fixture.backing.getdFile().toPath());
            var displayBefore = fixture.data.getVotePartyDisplay();
            assertThrows(IllegalStateException.class, () -> mutation.accept(fixture.data));
            assertEquals(liveBefore, fixture.backing.getData().saveToString(),
                    "SDK rejection must precede wrapper YAML mutation");
            assertEquals(displayBefore, fixture.data.getVotePartyDisplay(),
                    "a rejected mutation cannot advance the live projection");
            assertArrayEquals(diskBefore, java.nio.file.Files.readAllBytes(fixture.backing.getdFile().toPath()));
            fixture.fence.retire();
            assertThrows(IllegalStateException.class, () ->
                    ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void synchronousFinalOwnerThrowAfterCommitRefreshesLiveDisplayAndRetainsFailure() throws Exception {
        var primary = new IllegalStateException("Synthetic post-commit owner cleanup failure");
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public java.util.concurrent.CompletionStage<Void> mutateFinalData(
                            java.util.function.Consumer<org.bukkit.configuration.file.FileConfiguration> mutation) {
                        var candidate = new YamlConfiguration();
                        mutation.accept(candidate);
                        assertEquals(17, candidate.getInt("VotingPlugin.VoteParty.Total"));
                        getData().set("VotingPlugin.VoteParty.Total", 17);
                        throw primary;
                    }
                })) {
            var result = fixture.exposed.mutateFinalData(candidate ->
                    candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            assertSame(primary, assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> result.get(5, TimeUnit.SECONDS)).getCause());
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(17, fixture.data.getVotePartyDisplay().total(),
                    "a synchronous final-owner throw must still observe its actual committed YAML");
            fixture.fence.retire();
            assertThrows(IllegalStateException.class, () ->
                    ServerDataPublicationFence.beginGeneration(fixture.backing.getdFile().toPath()));
        }
    }

    @Test void synchronousFinalOwnerThrowBeforeCommitKeepsPredecessorDisplay() throws Exception {
        var primary = new IllegalArgumentException("Synthetic owner rejection before commit");
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public java.util.concurrent.CompletionStage<Void> mutateFinalData(
                            java.util.function.Consumer<org.bukkit.configuration.file.FileConfiguration> mutation) {
                        var candidate = new YamlConfiguration();
                        mutation.accept(candidate);
                        throw primary;
                    }
                })) {
            var result = fixture.exposed.mutateFinalData(candidate ->
                    candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
            assertSame(primary, assertThrows(java.util.concurrent.ExecutionException.class,
                    () -> result.get(5, TimeUnit.SECONDS)).getCause());
            assertEquals(7, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
            assertEquals(7, fixture.data.getVotePartyDisplay().total());
        }
    }

    @Test void synchronousFinalOwnerThrowKeepsPrimaryAndSuppressesObservationFailure() throws Exception {
        var primary = new IllegalArgumentException("Synthetic committed owner cleanup failure");
        var secondary = new IllegalStateException("Synthetic unavailable live observation");
        var unavailable = new AtomicBoolean();
        try (var schedulers = mockConstruction(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
                var fixture = new ProjectionFixture(plugin -> new com.bencodez.advancedcore.data.ServerData(plugin) {
                    @Override public org.bukkit.configuration.file.FileConfiguration getData() {
                        if (unavailable.get()) throw secondary;
                        return super.getData();
                    }
                    @Override public java.util.concurrent.CompletionStage<Void> mutateFinalData(
                            java.util.function.Consumer<org.bukkit.configuration.file.FileConfiguration> mutation) {
                        var candidate = new YamlConfiguration();
                        mutation.accept(candidate);
                        super.getData().set("VotingPlugin.VoteParty.Total", 17);
                        unavailable.set(true);
                        throw primary;
                    }
                })) {
            try {
                var result = fixture.exposed.mutateFinalData(candidate ->
                        candidate.set("VotingPlugin.VoteParty.Total", 17)).toCompletableFuture();
                assertSame(primary, assertThrows(java.util.concurrent.ExecutionException.class,
                        () -> result.get(5, TimeUnit.SECONDS)).getCause());
                assertTrue(java.util.Arrays.asList(primary.getSuppressed()).contains(secondary));
            } finally { unavailable.set(false); }
            assertEquals(17, fixture.backing.getData().getInt("VotingPlugin.VoteParty.Total"));
        }
    }

    private final class ProjectionFixture implements AutoCloseable {
        final com.bencodez.advancedcore.data.ServerData backing;
        final YamlConfiguration yaml = new YamlConfiguration();
        final ServerDataPublicationFence fence;
        final GenerationFencedServerDataFile exposed;
        final ServerData data;
        ProjectionFixture(java.util.function.Function<VotingPluginMain,
                com.bencodez.advancedcore.data.ServerData> factory) {
            var plugin = mock(VotingPluginMain.class);
            when(plugin.getDataFolder()).thenReturn(directory.toFile());
            when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
            backing = factory.apply(plugin);
            yaml.set("VotingPlugin.VoteParty.Total", 7);
            yaml.set("VotingPlugin.VotePartyExtraRequired", 3);
            backing.setData(yaml);
            fence = ServerDataPublicationFence.beginGeneration(directory.resolve("ServerData.yml"));
            exposed = new GenerationFencedServerDataFile(plugin, backing, fence);
            when(plugin.getServerDataPublicationFence()).thenReturn(fence);
            when(plugin.getServerDataFile()).thenReturn(exposed);
            data = new ServerData(plugin);
            when(plugin.getServerData()).thenReturn(data);
            data.refreshVotePartyDisplay();
        }
        @Override public void close() { fence.retire(); }
    }

    private static String value(PlaceHolders placeholders, String identifier) {
        return placeholders.getNonPlayerPlaceholders().stream().filter(p -> p.matches(identifier))
                .findFirst().orElseThrow().placeholderRequest(identifier);
    }
}
