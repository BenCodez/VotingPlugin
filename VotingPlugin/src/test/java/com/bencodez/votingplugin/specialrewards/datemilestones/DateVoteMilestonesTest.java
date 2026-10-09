package com.bencodez.votingplugin.specialrewards.datemilestones;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
class DateVoteMilestonesTest {
    @TempDir Path root;
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final YamlConfiguration config = new YamlConfiguration();
    final UUID player = UUID.randomUUID();
    final long time = java.time.Instant.parse("2026-10-15T00:00:00Z").toEpochMilli();
    private void event(String id, String owner) {
        String key = "DateVoteMilestones." + id + ".";
        config.set(key + "Enabled", true); config.set(key + "DisplayName", id);
        config.set(key + "Start", "2026-10-01T00:00:00"); config.set(key + "End", "2026-11-01T00:00:00");
        config.set(key + "Timezone", "UTC"); config.set(key + "AccountingServer", owner);
        config.set(key + "Milestones.1.Rewards.Messages.Player", "Thanks");
    }
    private DateVoteMilestones manager() {
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getBungeeSettings().getServer()).thenReturn("owner");
        when(user.getJavaUUID()).thenReturn(player);
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        var m = new DateVoteMilestones(plugin); m.reload(); return m;
    }
    @Test void failedOccurrencePublicationIsReportedWithoutCountingOrRepeatingAnAward() throws Exception {
        event("october", "owner"); var manager = manager();
        assertTrue(manager.accepted(user, "a", UUID.randomUUID(), time, true, true, true, false, false));
        UUID retry = UUID.randomUUID();
        try (var publication = mockStatic(com.bencodez.votingplugin.util.DurableFiles.class)) {
            publication.when(() -> com.bencodez.votingplugin.util.DurableFiles.publishStagedFile(any(Path.class), any(Path.class)))
                    .thenThrow(new java.io.IOException("fixture occurrence unavailable"));
            assertFalse(manager.accepted(user, "a", retry, time + 1, true, true, true, false, false));
        }
        var definition = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        assertEquals(1, new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player).votes());
        assertTrue(manager.accepted(user, "a", retry, time + 1, true, true, true, false, false));
        assertTrue(manager.accepted(user, "a", retry, time + 1, true, true, true, false, false));
        assertEquals(2, new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player).votes());
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), anyString(), any());
    }
    @Test void disabledRewardProcessingCountsWithoutSubmittingOrLosingThePendingThreshold() throws Exception {
        event("october", ""); var m = manager();
        when(plugin.getOptions().isProcessRewards()).thenReturn(false);
        UUID occurrence = UUID.randomUUID();
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        var definition = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(1, progress.votes()); assertEquals(java.util.Set.of(1), progress.deferredAwards());
        assertTrue(progress.reservedAwards().isEmpty()); assertTrue(progress.submittedAwards().isEmpty());
    }
    @Test void confirmedDeferredAwardsResumeOnceAfterRestartAndRewardProcessingIsEnabled() throws Exception {
        event("october", ""); var m = manager();
        when(plugin.getOptions().isProcessRewards()).thenReturn(false);
        UUID occurrence = UUID.randomUUID();
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        m = manager(); // Fresh manager and on-disk ledger; manager enables processing.
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        m.accepted(user, "a", UUID.randomUUID(), time + 1, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(rewardPath("october")), any());
        var definition = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(2, progress.votes()); assertEquals(java.util.Set.of(1), progress.submittedAwards());
        assertTrue(progress.deferredAwards().isEmpty());
    }
    private com.bencodez.votingplugin.core.datemilestones.DateVoteEvent deferredPair(DateVoteMilestones manager) {
        when(plugin.getOptions().isProcessRewards()).thenReturn(false);
        manager.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        manager.accepted(user, "a", UUID.randomUUID(), time + 1, true, false, false, false, false);
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        return DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
    }
    @Test void earlierSubmissionFailureDoesNotReserveAnUnattemptedLaterThreshold() throws Exception {
        event("october", ""); config.set("DateVoteMilestones.october.Milestones.2.Rewards.Messages.Player", "Second");
        var manager = manager(); var definition = deferredPair(manager);
        var first = DateVoteMilestones.path(definition, 1); var second = DateVoteMilestones.path(definition, 2);
        var handler = plugin.getRewardHandler();
        doThrow(new IllegalStateException("fixture uncertain submission")).when(handler)
                .giveReward(eq(user), any(), eq(first), any());
        UUID delivery = UUID.randomUUID();
        manager.accepted(user, "a", delivery, time + 2, true, false, false, false, false);
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(java.util.Set.of(1), progress.reservedAwards());
        assertEquals(java.util.Set.of(2), progress.deferredAwards());
        verify(plugin.getRewardHandler(), never()).giveReward(eq(user), any(), eq(second), any());
        manager = manager(); // Restart and replay must retain the earlier uncertain reservation.
        manager.accepted(user, "a", delivery, time + 2, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(first), any());
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(second), any());
        progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(3, progress.votes()); assertEquals(java.util.Set.of(2), progress.submittedAwards());
    }
    @Test void acknowledgementFailureDoesNotReserveAnUnattemptedLaterThreshold() throws Exception {
        event("october", ""); config.set("DateVoteMilestones.october.Milestones.2.Rewards.Messages.Player", "Second");
        var manager = manager(); var definition = deferredPair(manager);
        try (var publication = mockStatic(com.bencodez.votingplugin.util.DurableFiles.class)) {
            publication.when(() -> com.bencodez.votingplugin.util.DurableFiles.publishStagedFile(any(Path.class), any(Path.class)))
                    .thenAnswer(invocation -> {
                        Path staged = invocation.getArgument(0);
                        if (Files.readString(staged).contains("award.1=SUBMITTED")) throw new java.io.IOException("fixture acknowledgement failure");
                        return invocation.callRealMethod();
                    });
            manager.accepted(user, "a", UUID.randomUUID(), time + 2, true, false, false, false, false);
        }
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(java.util.Set.of(1), progress.reservedAwards()); assertEquals(java.util.Set.of(2), progress.deferredAwards());
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 1)), any());
        verify(plugin.getRewardHandler(), never()).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 2)), any());
        manager = manager(); manager.accepted(user, "a", UUID.randomUUID(), time + 3, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 1)), any());
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 2)), any());
    }
    @Test void turningRewardsOffBetweenSubmissionsKeepsTheLaterThresholdDeferred() throws Exception {
        event("october", ""); config.set("DateVoteMilestones.october.Milestones.2.Rewards.Messages.Player", "Second");
        var manager = manager(); var definition = deferredPair(manager);
        var handler = plugin.getRewardHandler();
        doAnswer(invocation -> { when(plugin.getOptions().isProcessRewards()).thenReturn(false); return null; })
                .when(handler).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 1)), any());
        manager.accepted(user, "a", UUID.randomUUID(), time + 2, true, false, false, false, false);
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(java.util.Set.of(1), progress.submittedAwards()); assertEquals(java.util.Set.of(2), progress.deferredAwards());
        assertTrue(progress.reservedAwards().isEmpty());
        verify(plugin.getRewardHandler(), never()).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 2)), any());
        manager = manager(); manager.accepted(user, "a", UUID.randomUUID(), time + 3, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(DateVoteMilestones.path(definition, 2)), any());
    }
    @Test void invalidTimezoneStopsAccountingButPreservesQueuedRewardResolutionOnReload() {
        event("october", ""); var m = manager(); String alias = rewardPath("october");
        config.set("DateVoteMilestones.october.Timezone", "No/SuchZone"); clearInvocations(plugin);
        m.reload();
        var handle = org.mockito.ArgumentCaptor.forClass(com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward.class);
        verify(plugin).addDirectlyDefinedRewards(handle.capture());
        assertEquals(alias, handle.getValue().getPath());
        assertEquals("Thanks", handle.getValue().getFileData().getString(alias + ".Messages.Player"));
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
    }
    @Test void accountingInvalidAtStartupStillRegistersExistingCanonicalRewardSections() {
        event("october", ""); String alias = rewardPath("october");
        config.set("DateVoteMilestones.october.End", "not-a-date"); var m = manager();
        var handle = org.mockito.ArgumentCaptor.forClass(com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward.class);
        verify(plugin).addDirectlyDefinedRewards(handle.capture());
        assertEquals(alias, handle.getValue().getPath());
        assertEquals("Thanks", handle.getValue().getFileData().getString(alias + ".Messages.Player"));
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
    }
    @Test void configuredRewardsUseExistingHandlerOnceAcrossReplayRestartAndRename() {
        event("october", ""); var m = manager(); UUID occurrence = UUID.randomUUID();
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        config.set("DateVoteMilestones.october.DisplayName", "Renamed");
        m = manager(); m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq(rewardPath("october")), any());
        verify(user, never()).addTotal();
        verify(user, never()).playerVote(any(), anyBoolean(), anyBoolean());
    }
    private String rewardPath(String id) {
        return DateVoteMilestones.path(DateVoteMilestones.parse(id, config.getConfigurationSection("DateVoteMilestones." + id)), 1);
    }
    @Test void disablingStopsAccountingButKeepsQueuedRewardReferencesResolvable() {
        event("october", ""); var m = manager();
        var handles = org.mockito.ArgumentCaptor.forClass(com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward.class);
        verify(plugin).addDirectlyDefinedRewards(handles.capture());
        String stable = handles.getValue().getPath();
        config.set("DateVoteMilestones.october.Enabled", false); clearInvocations(plugin);
        m.reload(); verify(plugin).addDirectlyDefinedRewards(handles.capture());
        assertEquals(stable, handles.getValue().getPath());
        assertEquals("Thanks", handles.getValue().getFileData().getString(stable + ".Messages.Player"));
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
    }
    @Test void caseDistinctEventsHaveDistinctRewardRegistryHandlesAndActualConfigurations() {
        event("Festival", ""); event("festival", "");
        config.set("DateVoteMilestones.Festival.Milestones.1.Rewards.Messages.Player", "First");
        config.set("DateVoteMilestones.festival.Milestones.1.Rewards.Messages.Player", "Second");
        var m = manager();
        var handles = org.mockito.ArgumentCaptor.forClass(com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward.class);
        verify(plugin, times(2)).addDirectlyDefinedRewards(handles.capture());
        var registry = new com.bencodez.advancedcore.api.rewards.RewardRegistry(plugin);
        handles.getAllValues().forEach(registry::addDirectlyDefined);
        assertEquals(2, registry.getDirectlyDefinedRewards().size());
        var first = registry.getDirectlyDefined(rewardPath("Festival"));
        var second = registry.getDirectlyDefined(rewardPath("festival"));
        assertNotSame(first, second);
        assertEquals("First", first.getFileData().getString(first.getPath() + ".Messages.Player"));
        assertEquals("Second", second.getFileData().getString(second.getPath() + ".Messages.Player"));
        UUID occurrence = UUID.randomUUID();
        m.accepted(user, "a", occurrence, time, true, false, false, false, false);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), eq(first.getPath()), any());
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), eq(second.getPath()), any());
    }
    @Test void fakeLegacyTargetedAndNonOwnerDeliveriesNeverCreateProgress() {
        event("october", "owner"); var m = manager();
        m.accepted(user, "a", UUID.randomUUID(), time, false, true, true, false, false);
        m.accepted(user, "a", UUID.randomUUID(), time, true, true, false, false, false);
        m.accepted(user, "a", UUID.randomUUID(), time, true, true, true, true, false);
        when(plugin.getBungeeSettings().getServer()).thenReturn("other");
        m.accepted(user, "a", UUID.randomUUID(), time, true, true, true, false, false);
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
    }
    @Test void configuredNetworkRejectsLocalVoteWithoutCanonicalNetworkMetadata() {
        event("october", "owner"); var m = manager();
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
    }
    @Test void overlappingWindowsUseSameOriginalOccurrenceIndependently() {
        event("october", "owner"); event("festival", "owner"); var m = manager(); UUID occurrence = UUID.randomUUID();
        m.accepted(user, "a", occurrence, time, true, true, true, false, false);
        verify(plugin.getRewardHandler(), times(2)).giveReward(eq(user), any(), anyString(), any());
    }
    @Test void failedRewardAdmissionStaysReservedAndNeverAutomaticallyRepeats() throws Exception {
        event("october", ""); var m = manager();
        var handler = plugin.getRewardHandler();
        doThrow(new IllegalStateException("fixture submission failure")).when(handler).giveReward(any(), any(), anyString(), any());
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        m = manager(); m.accepted(user, "a", UUID.randomUUID(), time + 1, true, false, false, false, false);
        verify(plugin.getRewardHandler(), times(1)).giveReward(any(), any(), anyString(), any());
        var definition = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        var progress = new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player);
        assertEquals(2, progress.votes()); assertEquals(java.util.Set.of(1), progress.reservedAwards());
    }
    @Test void malformedDefinitionIsDisabledWithoutBreakingOtherDefinitions() {
        event("broken", ""); event("valid", ""); config.set("DateVoteMilestones.broken.Timezone", "No/SuchZone");
        var m = manager(); m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), eq(rewardPath("valid")), any());
    }
    @Test void milestoneMustHaveCanonicalThresholdAndActualRewardSection() {
        event("a", ""); config.set("DateVoteMilestones.a.Milestones.01.Rewards.Messages.Player", "bad");
        assertThrows(IllegalArgumentException.class, () -> DateVoteMilestones.parse("a", config.getConfigurationSection("DateVoteMilestones.a")));
    }
    @Test void progressIsWorkerScheduledBoundedReadOnlyAndReloadFenced() {
        event("october", ""); var m = manager();
        UUID storageId = UUID.randomUUID();
        when(user.getJavaUUID()).thenReturn(storageId);
        m.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
        clearInvocations(plugin.getRewardHandler());
        var playerEntity = mock(org.bukkit.entity.Player.class);
        when(playerEntity.getUniqueId()).thenReturn(player);
        when(playerEntity.getName()).thenReturn("Player");
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(player, "Player")).thenReturn(user);
        when(playerEntity.isOnline()).thenReturn(true);
        when(playerEntity.hasPermission(anyString())).thenReturn(true);
        when(plugin.isEnabled()).thenReturn(true);
        java.util.Queue<Runnable> worker = new java.util.ArrayDeque<>(), entity = new java.util.ArrayDeque<>();
        var timer = mock(java.util.concurrent.ScheduledExecutorService.class);
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
        doAnswer(invocation -> { worker.add(invocation.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        try (var scheduler = mockStatic(com.bencodez.votingplugin.util.BukkitCompletionScheduler.class)) {
            scheduler.when(() -> com.bencodez.votingplugin.util.BukkitCompletionScheduler.run(eq(plugin), eq(playerEntity),
                    any(Runnable.class), any(Runnable.class), any(Runnable.class))).thenAnswer(invocation -> {
                        entity.add(invocation.getArgument(2)); return null;
                    });
            worker.add(() -> when(user.getJavaUUID()).thenReturn(storageId));
            when(user.getJavaUUID()).thenReturn(player); // Earlier ordered mutation has not yet run.
            m.progress(playerEntity); entity.remove().run();
            for (int i = 0; i < 100; i++) { m.progress(playerEntity); entity.remove().run(); }
            assertEquals(2, worker.size());
            verify(plugin, never()).getTimer();
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
            worker.remove().run(); // The prior mutation must precede the progress lookup.
            verify(playerEntity, never()).sendMessage(startsWith("october ("));
            worker.remove().run(); assertEquals(1, entity.size());
            entity.remove().run(); verify(playerEntity).sendMessage(contains(": 1 votes;"));
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
            assertEquals(1, countPlayerRecords(root.resolve("date-vote-milestones")));
            clearInvocations(playerEntity);
            m.progress(playerEntity); entity.remove().run(); worker.remove().run();
            m.reload(); entity.remove().run();
            verify(playerEntity, never()).sendMessage(startsWith("october ("));
        }
    }
    @Test void malformedSiteFilterDisablesOnlyThatDefinitionAndPreservesQueuedRewardHandles() {
        for (Object malformed : new Object[] {"SiteA", 42, java.util.Map.of("SiteA", true), java.util.List.of("SiteA", 42)}) {
            event("badfilter", ""); event("validfilter", "");
            config.set("DateVoteMilestones.badfilter.VoteSites", malformed);
            var manager = manager();
            assertThrows(IllegalArgumentException.class, () -> DateVoteMilestones.parse("badfilter", config.getConfigurationSection("DateVoteMilestones.badfilter")));
            var handles = org.mockito.ArgumentCaptor.forClass(com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward.class);
            verify(plugin, atLeastOnce()).addDirectlyDefinedRewards(handles.capture());
            assertTrue(handles.getAllValues().stream().anyMatch(h -> h.getPath().contains(com.bencodez.votingplugin.core.datemilestones.DateVoteEvent.fileId("badfilter"))));
            manager.accepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false);
            verify(plugin.getRewardHandler(), never()).giveReward(eq(user), any(), contains(com.bencodez.votingplugin.core.datemilestones.DateVoteEvent.fileId("badfilter")), any());
            verify(plugin.getRewardHandler(), atLeastOnce()).giveReward(eq(user), any(), eq(rewardPath("validfilter")), any());
        }
    }
    @Test void absentEmptyAndExplicitStringSiteFiltersKeepTheirDocumentedScope() {
        event("filters", ""); var section = config.getConfigurationSection("DateVoteMilestones.filters");
        assertTrue(DateVoteMilestones.parse("filters", section).matches(time, "a", true, false, ""));
        section.set("VoteSites", java.util.List.of());
        assertTrue(DateVoteMilestones.parse("filters", section).matches(time, "b", true, false, ""));
        section.set("VoteSites", java.util.List.of("a"));
        var filtered = DateVoteMilestones.parse("filters", section);
        assertTrue(filtered.matches(time, "a", true, false, "")); assertFalse(filtered.matches(time, "b", true, false, ""));
    }

    private long countPlayerRecords(Path directory) {
        try (var files = Files.list(directory)) {
            return files.filter(p -> !p.getFileName().toString().equals("definitions.properties")).count();
        } catch (java.io.IOException failure) { throw new AssertionError(failure); }
    }

    @Test void ownerEventAccountingIsBoundedDeferredAndPreservesCanonicalDeduplication() throws Exception {
        event("october", ""); var m = manager(); when(plugin.isEnabled()).thenReturn(true);
        var queue = new java.util.ArrayDeque<Runnable>();
        var timer = mock(java.util.concurrent.ScheduledExecutorService.class);
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
        doAnswer(call -> { queue.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        UUID id = UUID.randomUUID();
        for (int i = 0; i < 65; i++) assertFalse(m.deferAccepted(user, "a", id, time, true, false, false, false, false));
        assertEquals(64, queue.size()); assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        while (!queue.isEmpty()) queue.remove().run();
        var definition = DateVoteMilestones.parse("october", config.getConfigurationSection("DateVoteMilestones.october"));
        assertEquals(1, new DateVoteLedger(root.resolve("date-vote-milestones")).progress(definition, player).votes());
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), anyString(), any());
        assertFalse(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false));
        assertEquals(1, queue.size());
    }
    @Test void deferredOwnerAccountingDoesNotCrossReloadOrDisableAndRejectsWithoutDisk() {
        for (boolean reload : new boolean[] { false, true }) {
            event("october", ""); var m = manager(); when(plugin.isEnabled()).thenReturn(true);
            var queue = new java.util.ArrayDeque<Runnable>();
            var timer = mock(java.util.concurrent.ScheduledExecutorService.class);
            when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
            doAnswer(call -> { queue.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            assertFalse(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false));
            if (reload) m.reload(); else when(plugin.isEnabled()).thenReturn(false);
            queue.remove().run(); assertFalse(Files.exists(root.resolve("date-vote-milestones")));
            doThrow(new java.util.concurrent.RejectedExecutionException("fixture retired")).when(timer).execute(any(Runnable.class));
            assertFalse(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false));
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        }
    }
    @Test void disabledUnmatchedFakeAndUnidentifiedProxyEventsNeedNoOwnerHandoff() {
        event("october", ""); var m = manager();
        assertTrue(m.deferAccepted(user, "a", UUID.randomUUID(), time, false, false, false, false, false));
        assertTrue(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, true, false, false, false));
        assertTrue(m.deferAccepted(user, "a", UUID.randomUUID(), 1L, true, false, false, false, false));
        config.set("DateVoteMilestones.october.Enabled", false); m.reload();
        assertTrue(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false));
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        verify(plugin.getUserManager().getDataManager().getTimer(), never()).execute(any(Runnable.class));
    }
    @Test void absentDateDefinitionsNeedNoOwnerThreadHandoffOrQuarantine() {
        var m = manager(); assertTrue(m.deferAccepted(user, "a", UUID.randomUUID(), time, true, false, false, false, false));
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        verify(plugin.getUserManager().getDataManager().getTimer(), never()).execute(any(Runnable.class));
    }
}
