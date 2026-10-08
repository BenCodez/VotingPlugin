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
        var m = new DateVoteMilestones(plugin); m.reload(); return m;
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
        when(plugin.getTimer()).thenReturn(timer);
        doAnswer(invocation -> { worker.add(invocation.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        try (var scheduler = mockStatic(com.bencodez.votingplugin.util.BukkitCompletionScheduler.class)) {
            scheduler.when(() -> com.bencodez.votingplugin.util.BukkitCompletionScheduler.run(eq(plugin), eq(playerEntity),
                    any(Runnable.class), any(Runnable.class), any(Runnable.class))).thenAnswer(invocation -> {
                        entity.add(invocation.getArgument(2)); return null;
                    });
            m.progress(playerEntity); entity.remove().run();
            for (int i = 0; i < 100; i++) { m.progress(playerEntity); entity.remove().run(); }
            assertEquals(1, worker.size());
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
    private long countPlayerRecords(Path directory) {
        try (var files = Files.list(directory)) {
            return files.filter(p -> !p.getFileName().toString().equals("definitions.properties")).count();
        } catch (java.io.IOException failure) { throw new AssertionError(failure); }
    }

}
