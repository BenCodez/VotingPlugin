package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import com.bencodez.votingplugin.specialrewards.datemilestones.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.commands.gui.player.DateVoteEventsMenu;
import com.bencodez.votingplugin.core.datemilestones.DateVoteProgressSnapshot;

class DateVoteProgressSchedulingTest {
    @TempDir Path root;
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final Player player = mock(Player.class);
    final ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
    final ArrayDeque<Runnable> worker = new ArrayDeque<>(), entity = new ArrayDeque<>(), fallback = new ArrayDeque<>();
    final ArrayList<DateVoteProgressSnapshot> shown = new ArrayList<>();
    final UUID playerId = UUID.randomUUID(), storageId = UUID.randomUUID();
    final long occurred = java.time.Instant.parse("2026-10-15T00:00:00Z").toEpochMilli();
    final YamlConfiguration config = new YamlConfiguration();

    private DateVoteMilestones manager(String owner) throws Exception {
        config.loadFromString("""
                DateVoteMilestones:
                  festival:
                    Enabled: true
                    DisplayName: 'Fête 🎉'
                    Start: '2026-10-01T00:00:00'
                    End: '2026-11-01T00:00:00'
                    Timezone: UTC
                    Milestones:
                      first:
                        At: 1
                        Rewards:
                          Messages: {Player: 'Literal %player% {javascript:data}'}
                          Commands: ['say private reward arguments']
                """);
        config.set("DateVoteMilestones.festival.AccountingServer", owner);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getBungeeSettings().getServer()).thenReturn("local");
        when(player.getUniqueId()).thenReturn(playerId); when(player.getName()).thenReturn("Player");
        when(player.isOnline()).thenReturn(true); when(player.hasPermission(anyString())).thenReturn(true);
        when(user.getJavaUUID()).thenReturn(storageId);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(playerId, "Player")).thenReturn(user);
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
        doAnswer(call -> { worker.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        var manager = new DateVoteMilestones(plugin); when(plugin.getDateVoteMilestones()).thenReturn(manager); manager.reload(); return manager;
    }
    private MockedStatic<BukkitCompletionScheduler> scheduling() {
        var scheduler = mockStatic(BukkitCompletionScheduler.class);
        scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), eq(player), any(Runnable.class), any(Runnable.class), any(Runnable.class)))
                .thenAnswer(call -> { entity.add(call.getArgument(2)); fallback.add(call.getArgument(3)); return null; });
        return scheduler;
    }
    private void request(DateVoteMilestones manager) {
        DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> { assertTrue(current.getAsBoolean()); shown.add(snapshot); });
        entity.remove().run(); worker.remove().run(); entity.remove().run();
    }
    @Test void publicEntryPointOpensMenuOnlyAfterWorkerAndEntityHandoffWithoutSubmittingRewards() throws Exception {
        var manager = manager(""); manager.accepted(user, "site", UUID.randomUUID(), occurred, true, false, false, false, false);
        try (var scheduler = scheduling(); var menu = mockStatic(DateVoteEventsMenu.class)) {
            manager.progress(player); assertTrue(worker.isEmpty()); entity.remove().run();
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
            menu.verifyNoInteractions(); worker.remove().run(); menu.verifyNoInteractions(); entity.remove().run();
            menu.verify(() -> DateVoteEventsMenu.open(eq(plugin), eq(player), argThat(snapshot ->
                    snapshot.events().size() == 1 && snapshot.events().getFirst().votes() == 1
                    && snapshot.events().getFirst().deferred().contains(1)
                    && snapshot.events().getFirst().milestones().getFirst().rewardTypes().contains("Commands")), any()));
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        }
    }
    @Test void remoteOwnerNeverReadsTheLocalLedgerOrFabricatesLocalProgress() throws Exception {
        var manager = manager("remote"); when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        try (var scheduler = scheduling()) { request(manager); }
        assertEquals(DateVoteProgressSnapshot.Availability.REMOTE, shown.getFirst().events().getFirst().availability());
        assertEquals("remote", shown.getFirst().events().getFirst().definition().accountingServer());
        verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(any(UUID.class), anyString());
        assertFalse(Files.exists(root.resolve("date-vote-milestones")));
    }
    @Test void disabledEmptyAndUnavailableRemainDistinctAndReadsNeverCreateAccountingState() throws Exception {
        var manager = manager(""); config.set("DateVoteMilestones.festival.Enabled", false); manager.reload();
        try (var scheduler = scheduling()) {
            request(manager); assertEquals("disabled", shown.getFirst().events().getFirst().status(occurred));
            assertEquals(DateVoteProgressSnapshot.Availability.OWNED, shown.getFirst().events().getFirst().availability());
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
            Files.createDirectory(root.resolve("date-vote-milestones")); Files.writeString(root.resolve("date-vote-milestones/definitions.properties"), "");
            request(manager); assertEquals(DateVoteProgressSnapshot.Availability.UNAVAILABLE, shown.get(1).events().getFirst().availability());
            config.set("DateVoteMilestones", null); manager.reload(); request(manager); assertTrue(shown.get(2).events().isEmpty());
        }
    }
    @Test void permissionLossOfflineReloadAndRetirementSuppressPlayerEffects() throws Exception {
        var manager = manager("");
        try (var scheduler = scheduling()) {
            when(player.hasPermission(anyString())).thenReturn(false);
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> shown.add(snapshot)); entity.remove().run(); assertTrue(worker.isEmpty());
            when(player.hasPermission(anyString())).thenReturn(true);
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> shown.add(snapshot)); entity.remove().run(); worker.remove().run();
            when(player.isOnline()).thenReturn(false); entity.remove().run(); assertTrue(shown.isEmpty());
            when(player.isOnline()).thenReturn(true);
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> shown.add(snapshot)); entity.remove().run(); worker.remove().run();
            manager.reload(); entity.remove().run(); assertTrue(shown.isEmpty());
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> shown.add(snapshot)); entity.remove().run(); worker.remove().run();
            fallback.getLast().run(); assertTrue(shown.isEmpty());
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        }
    }
    @Test void managerReplacementInvalidatesAnAlreadyDeliveredMenuSnapshot() throws Exception {
        var manager = manager("");
        var guards = new ArrayList<java.util.function.BooleanSupplier>();
        try (var scheduler = scheduling()) {
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> guards.add(current));
            entity.remove().run(); worker.remove().run(); entity.remove().run();
            assertEquals(1, guards.size());
            assertTrue(guards.getFirst().getAsBoolean());
            when(plugin.getDateVoteMilestones()).thenReturn(new DateVoteMilestones(plugin));
            assertFalse(guards.getFirst().getAsBoolean());
        }
    }
    @Test void rejectedStorageAdmissionReleasesTheBusyFenceAndReportsUnavailableOnPlayerThread() throws Exception {
        var manager = manager(""); doThrow(new java.util.concurrent.RejectedExecutionException()).when(timer).execute(any(Runnable.class));
        try (var scheduler = scheduling()) {
            DateVoteMilestonesAccess.progress(manager, player, (snapshot, current) -> shown.add(snapshot)); entity.remove().run();
            verify(player).sendMessage("Date voting progress is temporarily unavailable.");
            doAnswer(call -> { worker.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            request(manager); assertEquals(1, shown.size());
        }
    }
}
