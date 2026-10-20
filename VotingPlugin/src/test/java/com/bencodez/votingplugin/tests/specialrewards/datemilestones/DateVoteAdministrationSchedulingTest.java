package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import com.bencodez.votingplugin.specialrewards.datemilestones.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones.AdminAction;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.ArrayDeque;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.RejectedExecutionException;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

class DateVoteAdministrationSchedulingTest {
    @TempDir Path root;
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final CommandSender console = mock(CommandSender.class);
    final Player player = mock(Player.class);
    final ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
    final ArrayDeque<Runnable> worker = new ArrayDeque<>(), effects = new ArrayDeque<>(), fallbacks = new ArrayDeque<>();
    final UUID storageId = UUID.randomUUID();
    final YamlConfiguration config = new YamlConfiguration();
    DateVoteMilestones manager;

    @BeforeEach void setup() throws Exception {
        config.loadFromString("""
                DateVoteMilestones:
                  festival_With_Underscores:
                    Enabled: true
                    Start: '2026-10-01T00:00:00'
                    End: '2026-11-01T00:00:00'
                    Timezone: UTC
                    Milestones:
                      starter:
                        At: [1, 3]
                        Rewards: {Messages: {Player: 'sample'}}
                """);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getBungeeSettings().getServer()).thenReturn("local");
        when(user.getJavaUUID()).thenReturn(storageId);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("Example")).thenReturn(user);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(storageId, false)).thenReturn(user);
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(timer);
        when(console.hasPermission(anyString())).thenReturn(true);
        when(player.hasPermission(anyString())).thenReturn(true);
        when(player.isOnline()).thenReturn(true);
        doAnswer(call -> { worker.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        manager = new DateVoteMilestones(plugin);
        when(plugin.getDateVoteMilestones()).thenReturn(manager);
        manager.reload();
    }
    private MockedStatic<BukkitCompletionScheduler> scheduling() {
        var scheduler = mockStatic(BukkitCompletionScheduler.class);
        scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), nullable(Player.class), any(Runnable.class), any(Runnable.class), any(Runnable.class)))
                .thenAnswer(call -> {
                    effects.add(call.getArgument(call.getArgument(1) == null ? 3 : 2));
                    fallbacks.add(call.getArgument(3)); return null;
                });
        return scheduler;
    }
    private void command(AdminAction action, String argument) {
        manager.administer(console, action, "Example", "festival_With_Underscores", argument);
        effects.remove().run();
        worker.remove().run();
        effects.remove().run();
    }
    @Test void settingAndStatusResolveOnWorkerAndReportOnlyConfirmedCompletion() {
        try (var scheduling = scheduling()) {
            manager.administer(console, AdminAction.SetVotes, "Example", "festival_With_Underscores", "3");
            assertTrue(worker.isEmpty()); effects.remove().run();
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(anyString());
            verify(console, never()).sendMessage(anyString());
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
            worker.remove().run(); verify(console, never()).sendMessage(anyString()); effects.remove().run();
            verify(console).sendMessage(contains("votes set to 3"));
            command(AdminAction.Status, null);
            verify(console).sendMessage(contains("votes=3"));
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        }
    }
    @Test void forceNamedEntryFencesEveryThresholdAndExplicitRepeatPreservesCounts() {
        try (var scheduling = scheduling()) {
            command(AdminAction.Force, "starter");
            verify(plugin.getRewardHandler(), times(2)).giveReward(eq(user), any(), anyString(), any());
            command(AdminAction.Force, "starter");
            verify(plugin.getRewardHandler(), times(2)).giveReward(eq(user), any(), anyString(), any());
            command(AdminAction.ForceNoLimits, "starter");
            verify(plugin.getRewardHandler(), times(4)).giveReward(eq(user), any(), anyString(), any());
            command(AdminAction.Status, null);
            verify(console).sendMessage(contains("votes=0"));
        }
    }
    @Test void failedRewardSubmissionLeavesReservationAndDoesNotReserveLaterSiblings() {
        var rewards = plugin.getRewardHandler();
        doThrow(new IllegalStateException("synthetic failure")).when(rewards).giveReward(any(), any(), anyString(), any());
        try (var scheduling = scheduling()) {
            command(AdminAction.Force, "starter");
            verify(console).sendMessage(contains("failed; inspect retained progress"));
            clearInvocations(plugin.getRewardHandler());
            command(AdminAction.ForceNoLimits, "1");
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
            command(AdminAction.Status, null);
            verify(console).sendMessage("Milestone starter at 1: pending review");
            verify(console).sendMessage("Milestone starter at 3: not reached");
        }
    }
    @Test void malformedSetUnknownEventAndRemoteOwnerNeverResolvePlayerOrCreateLedger() {
        try (var scheduling = scheduling()) {
            command(AdminAction.SetVotes, "4097");
            manager.administer(console, AdminAction.SetVotes, "Example", "unknown", "1");
            effects.remove().run(); worker.remove().run(); effects.remove().run();
            config.set("DateVoteMilestones.festival_With_Underscores.AccountingServer", "remote");
            when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true); manager.reload();
            command(AdminAction.SetVotes, "1");
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(anyString());
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        }
    }
    @Test void permissionLossReloadAndRetirementPreventStaleCommandsAndPlayerEffects() {
        try (var scheduling = scheduling()) {
            manager.administer(player, AdminAction.SetVotes, "Example", "festival_With_Underscores", "3");
            when(player.hasPermission(anyString())).thenReturn(false); effects.remove().run(); assertTrue(worker.isEmpty());
            when(player.hasPermission(anyString())).thenReturn(true);
            manager.administer(player, AdminAction.SetVotes, "Example", "festival_With_Underscores", "3");
            effects.remove().run(); manager.reload(); worker.remove().run(); assertTrue(effects.isEmpty());
            manager.administer(player, AdminAction.SetVotes, "Example", "festival_With_Underscores", "3");
            fallbacks.getLast().run(); assertTrue(worker.isEmpty()); effects.remove();
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        }
    }
    @Test void disabledProcessingAndRejectedWorkerDoNotClaimRewardSuccess() {
        try (var scheduling = scheduling()) {
            when(plugin.getOptions().isProcessRewards()).thenReturn(false);
            command(AdminAction.Force, "starter");
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(anyString());
            doThrow(new RejectedExecutionException()).when(timer).execute(any(Runnable.class));
            manager.administer(console, AdminAction.SetVotes, "Example", "festival_With_Underscores", "1");
            effects.remove().run();
            verify(console).sendMessage(contains("nothing was submitted"));
            assertFalse(Files.exists(root.resolve("date-vote-milestones")));
        }
    }
    @Test void placeholdersAreNonblockingCaseExactAndInvalidateAfterConfirmedSet() {
        try (var scheduling = scheduling()) {
            assertEquals("invalid", manager.placeholder(storageId, "votes_festival_with_underscores"));
            assertEquals("invalid", manager.placeholder(storageId, "award_2_festival_With_Underscores"));
            assertTrue(worker.isEmpty());
            assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
            assertEquals("loading", manager.placeholder(storageId, "next_festival_With_Underscores"));
            assertEquals(1, worker.size()); assertFalse(Files.exists(root.resolve("date-vote-milestones")));
            worker.remove().run();
            assertEquals("0", manager.placeholder(storageId, "votes_festival_With_Underscores"));
            assertEquals("1", manager.placeholder(storageId, "next_festival_With_Underscores"));
            command(AdminAction.SetVotes, "3");
            assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores")); worker.remove().run();
            assertEquals("3", manager.placeholder(storageId, "votes_festival_With_Underscores"));
            assertEquals("0", manager.placeholder(storageId, "next_festival_With_Underscores"));
            assertEquals("deferred", manager.placeholder(storageId, "award_3_festival_With_Underscores"));
        }
    }
    @Test void placeholderReloadRejectionAndCorruptionDoNotBecomeAuthoritativeZero() throws Exception {
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        manager.reload(); worker.remove().run();
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        Files.createDirectory(root.resolve("date-vote-milestones"));
        Files.writeString(root.resolve("date-vote-milestones/definitions.properties"), "");
        worker.remove().run(); assertEquals("unavailable", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        manager.reload(); doThrow(new RejectedExecutionException()).when(timer).execute(any(Runnable.class));
        assertEquals("unavailable", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        assertEquals("unavailable", manager.placeholder(storageId, "votes_festival_With_Underscores"));
    }
    @Test void pendingPlaceholderAndAdminWorkIsBounded() {
        for (int i = 0; i < 64; i++) assertEquals("loading", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        assertEquals("unavailable", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        assertEquals(64, worker.size()); worker.clear();
        try (var scheduling = scheduling()) {
            for (int i = 0; i < 65; i++) {
                manager.administer(console, AdminAction.Status, "Example", "festival_With_Underscores", null);
                effects.remove().run();
            }
            assertEquals(64, worker.size()); verify(console).sendMessage(contains("busy"));
        }
    }
    @Test void reloadRetainsOutstandingPlaceholderSlotsUntilTheirWorkersExit() {
        for (int i = 0; i < 64; i++)
            assertEquals("loading", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        for (int reload = 0; reload < 3; reload++) {
            manager.reload();
            assertEquals("unavailable", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
            assertEquals(64, worker.size());
        }
        worker.remove().run();
        assertFalse(Files.exists(root.resolve("date-vote-milestones")), "Stale queued work must not read the ledger");
        UUID fresh = UUID.randomUUID();
        assertEquals("loading", manager.placeholder(fresh, "votes_festival_With_Underscores"));
        assertEquals(64, worker.size());
        assertEquals("unavailable", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        while (worker.size() > 1) worker.remove().run();
        worker.remove().run();
        assertEquals("0", manager.placeholder(fresh, "votes_festival_With_Underscores"));
    }
    @Test void repeatedReloadDoesNotDuplicateTheSameOutstandingIdentity() {
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        for (int reload = 0; reload < 3; reload++) {
            manager.reload();
            assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
            assertEquals(1, worker.size());
        }
        worker.remove().run();
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores"));
        worker.remove().run();
        assertEquals("0", manager.placeholder(storageId, "votes_festival_With_Underscores"));
    }
    @Test void reloadWhileARefreshRunsRetainsItsAdmissionUntilFinally() throws Exception {
        var once = new java.util.concurrent.atomic.AtomicBoolean(true);
        var ledger = spy(new DateVoteLedger(root.resolve("date-vote-milestones")));
        doAnswer(call -> {
            if (once.getAndSet(false)) {
                manager.reload();
                assertEquals("unavailable", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
                assertEquals(63, worker.size());
            }
            return call.callRealMethod();
        }).when(ledger).progress(any(), any());
        var field = DateVoteMilestones.class.getDeclaredField("ledger"); field.setAccessible(true); field.set(manager, ledger);
        for (int i = 0; i < 64; i++)
            assertEquals("loading", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        worker.remove().run();
        assertEquals("loading", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores"));
        assertEquals(64, worker.size());
    }
    @Test void aVoteDuringPlaceholderReadCannotPublishTheOlderCount() throws Exception {
        when(plugin.getOptions().isProcessRewards()).thenReturn(false);
        var first = new java.util.concurrent.atomic.AtomicBoolean(true);
        var ledger = spy(new DateVoteLedger(root.resolve("date-vote-milestones")));
        doAnswer(call -> {
            var old = call.callRealMethod();
            if (first.getAndSet(false)) manager.accepted(user, "site", UUID.randomUUID(),
                    java.time.Instant.parse("2026-10-15T00:00:00Z").toEpochMilli(), true, false, false, false, false);
            return old;
        }).when(ledger).progress(any(), eq(storageId));
        var field = DateVoteMilestones.class.getDeclaredField("ledger"); field.setAccessible(true); field.set(manager, ledger);
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores")); worker.remove().run();
        assertEquals("loading", manager.placeholder(storageId, "votes_festival_With_Underscores")); worker.remove().run();
        assertEquals("1", manager.placeholder(storageId, "votes_festival_With_Underscores"));
    }
    @Test void completedPlaceholderCacheEvictsOldIdentitiesAndRemoteReadsDoNotQueueStorage() {
        UUID oldest = UUID.randomUUID();
        assertEquals("loading", manager.placeholder(oldest, "votes_festival_With_Underscores")); worker.remove().run();
        for (int i = 0; i < 256; i++) {
            assertEquals("loading", manager.placeholder(UUID.randomUUID(), "votes_festival_With_Underscores")); worker.remove().run();
        }
        assertEquals("loading", manager.placeholder(oldest, "votes_festival_With_Underscores"));
        config.set("DateVoteMilestones.festival_With_Underscores.AccountingServer", "remote");
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true); manager.reload(); worker.remove().run();
        assertEquals("remote", manager.placeholder(oldest, "votes_festival_With_Underscores"));
        assertTrue(worker.isEmpty());
        verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(anyString());
    }
    @Test void disabledEventsAndMilestonesCannotBeForcedAndUuidTargetsUseTheirExactIdentity() {
        try (var scheduling = scheduling()) {
            config.set("DateVoteMilestones.festival_With_Underscores.Enabled", false); manager.reload();
            command(AdminAction.ForceNoLimits, "starter");
            verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(anyString());
            config.set("DateVoteMilestones.festival_With_Underscores.Enabled", true);
            config.set("DateVoteMilestones.festival_With_Underscores.Milestones.starter.Enabled", false); manager.reload();
            command(AdminAction.ForceNoLimits, "starter");
            verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
            manager.administer(console, AdminAction.SetVotes, storageId.toString(), "festival_With_Underscores", "2");
            effects.remove().run(); worker.remove().run(); effects.remove().run();
            verify(plugin.getVotingPluginUserManager()).getVotingPluginUser(storageId, false);
            verify(console).sendMessage(contains("votes set to 2"));
        }
    }
    @Test void playerCompletionRemainsEntityOwnedAfterPermissionLossOrRetirement() {
        try (var scheduling = scheduling()) {
            manager.administer(player, AdminAction.SetVotes, "Example", "festival_With_Underscores", "2");
            effects.remove().run(); worker.remove().run();
            fallbacks.getLast().run(); verify(player, never()).sendMessage(anyString());
            when(player.hasPermission(anyString())).thenReturn(false); effects.remove().run();
            verify(player, never()).sendMessage(anyString());
            command(AdminAction.Status, null); verify(console).sendMessage(contains("votes=2"));
        }
    }
}
