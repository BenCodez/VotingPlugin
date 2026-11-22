package com.bencodez.votingplugin.tests.specialrewards;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import org.bukkit.Server;
import org.bukkit.command.SimpleCommandMap;
import org.bukkit.plugin.SimplePluginManager;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.AdvancedCorePlugin.UserStorageOwner;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.specialrewards.voteparty.VoteParty;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VotePartyStoredRecipientFailureTest {
    private static final UUID ID = UUID.fromString("9345fd57-4cc0-4492-ad39-0bbea6ca7525");
    private final ArrayDeque<Runnable> storage = new ArrayDeque<>();
    private final ArrayDeque<Runnable> voteTasks = new ArrayDeque<>();
    private final List<java.util.concurrent.ScheduledFuture<?>> storageWakeups = new ArrayList<>();
    private final List<java.util.concurrent.ScheduledFuture<?>> voteWakeups = new ArrayList<>();
    private final AtomicReference<UserStorageOwner> currentOwner = new AtomicReference<>();
    private final ArrayDeque<Runnable> platform = new ArrayDeque<>();
    private final AtomicInteger votes = new AtomicInteger(2);
    private volatile boolean worker;
    private PluginManager events;
    private final java.util.concurrent.atomic.AtomicBoolean lastParty = new java.util.concurrent.atomic.AtomicBoolean();
    private VotingPluginMain plugin;
    private UserDataManager manager;
    private ScheduledExecutorService persistence;
    private com.bencodez.simpleapi.folialib.impl.PlatformScheduler commitScheduler;
    private final List<com.bencodez.simpleapi.folialib.wrapper.task.WrappedTask> commitWakeups = new ArrayList<>();
    private MySQL mysql;
    private SpecialRewardsConfig config;
    private ServerData serverData;
    private YamlConfiguration state;
    private YamlConfiguration rewards;
    private RewardHandler apiRewards;
    private VotingPluginUser user;
    private VoteParty party;
    private com.bencodez.advancedcore.api.user.UserManager coreUsers;
    private MockedStatic<Bukkit> bukkit;
    private MockedStatic<MiscUtils> misc;
    private Consumer<Object> firstSuccess;
    private Consumer<Throwable> firstFailure;

    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        plugin = mock(VotingPluginMain.class);
        coreUsers = mock(com.bencodez.advancedcore.api.user.UserManager.class);
        manager = mock(UserDataManager.class);
        mysql = mock(MySQL.class);
        config = mock(SpecialRewardsConfig.class);
        serverData = mock(ServerData.class);
        user = mock(VotingPluginUser.class);
        UserManager votingUsers = mock(UserManager.class);
        apiRewards = mock(RewardHandler.class);
        state = new YamlConfiguration();
        state.set("VoteParty.Total", 3);
        state.set("VoteParty.Voted", List.of(ID.toString()));
        rewards = new YamlConfiguration();
        when(plugin.getUserManager()).thenReturn(coreUsers);
        when(coreUsers.getDataManager()).thenReturn(manager);
        when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
        when(plugin.getSpecialRewardsConfig()).thenReturn(config);
        when(plugin.getServerData()).thenReturn(serverData);
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
        when(plugin.getRewardHandler()).thenReturn(apiRewards);
        when(plugin.getPlaceholders()).thenReturn(mock(com.bencodez.votingplugin.placeholders.PlaceHolders.class));
        doAnswer(c -> { votes.set(c.getArgument(0)); return null; }).when(user).setVotePartyVotes(anyInt());
        when(config.isVotePartyEnabled()).thenReturn(true);
        when(config.isVotePartyGiveAllPlayers()).thenReturn(true);
        when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(false);
        when(config.getVotePartyUserVotesRequired()).thenReturn(2);
        when(config.getVotePartyVotesRequired()).thenReturn(3);
        when(config.isVotePartyResetCount()).thenReturn(true);
        when(config.getData()).thenReturn(rewards);
        when(config.getVotePartyRewardsPath()).thenReturn("VoteParty.Rewards");
        when(serverData.getData()).thenReturn(state);
        when(serverData.isLastVotePartySameDay()).thenAnswer(c -> lastParty.get());
        doAnswer(c -> { lastParty.set(true); return null; }).when(serverData).updateLastVoteParty();
        doAnswer(c -> { state.set(c.getArgument(0), c.getArgument(1)); return null; })
                .when(serverData).setStringList(anyString(), anyList());
        doAnswer(c -> { state.set(c.getArgument(0), c.getArgument(1)); return null; })
                .when(serverData).setInt(anyString(), anyInt());
        when(manager.hasSharedSqlBackend()).thenReturn(true);
        when(manager.isPlatformOwnedThread()).thenAnswer(c -> !worker);
        when(manager.getPublishedCache(ID)).thenReturn(mock(UserDataCache.class));
        UserStorageOwner owner = new UserStorageOwner(UserStorage.MYSQL, mysql, null);
        currentOwner.set(owner);
        when(manager.sharedNativeUserStorageOwner()).thenAnswer(c -> currentOwner.get());
        when(manager.withSharedNativeUserStorage(any())).thenAnswer(c -> {
            assertTrue(worker, "native storage admission must stay off the platform");
            return ((Function<UserStorageOwner, ?>) c.getArgument(0)).apply(currentOwner.get());
        });
        org.mockito.stubbing.Answer<Boolean> deferResult = c -> {
            if (worker && c.getArguments().length == 4) return false;
            Supplier<Object> work = c.getArgument(0);
            Consumer<Object> success = c.getArgument(1);
            Consumer<Throwable> failure = c.getArgument(2);
            if (firstSuccess == null) { firstSuccess = success; firstFailure = failure; }
            storage.add(() -> {
                try { Object result = work.get(); platform.add(() -> success.accept(result)); }
                catch (RuntimeException problem) { platform.add(() -> failure.accept(problem)); }
            });
            return true;
        };
        when(manager.deferSharedStorageResult(any(), any(), any(), nullable(Entity.class))).thenAnswer(deferResult);
        when(manager.deferSharedStorageResultFromPlatform(any(), any(), any())).thenAnswer(deferResult);
        when(manager.deferSharedStorageWork(any())).thenAnswer(c -> {
            if (worker) return false;
            storage.add(c.getArgument(0));
            return true;
        });
        doAnswer(c -> { votes.set(0); return null; }).when(coreUsers)
                .removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        when(user.getJavaUUID()).thenReturn(ID);
        when(user.getVotePartyVotes()).thenAnswer(c -> votes.get());
        when(user.getPlugin()).thenReturn(plugin);
        when(user.isOnline()).thenReturn(true);
        when(votingUsers.getVotingPluginUser(ID, true)).thenReturn(user);
        // This is the resolved legacy API's failed-empty result. The nullable
        // native query below still distinguishes failure from successful-empty.
        when(votingUsers.getAllUUIDs()).thenReturn(new ArrayList<>());
        BukkitScheduler scheduler = mock(BukkitScheduler.class);
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);
        var folia = mock(com.bencodez.simpleapi.folialib.FoliaLib.class);
        commitScheduler = mock(com.bencodez.simpleapi.folialib.impl.PlatformScheduler.class);
        when(scheduler.getFoliaLib()).thenReturn(folia);
        when(folia.getScheduler()).thenReturn(commitScheduler);
        when(commitScheduler.runLater(any(Runnable.class), eq(1L))).thenAnswer(c -> {
            var cancelled = new java.util.concurrent.atomic.AtomicBoolean();
            var task = mock(com.bencodez.simpleapi.folialib.wrapper.task.WrappedTask.class);
            when(task.isCancelled()).thenAnswer(ignored -> cancelled.get());
            doAnswer(ignored -> { cancelled.set(true); return null; }).when(task).cancel();
            commitWakeups.add(task);
            platform.add(c.getArgument(0));
            return task;
        });
        doAnswer(c -> { platform.add(c.getArgument(1)); return null; }).when(scheduler)
                .runTask(eq(plugin), any(Runnable.class));
        doAnswer(c -> { platform.add(c.getArgument(1)); return null; }).when(scheduler)
                .runTask(eq(plugin), any(Runnable.class), any(Entity.class));
        bukkit = mockStatic(Bukkit.class);
        Player online = mock(Player.class);
        when(online.getUniqueId()).thenReturn(ID);
        when(online.getName()).thenReturn("SyntheticVoter");
        when(votingUsers.getVotingPluginUser(online)).thenReturn(user);
        bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of(online));
        Server server = mock(Server.class);
        when(server.isPrimaryThread()).thenAnswer(c -> !worker);
        events = new SimplePluginManager(server, mock(SimpleCommandMap.class));
        bukkit.when(Bukkit::getPluginManager).thenReturn(events);
        bukkit.when(Bukkit::isPrimaryThread).thenAnswer(c -> !worker);
        ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
        when(plugin.getVoteTimer()).thenReturn(voteTimer);
        doAnswer(c -> { voteTasks.add(c.getArgument(0)); return null; }).when(voteTimer).execute(any());
        doAnswer(c -> {
            var future = enqueueWakeup(voteTasks, c.getArgument(0));
            voteWakeups.add(future);
            return future;
        }).when(voteTimer)
                .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        persistence = mock(ScheduledExecutorService.class);
        when(manager.getTimer()).thenReturn(persistence);
        doAnswer(c -> {
            var future = enqueueWakeup(storage, c.getArgument(0));
            storageWakeups.add(future);
            return future;
        }).when(persistence).schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        misc = mockStatic(MiscUtils.class);
        misc.when(MiscUtils::getInstance).thenReturn(mock(MiscUtils.class));
        party = new VoteParty(plugin);
    }

    @AfterEach
    void closeStatics() { misc.close(); bukkit.close(); }

    private java.util.concurrent.ScheduledFuture<?> enqueueWakeup(ArrayDeque<Runnable> queue, Runnable work) {
        var cancelled = new java.util.concurrent.atomic.AtomicBoolean();
        var completed = new java.util.concurrent.atomic.AtomicBoolean();
        java.util.concurrent.ScheduledFuture<?> future = mock(java.util.concurrent.ScheduledFuture.class);
        when(future.cancel(anyBoolean())).thenAnswer(c -> { cancelled.set(true); return true; });
        when(future.isCancelled()).thenAnswer(c -> cancelled.get());
        when(future.isDone()).thenAnswer(c -> cancelled.get() || completed.get());
        queue.add(() -> {
            if (cancelled.get()) return;
            try { work.run(); } finally { completed.set(true); }
        });
        return future;
    }

    private void drain() { drain(12); }

    private void drain(int maxPhases) {
        for (int phase = 0; phase < maxPhases && (!storage.isEmpty() || !platform.isEmpty() || !voteTasks.isEmpty()); phase++) {
            while (!voteTasks.isEmpty()) {
                worker = true;
                try { voteTasks.remove().run(); } finally { worker = false; }
            }
            while (!storage.isEmpty()) {
                worker = true;
                try { storage.remove().run(); } finally { worker = false; }
            }
            while (!platform.isEmpty()) platform.remove().run();
        }
        assertTrue(storage.isEmpty() && platform.isEmpty() && voteTasks.isEmpty(), "bounded party work must settle");
    }

    private void assertUnconsumed() {
        assertEquals(List.of(ID.toString()), party.getVotedUsers());
        assertEquals(2, votes.get());
        verify(coreUsers, never()).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(serverData, never()).updateLastVoteParty();
    }

    @Test
    void failedEmptyRetainsEligibilityAndRetryDoesNotRepeatOnlineReward() {
        when(mysql.getRowsQuery()).thenReturn(null);
        party.giveRewards(user, false);
        drain();
        assertUnconsumed();
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>(List.of(new Column("uuid", new DataValueString(ID.toString())))));
        party.giveRewards(user, false);
        drain();
        assertEquals(List.of(), party.getVotedUsers());
        assertEquals(0, votes.get());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), eq("VoteParty.Rewards"), any(RewardOptions.class));
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(mysql, times(2)).getRowsQuery();
        verify(plugin.getVotingPluginUserManager(), never()).getAllUUIDs();
    }

    @Test
    void successfulEmptyCompletesAndStillRewardsTheLiveRecipient() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        drain();
        assertEquals(List.of(), party.getVotedUsers());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @Test
    void failedCheckDoesNotSpendGlobalCountOrFirstPartyTimestamp() {
        when(mysql.getRowsQuery()).thenReturn(null);
        party.check(user, false);
        drain();
        assertUnconsumed();
        assertEquals(3, party.getTotalVotes());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        verify(serverData, times(1)).updateLastVoteParty();
        org.mockito.ArgumentCaptor<RewardOptions> options = org.mockito.ArgumentCaptor.forClass(RewardOptions.class);
        verify(apiRewards).giveReward(eq(user), eq(rewards), anyString(), options.capture());
        assertEquals("true", options.getValue().getPlaceholders().get("FirstVotePartyToday"));
    }

    @Test
    void thrownEnumerationFailureAlsoLeavesPartyAvailableForRetry() {
        when(mysql.getRowsQuery()).thenThrow(new IllegalStateException("synthetic storage failure"));
        party.giveRewards(user, false);
        drain();
        assertUnconsumed();
        doReturn(new ArrayList<>()).when(mysql).getRowsQuery();
        party.giveRewards(user, false);
        drain();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void lateCallbacksFromFailedAttemptCannotConsumeAnotherParty() {
        when(mysql.getRowsQuery()).thenReturn(null);
        party.giveRewards(user, false);
        drain();
        assertUnconsumed();
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        firstSuccess.accept(null);
        firstFailure.accept(new IllegalStateException("late failure"));
        assertEquals(List.of(ID.toString()), party.getVotedUsers());
        drain();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }
    @Test
    void rejectedAdmissionDoesNotRunQueryInlineOrConsumeParty() {
        when(manager.deferSharedStorageResultFromPlatform(any(), any(), any()))
                .thenThrow(new RejectedExecutionException("synthetic worker stopped"));
        party.giveRewards(user, false);
        assertUnconsumed();
        verify(mysql, never()).getRowsQuery();
        doReturn(false).when(manager).deferSharedStorageResultFromPlatform(any(), any(), any());
        party.giveRewards(user, false);
        assertUnconsumed();
        verify(mysql, never()).getRowsQuery();
    }

    @Test
    void pendingPreparationAndResetExcludeAnotherForceRequest() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        party.giveRewards(user, false);
        assertEquals(1, storage.size());
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        int admissions = storage.size();
        party.giveRewards(user, false);
        assertEquals(admissions, storage.size());
        drain();
        verify(mysql, times(1)).getRowsQuery();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void manualResetInvalidatesPreparedResultBeforeItCanTouchNewParty() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        party.reset(true);
        votes.set(7);
        party.setVotedUsers(List.of("new-party"));
        drain();
        assertEquals(7, votes.get());
        assertEquals(List.of("new-party"), party.getVotedUsers());
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void workerOriginatedPreparationNeverQueriesWhileVoteMonitorIsHeld() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        synchronized (party) {
            worker = true;
            try { party.giveRewards(user, false); } finally { worker = false; }
            verify(mysql, never()).getRowsQuery();
            assertEquals(1, storage.size());
        }
        drain();
        verify(mysql, times(1)).getRowsQuery();
    }

    @Test
    void invalidIdentityDoesNotTurnAnIncompleteEnumerationIntoSuccessfulEmpty() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>(List.of(new Column("uuid", new DataValueString("invalid")))));
        party.giveRewards(user, false);
        drain();
        assertUnconsumed();
    }

    @Test
    void queuedOldCounterWipeCannotClearCountersAfterManualPartyReset() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run(); // Recipient reads and the old wipe are admitted.
        party.reset(true);
        votes.set(7);
        party.setVotedUsers(List.of("new-party"));
        drain();
        assertEquals(7, votes.get());
        assertEquals(List.of("new-party"), party.getVotedUsers());
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @Test
    void ownerReplacementAfterQueryCannotCommitOrResetReplacementStorage() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        currentOwner.set(new UserStorageOwner(UserStorage.MYSQL, mock(MySQL.class), null));
        drain();
        assertUnconsumed();
        assertEquals(3, party.getTotalVotes());
    }

    @Test
    void ownerReplacementBeforeDeliveryDoesNotReadOrResetReplacementEligibility() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        currentOwner.set(new UserStorageOwner(UserStorage.MYSQL, mock(MySQL.class), null));
        drain();
        assertEquals(2, votes.get());
        verify(coreUsers, never()).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void platformCommitNeverWaitsForVoteStorageAndLeavesStateRetryable() throws Exception {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        Runnable commit = platform.remove();
        java.util.concurrent.CountDownLatch storageEntered = new java.util.concurrent.CountDownLatch(1);
        java.util.concurrent.CountDownLatch releaseStorage = new java.util.concurrent.CountDownLatch(1);
        java.util.concurrent.CountDownLatch platformDone = new java.util.concurrent.CountDownLatch(1);
        when(user.getVotePartyVotes()).thenAnswer(c -> {
            storageEntered.countDown();
            if (!releaseStorage.await(5, java.util.concurrent.TimeUnit.SECONDS))
                throw new AssertionError("storage fixture was not released");
            return votes.get();
        });
        AtomicReference<Throwable> voteFailure = new AtomicReference<>();
        AtomicReference<Throwable> platformFailure = new AtomicReference<>();
        Thread incomingVote = new Thread(() -> {
            worker = true;
            try { party.addTotal(user); } catch (Throwable error) { voteFailure.set(error); }
            finally { worker = false; }
        });
        Thread platformAttempt = new Thread(() -> {
            worker = false;
            try (MockedStatic<Bukkit> threadBukkit = mockStatic(Bukkit.class)) { commit.run(); }
            catch (Throwable error) { platformFailure.set(error); }
            finally { platformDone.countDown(); }
        });
        incomingVote.start();
        boolean completedWithoutStorage;
        try {
            assertTrue(storageEntered.await(5, java.util.concurrent.TimeUnit.SECONDS));
            platformAttempt.start();
            completedWithoutStorage = platformDone.await(1, java.util.concurrent.TimeUnit.SECONDS);
        } finally { releaseStorage.countDown(); }
        incomingVote.join(5000);
        platformAttempt.join(5000);
        assertFalse(incomingVote.isAlive());
        assertFalse(platformAttempt.isAlive());
        assertNull(voteFailure.get());
        assertNull(platformFailure.get());
        assertTrue(completedWithoutStorage, "the platform must not wait for a storage-held vote monitor");
        assertEquals(4, party.getTotalVotes());
        assertEquals(List.of(ID.toString()), party.getVotedUsers());
        verify(serverData, never()).updateLastVoteParty();
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        // Releasing the contended vote must be enough; no new vote/check is needed.
        drain();
        assertEquals(1, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void realAsyncEventContractRejectsPrimaryThreadAndAllowsWorkerThread() {
        assertThrows(IllegalStateException.class,
                () -> events.callEvent(new com.bencodez.votingplugin.events.VotePartyEvent()));
        worker = true;
        try { assertDoesNotThrow(() -> events.callEvent(new com.bencodez.votingplugin.events.VotePartyEvent())); }
        finally { worker = false; }
    }

    @Test
    void replacementAfterAdmissionsCannotReadOrWipeNewOwnersCounters() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        currentOwner.set(new UserStorageOwner(UserStorage.MYSQL, mock(MySQL.class), null));
        votes.set(7);
        worker = true;
        try {
            storage.remove().run(); // The per-user read reports failure, without granting a reward.
            assertThrows(IllegalStateException.class, () -> storage.remove().run()); // Stored loop.
            assertDoesNotThrow(() -> storage.remove().run()); // Stale counter wipe is cancelled.
        } finally { worker = false; }
        while (!platform.isEmpty()) platform.remove().run();
        assertEquals(7, votes.get());
        verify(coreUsers, never()).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void rejectedPlatformDispatchRetainsCountsVotersAndFirstPartyMetadata() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        doThrow(new RejectedExecutionException("synthetic platform stopped"))
                .when(commitScheduler).runLater(any(Runnable.class), eq(1L));
        party.check(user, false);
        drain();
        assertUnconsumed();
        assertEquals(3, party.getTotalVotes());
    }

    @Test
    void replacementAfterEventBeforeDispatchLeavesPartyRetryable() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        currentOwner.set(new UserStorageOwner(UserStorage.MYSQL, mock(MySQL.class), null));
        drain();
        assertUnconsumed();
        assertEquals(3, party.getTotalVotes());
    }

    @Test
    void countCommandDoesNotWaitForInFlightVoteStorage() throws Exception {
        assertMetadataCommandDoesNotWait(false);
    }

    @Test
    void resetCommandDoesNotWaitForInFlightVoteStorage() throws Exception {
        assertMetadataCommandDoesNotWait(true);
    }

    private void assertMetadataCommandDoesNotWait(boolean reset) throws Exception {
        java.util.concurrent.CountDownLatch entered = new java.util.concurrent.CountDownLatch(1);
        java.util.concurrent.CountDownLatch release = new java.util.concurrent.CountDownLatch(1);
        java.util.concurrent.CountDownLatch changed = new java.util.concurrent.CountDownLatch(1);
        when(user.getVotePartyVotes()).thenAnswer(c -> {
            entered.countDown();
            if (!release.await(5, java.util.concurrent.TimeUnit.SECONDS)) throw new AssertionError("storage not released");
            return votes.get();
        });
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread incoming = new Thread(() -> {
            try { party.addTotal(user); } catch (Throwable error) { failure.set(error); }
        });
        Thread command = new Thread(() -> {
            try {
                if (reset) party.reset(true); else party.setTotalVotes(8);
            } catch (Throwable error) { failure.set(error); }
            finally { changed.countDown(); }
        });
        incoming.start();
        boolean immediate;
        try {
            assertTrue(entered.await(5, java.util.concurrent.TimeUnit.SECONDS));
            command.start();
            immediate = changed.await(1, java.util.concurrent.TimeUnit.SECONDS);
        } finally { release.countDown(); }
        incoming.join(5000);
        command.join(5000);
        assertFalse(incoming.isAlive());
        assertFalse(command.isAlive());
        assertNull(failure.get());
        assertTrue(immediate, "scalar commands must not wait for user storage");
        assertEquals(reset ? 0 : 8, party.getTotalVotes());
    }


    @Test
    void wholeVoteRefusalPrecedesMutationAndOriginalOwnerCanRetry() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        party.check(user, false);
        for (int index = 0; index < 511; index++) party.addTotal(user);
        int global = party.getTotalVotes();
        int personal = votes.get();
        AtomicInteger eventEffects = new AtomicInteger();
        assertFalse(party.tryWithVoteAdmission(eventEffects::incrementAndGet));
        assertThrows(RejectedExecutionException.class, () -> party.vote(user, true, false, true));
        assertEquals(0, eventEffects.get());
        assertEquals(global, party.getTotalVotes());
        assertEquals(personal, votes.get());
        party.reset(true);
        assertTrue(party.tryWithVoteAdmission(() -> {
            eventEffects.incrementAndGet();
            party.vote(user, true, false, true);
        }));
        assertEquals(1, eventEffects.get());
        assertEquals(1, party.getTotalVotes());
    }

    @Test
    void saturatedAcceptedContributionsDrainWithoutAnotherVoteAndReleaseAdmission() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        assertThrows(RejectedExecutionException.class, () -> party.addTotal(user));
        drain(2048);
        assertEquals(2, party.getTotalVotes());
        assertEquals(2, votes.get());
        verify(serverData, times(171)).updateLastVoteParty();
        verify(apiRewards, times(171)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        assertTrue(party.tryWithVoteAdmission(() -> party.vote(user, true, false, true)));
        drain();
        assertEquals(0, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(apiRewards, times(172)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void subthresholdCreditHistoryDoesNotBlockTheNextVoteAtLargeThreshold() {
        when(config.getVotePartyVotesRequired()).thenReturn(1000);
        state.set("VoteParty.Total", 1000);
        votes.set(1000);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        drain();
        assertEquals(512, party.getTotalVotes());
        assertEquals(512, votes.get());
        assertTrue(party.tryDispatchVoteEvent(() -> party.vote(user, true, false, true)));
        assertEquals(513, party.getTotalVotes());
        assertEquals(513, votes.get());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void dailyLimitDoesNotTurnRetainedCreditsIntoWholeVoteBackpressure() {
        assertPeriodLimitDoesNotBlockOrdinaryVotes(false);
    }

    @Test
    void weeklyLimitDoesNotTurnRetainedCreditsIntoWholeVoteBackpressure() {
        assertPeriodLimitDoesNotBlockOrdinaryVotes(true);
    }

    private void assertPeriodLimitDoesNotBlockOrdinaryVotes(boolean weekly) {
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        if (weekly) {
            when(config.isVotePartyOnlyOncePerWeek()).thenReturn(true);
            when(serverData.isLastVotePartySameWeek()).thenAnswer(c -> lastParty.get());
        } else when(config.isVotePartyOnlyOncePerDay()).thenReturn(true);
        votes.set(3);
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        drain();
        assertEquals(512, party.getTotalVotes());
        assertEquals(512, votes.get());
        assertTrue(party.tryDispatchVoteEvent(() -> party.vote(user, true, false, true)));
        assertEquals(513, party.getTotalVotes());
        assertEquals(513, votes.get());
        verify(serverData, times(1)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void fullRetainedBoundaryCanResumeAfterAutomaticPartiesAreReenabled() {
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        votes.set(3);
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        when(config.isVotePartyEnabled()).thenReturn(false);
        drain();
        when(config.isVotePartyEnabled()).thenReturn(true);
        AtomicInteger incomingEffects = new AtomicInteger();
        assertFalse(party.tryDispatchVoteEvent(() -> {
            incomingEffects.incrementAndGet();
            party.vote(user, true, false, true);
        }));
        assertEquals(0, incomingEffects.get());
        drain(2048);
        assertEquals(2, party.getTotalVotes(), "retained eligible work must resume before accepting a new vote");
        assertTrue(party.tryDispatchVoteEvent(() -> {
            incomingEffects.incrementAndGet();
            party.vote(user, true, false, true);
        }));
        drain();
        assertEquals(1, incomingEffects.get());
        assertEquals(0, party.getTotalVotes());
        verify(apiRewards, times(172)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void loweringCountBelowThresholdRetainsCreditsWithoutStrandingAdmission() {
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        votes.set(3);
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        party.setTotalVotes(2);
        drain();
        assertEquals(2, party.getTotalVotes());
        assertEquals(515, votes.get(), "the invalidated attempt did not spend personal credit");
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        assertTrue(party.tryDispatchVoteEvent(() -> party.vote(user, true, false, true)));
        drain();
        assertEquals(0, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void nonResettingCheckOwnsEveryContributionAdmittedBeforeThatCheck() {
        when(config.isVotePartyResetCount()).thenReturn(false);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        for (int index = 0; index < 3; index++) party.addTotal(user);
        party.check(user, false);
        drain();
        assertEquals(6, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(serverData, times(2)).updateLastVoteParty();
        verify(apiRewards, times(2)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void preparationFailureDoesNotWaitForContendedScalarCommitOnPlatform() throws Exception {
        party.check(user, false);
        var field = VoteParty.class.getDeclaredField("scalarCommit");
        field.setAccessible(true);
        var lock = (java.util.concurrent.locks.ReentrantLock) field.get(party);
        var held = new java.util.concurrent.CountDownLatch(1);
        var release = new java.util.concurrent.CountDownLatch(1);
        var returned = new java.util.concurrent.CountDownLatch(1);
        AtomicReference<Throwable> failure = new AtomicReference<>();
        Thread holder = new Thread(() -> {
            lock.lock();
            try {
                held.countDown();
                try { release.await(); } catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
            } finally { lock.unlock(); }
        });
        Thread callback = new Thread(() -> {
            try { firstFailure.accept(new IllegalStateException("synthetic query failure")); }
            catch (Throwable problem) { failure.set(problem); }
            finally { returned.countDown(); }
        });
        boolean nonblocking;
        holder.start();
        try {
            assertTrue(held.await(2, java.util.concurrent.TimeUnit.SECONDS));
            callback.start();
            nonblocking = returned.await(1, java.util.concurrent.TimeUnit.SECONDS);
        } finally {
            release.countDown();
            holder.join(2000);
            callback.join(2000);
        }
        assertNull(failure.get());
        assertTrue(nonblocking, "preparation failure must defer rather than wait on the platform owner");
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        drain();
        party.check(user, false);
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void rejectedContinuationResumesOnLaterCheckWithoutReplayingOldParty() {
        when(config.isVotePartyResetCount()).thenReturn(false);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        ScheduledExecutorService timer = plugin.getVoteTimer();
        AtomicInteger resets = new AtomicInteger();
        doAnswer(c -> {
            votes.set(0);
            if (resets.incrementAndGet() == 1) {
                doThrow(new RejectedExecutionException("synthetic timer rejection"))
                        .when(timer).execute(any());
                doThrow(new RejectedExecutionException("synthetic timer rejection")).when(timer)
                        .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
                doThrow(new RejectedExecutionException("synthetic persistence rejection"))
                        .when(persistence).schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
            }
            return null;
        }).when(coreUsers).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        party.check(user, false);
        party.addTotal(user);
        party.check(user, false);
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
        doAnswer(c -> { voteTasks.add(c.getArgument(0)); return null; }).when(timer).execute(any());
        doAnswer(c -> enqueueWakeup(voteTasks, c.getArgument(0))).when(timer)
                .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        doAnswer(c -> enqueueWakeup(storage, c.getArgument(0))).when(persistence)
                .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        party.check(user, false);
        drain();
        assertEquals(0, votes.get());
        verify(serverData, times(3)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void newerVoteCannotOvertakeAnAcceptedFiniteCheckContinuation() {
        when(config.isVotePartyResetCount()).thenReturn(false);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger resets = new AtomicInteger();
        doAnswer(c -> { votes.set(0); resets.incrementAndGet(); return null; }).when(coreUsers)
                .removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        party.check(user, false);
        for (int index = 0; index < 3; index++) { party.addTotal(user); party.check(user, false); }
        for (int step = 0; step < 100 && resets.get() == 0; step++) {
            if (!voteTasks.isEmpty()) {
                worker = true;
                try { voteTasks.remove().run(); } finally { worker = false; }
            } else if (!storage.isEmpty()) {
                worker = true;
                try { storage.remove().run(); } finally { worker = false; }
            } else if (!platform.isEmpty()) platform.remove().run();
            else fail("first party did not reach its counter-reset handoff");
        }
        assertEquals(1, resets.get());
        assertFalse(voteTasks.isEmpty(), "a finite continuation is queued but has not run");
        worker = true;
        try { party.vote(user, true, false, true); } finally { worker = false; }
        drain();
        assertEquals(7, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(serverData, times(5)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void zeroThresholdRetainsFiniteExplicitCheckBoundaries() {
        when(config.getVotePartyVotesRequired()).thenReturn(0);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        for (int index = 0; index < 3; index++) { party.addTotal(user); party.check(user, false); }
        drain();
        assertEquals(6, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(serverData, times(4)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void recipientModeRefreshDoesNotRepeatPartyEventOrCommands() throws Exception {
        when(plugin.isEnabled()).thenReturn(true);
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(config.getVotePartyGlobalCommands()).thenReturn(new ArrayList<>(List.of("synthetic-voteparty-command")));
        Server commandServer = mock(Server.class);
        bukkit.when(Bukkit::getServer).thenReturn(commandServer);
        AtomicInteger approvals = new AtomicInteger();
        var registered = new org.bukkit.plugin.RegisteredListener(new org.bukkit.event.Listener() { },
                (listener, event) -> { approvals.incrementAndGet(); }, org.bukkit.event.EventPriority.MONITOR, plugin, false);
        com.bencodez.votingplugin.events.VotePartyEvent.getHandlerList().register(registered);
        try {
            party.check(user, false);
            when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(true);
            worker = true;
            try { storage.remove().run(); } finally { worker = false; }
            platform.remove().run();
            when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(false);
            drain();
            assertEquals(1, approvals.get());
            verify(mysql, times(1)).getRowsQuery();
            verify(commandServer, times(1)).dispatchCommand(nullable(org.bukkit.command.CommandSender.class), eq("synthetic-voteparty-command"));
            verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        } finally { org.bukkit.event.HandlerList.unregisterAll(plugin); }
    }

    @Test
    void onlineOnlyModeChangeDrainsAcceptedBoundariesAndReleasesAdmission() {
        assertModeChangeDrainsAcceptedBoundaries(true);
    }

    @Test
    void votedPlayersModeChangeDrainsAcceptedBoundariesAndReleasesAdmission() {
        assertModeChangeDrainsAcceptedBoundaries(false);
    }

    private void assertModeChangeDrainsAcceptedBoundaries(boolean onlineOnly) {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        votes.set(3);
        party.check(user, false);
        for (int index = 0; index < 3; index++) {
            party.addTotal(user);
            party.check(user, false);
        }
        if (onlineOnly) when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(true);
        else when(config.isVotePartyGiveAllPlayers()).thenReturn(false);
        drain();
        assertEquals(0, party.getTotalVotes(), "both accepted thresholds still complete under the new recipient policy");
        assertEquals(0, votes.get());
        verify(apiRewards, times(2)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(mysql, never()).getRowsQuery();
        when(config.isVotePartyGiveAllPlayers()).thenReturn(true);
        when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(false);
        party.setTotalVotes(3);
        party.check(user, false);
        assertDoesNotThrow(() -> {
            for (int index = 0; index < 512; index++) party.addTotal(user);
        }, "completed records must not retain admission slots");
        assertEquals(515, party.getTotalVotes());
        assertEquals(512, votes.get());
    }

    @Test
    void disabledPartyDoesNotBlockOrdinaryIngressBehindRetainedPartyWork() {
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        when(config.isVotePartyEnabled()).thenReturn(false);
        AtomicInteger ordinaryEffects = new AtomicInteger();
        assertTrue(party.tryWithVoteAdmission(() -> {
            ordinaryEffects.incrementAndGet();
            party.vote(user, true, false, true);
        }));
        assertEquals(1, ordinaryEffects.get());
        assertEquals(515, party.getTotalVotes(), "disabled party counting stays inactive");
    }

    @Test
    void saturatedContributionOwnershipRejectsBeforeEitherCounterMutation() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        int acceptedGlobal = party.getTotalVotes();
        int acceptedPersonal = votes.get();
        assertThrows(RejectedExecutionException.class, () -> party.addTotal(user));
        assertEquals(acceptedGlobal, party.getTotalVotes());
        assertEquals(acceptedPersonal, votes.get());
        party.reset(true);
        assertDoesNotThrow(() -> party.addTotal(user), "manual reset releases old metadata ownership");
    }

    @Test
    void votesAccumulatedDuringStoredPreparationTriggerNextPartyWithoutAnotherVote() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        votes.set(3); // The first three accepted votes belong to the first party.

        // The first threshold starts asynchronous stored-recipient preparation.
        party.check(user, false);
        assertEquals(1, storage.size());

        // Three more votes arrive while the first party is still preparing.
        // Each vote performs the normal threshold check, but those checks are
        // suppressed while storedPreparation is active.
        for (int i = 0; i < 3; i++) {
            party.addTotal(user);
            party.check(user, false);
        }
        assertEquals(6, party.getTotalVotes());

        // Completing the first party must retain responsibility for the three
        // accumulated votes and start the second party without a seventh vote.
        drain();

        assertEquals(0, party.getTotalVotes(),
                "remaining eligible votes must be rechecked when stored preparation finishes");
        assertEquals(0, votes.get(), "both completed parties consume only their own personal contributions");
        verify(serverData, times(2)).updateLastVoteParty();
        verify(apiRewards, times(2)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }


    @Test
    void dailyLimitSkipsStoredRecipientPreparation() {
        when(config.isVotePartyOnlyOncePerDay()).thenReturn(true);
        lastParty.set(true);
        party.check(user, false);
        assertTrue(storage.isEmpty());
        assertEquals(3, party.getTotalVotes());
        verify(mysql, never()).getRowsQuery();
    }

    @Test
    void weeklyLimitSkipsStoredRecipientPreparation() {
        when(config.isVotePartyOnlyOncePerWeek()).thenReturn(true);
        when(serverData.isLastVotePartySameWeek()).thenReturn(true);
        party.check(user, false);
        assertTrue(storage.isEmpty());
        assertEquals(3, party.getTotalVotes());
        verify(mysql, never()).getRowsQuery();
    }

    @Test
    void disablingPartyDuringPreparationPreservesEligibility() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        when(config.isVotePartyEnabled()).thenReturn(false);
        drain();
        assertEquals(3, party.getTotalVotes());
        verify(serverData, never()).updateLastVoteParty();
    }

    @Test
    void disablingPartyAfterEventBeforeCommitPreservesEligibility() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        when(config.isVotePartyEnabled()).thenReturn(false);
        drain();
        assertEquals(3, party.getTotalVotes());
        verify(serverData, never()).updateLastVoteParty();
    }

    @Test
    void manualResetInvalidatesAlreadyQueuedRecipientReads() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        party.reset(true);
        votes.set(2);
        drain();
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void forceRequestReportsBusyWithoutStartingAnotherParty() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        AtomicInteger busy = new AtomicInteger();
        party.giveRewards(null, false, busy::incrementAndGet);
        assertEquals(1, busy.get());
        assertEquals(1, storage.size());
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
    }

    @Test
    void partialScalarCommitFailureRestoresPartyAndAllowsRetry() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (fail.getAndSet(false)) throw new IllegalStateException("save failed after mutation");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        assertEquals(3, party.getTotalVotes());
        assertEquals(List.of(ID.toString()), party.getVotedUsers());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void voteAfterScalarCommitSurvivesQueuedPersonalCounterReset() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { party.addTotal(user); } finally { worker = false; }
        drain();
        assertEquals(1, party.getTotalVotes());
        assertEquals(1, votes.get(), "the previous party must not wipe a later vote");
    }

    @Test
    @SuppressWarnings("unchecked")
    void queuedCounterResetTakesPartyFenceBeforeStorageAdmission() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        Runnable reset = storage.removeLast();
        doAnswer(c -> {
            assertTrue(Thread.holdsLock(party), "reset must take party admission before shared storage");
            return ((Function<UserStorageOwner, ?>) c.getArgument(0)).apply(currentOwner.get());
        }).when(manager).withSharedNativeUserStorage(any());
        worker = true;
        try { reset.run(); } finally { worker = false; }
    }

    @Test
    void earlyBroadcastFailureRestoresPartyBeforeRecipientAdmission() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        MiscUtils broadcaster = MiscUtils.getInstance();
        doThrow(new IllegalArgumentException("invalid broadcast")).doNothing()
                .when(broadcaster).broadcast(nullable(String.class));
        party.check(user, false);
        drain();
        assertEquals(3, party.getTotalVotes());
        assertEquals(List.of(ID.toString()), party.getVotedUsers());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void snapshotReadFailureReleasesAttemptForLaterRetry() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        when(serverData.isLastVotePartySameDay()).thenThrow(new IllegalStateException("snapshot unavailable"));
        drain();
        assertEquals(3, party.getTotalVotes());
        doReturn(false).when(serverData).isLastVotePartySameDay();
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void failedCounterResetRetriesWithoutRepeatingRewards() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(c -> {
            if (fail.getAndSet(false)) throw new IllegalStateException("temporary reset failure");
            votes.set(0);
            return null;
        }).when(coreUsers).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        party.addTotal(user);
        party.check(user, false);
        drain();
        assertEquals(1, votes.get());
        assertEquals(1, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(coreUsers, times(2)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @Test
    void rejectedCounterResetAdmissionRetriesWithoutRepeatingRewards() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger admissions = new AtomicInteger();
        doAnswer(c -> {
            if (admissions.incrementAndGet() == 1)
                throw new java.util.concurrent.RejectedExecutionException("reset rejected");
            return enqueueWakeup(storage, c.getArgument(0));
        }).when(persistence).schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        party.check(user, false);
        drain();
        party.check(user, false);
        drain();
        assertEquals(0, votes.get());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @SuppressWarnings("unchecked")
    private AtomicReference<Object> privateAttemptReference(String name) throws Exception {
        var field = VoteParty.class.getDeclaredField(name);
        field.setAccessible(true);
        return (AtomicReference<Object>) field.get(party);
    }

    private void finishAttempt(Object attempt) throws Exception {
        var method = VoteParty.class.getDeclaredMethod("finishStoredParty", Object.class);
        method.setAccessible(true);
        method.invoke(party, attempt);
    }

    private void registerCheck(Object attempt, Runnable action) throws Exception {
        var method = VoteParty.class.getDeclaredMethod("registerPendingCheck", Object.class, long.class, Runnable.class);
        method.setAccessible(true);
        method.invoke(party, attempt, 0L, action);
    }

    @Test
    void completionBeforeCheckRegistrationStillSchedulesCheck() throws Exception {
        Object attempt = new Object();
        privateAttemptReference("storedPreparation").set(attempt);
        finishAttempt(attempt);
        AtomicInteger checks = new AtomicInteger();
        registerCheck(attempt, checks::incrementAndGet);
        drain();
        assertEquals(1, checks.get());
    }

    @Test
    void oldCompletionCannotConsumeNewAttemptCheck() throws Exception {
        Object old = new Object();
        Object current = new Object();
        privateAttemptReference("storedPreparation").set(current);
        AtomicInteger checks = new AtomicInteger();
        registerCheck(current, checks::incrementAndGet);
        finishAttempt(old);
        assertTrue(voteTasks.isEmpty());
        finishAttempt(current);
        drain();
        assertEquals(1, checks.get());
    }

    @Test
    void resetReleasesCancelledScheduledCheckAdmission() throws Exception {
        AtomicInteger oldEffects = new AtomicInteger();
        registerCheck(new Object(), oldEffects::incrementAndGet);
        assertEquals(1, voteTasks.size());
        voteTasks.clear(); // The executor accepted, then cancelled, the callback.
        party.reset(true);
        party.setTotalVotes(3);
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        assertEquals(515, party.getTotalVotes(), "cancelled old work must not exhaust replacement ownership");
        assertEquals(0, oldEffects.get());
    }

    @Test
    void resetDuringCheckRegistrationCannotRelabelOldCheckAsNewGeneration() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        java.util.concurrent.atomic.AtomicBoolean resetOnce = new java.util.concurrent.atomic.AtomicBoolean();
        when(config.isVotePartyResetCount()).thenAnswer(c -> {
            if (!resetOnce.getAndSet(true)) {
                party.reset(true);
                party.setTotalVotes(8);
            }
            return false;
        });
        party.check(user, false);
        drain();
        assertEquals(8, party.getTotalVotes());
        verify(serverData, never()).updateLastVoteParty();
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void rejectedOldCheckCannotBeLostBehindReplacementCheck() throws Exception {
        Object replacement = new Object();
        AtomicInteger oldChecks = new AtomicInteger();
        AtomicInteger replacementChecks = new AtomicInteger();
        java.util.concurrent.atomic.AtomicBoolean firstSubmission = new java.util.concurrent.atomic.AtomicBoolean(true);
        var timer = plugin.getVoteTimer();
        doAnswer(c -> {
            if (firstSubmission.getAndSet(false)) {
                privateAttemptReference("storedPreparation").set(replacement);
                registerCheck(replacement, replacementChecks::incrementAndGet);
            }
            throw new RejectedExecutionException("synthetic admission refused");
        }).when(timer).schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        registerCheck(new Object(), oldChecks::incrementAndGet);
        finishAttempt(replacement);
        drain();
        assertEquals(1, oldChecks.get(), "a rejected old callback keeps its own completion ownership");
        assertEquals(1, replacementChecks.get(), "replacement context must remain independent");
    }

    @Test
    void rejectedRecheckRetainsItsContext() throws Exception {
        Object attempt = new Object();
        AtomicInteger checks = new AtomicInteger();
        var timer = plugin.getVoteTimer();
        doThrow(new java.util.concurrent.RejectedExecutionException("recheck rejected"))
                .when(timer).schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        registerCheck(attempt, checks::incrementAndGet);
        var ownedField = VoteParty.class.getDeclaredField("scheduledChecks");
        ownedField.setAccessible(true);
        assertEquals(1, ((java.util.Set<?>) ownedField.get(party)).size(), "the rejected check remains owned");
        assertEquals(1, storage.size(), "production must provide a fallback wakeup");
        drain();
        assertEquals(1, checks.get());
    }

    @Test
    void postcommitVoteCannotQualifyThePrecedingParty() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        votes.set(1);
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { party.addTotal(user); } finally { worker = false; }
        drain();
        assertEquals(1, votes.get());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void transientOwnerReadFailureRetainsCounterReset() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        Runnable reset = storage.removeLast();
        doThrow(new IllegalStateException("temporarily unavailable")).doReturn(currentOwner.get())
                .when(manager).sharedNativeUserStorageOwner();
        worker = true;
        try { reset.run(); } finally { worker = false; }
        verify(coreUsers, never()).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        drain();
        party.check(user, false);
        drain();
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @Test
    void failedRollbackRecoversBeforeTheNextVoteMutation() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger writes = new AtomicInteger();
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (writes.incrementAndGet() <= 2) throw new IllegalStateException("temporary save outage");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        party.addTotal(user);
        assertEquals(4, party.getTotalVotes(), "restore the original three votes before adding the next");
        party.check(user, false);
        drain();
        assertEquals(1, party.getTotalVotes());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void fourAccumulatedPartiesDrainAndRetainEachRewardEligibility() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(user.getUUID()).thenReturn(ID.toString());
        party.check(user, false);
        for (int i = 0; i < 9; i++) {
            party.addTotal(user);
            party.check(user, false);
        }
        assertEquals(12, party.getTotalVotes());
        drain();
        assertEquals(0, party.getTotalVotes(), "every accumulated threshold must drain");
        verify(serverData, times(4)).updateLastVoteParty();
        verify(apiRewards, times(4)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void forceRequestAdvancesRetainedResetWhenAutomaticPartiesAreDisabled() {
        when(config.isVotePartyEnabled()).thenReturn(false);
        when(config.getVotePartyGlobalCommands()).thenReturn(new ArrayList<>(List.of("synthetic-voteparty-command")));
        Server commandServer = mock(Server.class);
        bukkit.when(Bukkit::getServer).thenReturn(commandServer);
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(c -> {
            if (fail.getAndSet(false)) throw new IllegalStateException("temporary wipe failure");
            votes.set(0);
            return null;
        }).when(coreUsers).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        party.giveRewards(user, false);
        drain();
        party.giveRewards(user, false);
        drain();
        verify(coreUsers, times(2)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(commandServer, times(1)).dispatchCommand(nullable(org.bukkit.command.CommandSender.class), eq("synthetic-voteparty-command"));
        party.giveRewards(user, false); // Cleanup is now complete: this is a genuinely new force request.
        drain();
        verify(commandServer, times(2)).dispatchCommand(nullable(org.bukkit.command.CommandSender.class), eq("synthetic-voteparty-command"));
        verify(coreUsers, times(3)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
    }

    @Test
    void eventAdapterReusesOnlyAnUnclaimedReservationAndBoundsNestedEvents() {
        party.check(user, false);
        for (int index = 0; index < 510; index++) party.addTotal(user);
        AtomicInteger effects = new AtomicInteger();
        assertTrue(party.tryWithVoteAdmission(() -> {
            assertTrue(party.tryDispatchVoteEvent(() -> {
                effects.incrementAndGet();
                assertFalse(party.tryDispatchVoteEvent(effects::incrementAndGet));
            }));
        }));
        assertEquals(1, effects.get());
        assertTrue(party.tryDispatchVoteEvent(effects::incrementAndGet));
        assertEquals(2, effects.get(), "completed scopes release unused reservation slots");
    }

    @Test
    void directRewardHandlerCannotRelabelPreResetPreparationAsCurrent() throws Exception {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        var field = VoteParty.class.getDeclaredField("rewardHandler");
        field.setAccessible(true);
        var handler = (com.bencodez.votingplugin.specialrewards.voteparty.VotePartyRewardHandler) field.get(party);
        handler.giveRewards(user, false);
        worker = true;
        try { party.reset(true); } finally { worker = false; }
        votes.set(4); // Replacement-generation personal state.
        drain();
        assertEquals(4, votes.get());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(mysql, never()).getRowsQuery();
    }

    @Test
    void legacyProxyEventApiRejectsBeforeUserStorageAndEventDispatchWhenFull() throws Exception {
        when(plugin.getVoteParty()).thenReturn(party);
        var settings = mock(com.bencodez.votingplugin.config.BungeeSettings.class);
        when(settings.isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings()).thenReturn(settings);
        when(plugin.getVoteSiteManager()).thenReturn(mock(com.bencodez.votingplugin.votesites.VoteSiteManager.class));
        Server server = mock(Server.class);
        PluginManager dispatcher = mock(PluginManager.class);
        when(server.getPluginManager()).thenReturn(dispatcher);
        when(plugin.getServer()).thenReturn(server);
        var pluginField = VotingPluginUser.class.getDeclaredField("plugin");
        pluginField.setAccessible(true);
        pluginField.set(user, plugin);
        doCallRealMethod().when(user).bungeeVotePluginMessagingEvent(any(), anyLong(), any(), anyBoolean(),
                anyBoolean(), anyBoolean(), anyInt(), anyBoolean(), anyBoolean(), anyBoolean(), any(), anyBoolean());
        party.check(user, false);
        for (int index = 0; index < 512; index++) party.addTotal(user);
        clearInvocations(user);
        worker = true;
        try {
            assertThrows(RejectedExecutionException.class, () -> user.bungeeVotePluginMessagingEvent(
                    "votes.example", 123L, null, true, true, true, 1, false, true, true, UUID.randomUUID(), false));
        } finally { worker = false; }
        verify(user, never()).getPlayerName();
        verify(dispatcher, never()).callEvent(any());
    }

    @Test
    void unresolvedRollbackRejectsIngressBeforeEffectsAndSchedulesOneRecovery() throws Exception {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger writes = new AtomicInteger();
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (writes.incrementAndGet() <= 2) throw new IllegalStateException("temporary save outage");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        assertNotNull(privateAttemptReference("pendingScalarRollback").get());
        AtomicInteger effects = new AtomicInteger();
        for (int index = 0; index < 10; index++)
            assertFalse(party.tryWithVoteAdmission(effects::incrementAndGet));
        assertEquals(0, effects.get());
        assertEquals(1, voteTasks.size(), "repeated refusal owns one recovery wakeup");
        drain();
        assertTrue(party.tryWithVoteAdmission(effects::incrementAndGet));
        assertEquals(1, effects.get());
        assertEquals(3, party.getTotalVotes());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void platformCommitCannotAdmitIngressBetweenItsScalarWrites() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicReference<Boolean> admitted = new AtomicReference<>();
        AtomicInteger effects = new AtomicInteger();
        doAnswer(c -> {
            if (admitted.get() == null)
                admitted.set(party.tryWithVoteAdmission(effects::incrementAndGet));
            state.set(c.getArgument(0), c.getArgument(1));
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        assertEquals(Boolean.FALSE, admitted.get());
        assertEquals(0, effects.get());
        verify(serverData, times(1)).updateLastVoteParty();
    }

    @Test
    void admittedIngressDefersPlatformCommitUntilItsScopeCloses() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        assertTrue(party.tryWithVoteAdmission(() -> {
            worker = true;
            try { storage.remove().run(); } finally { worker = false; }
            platform.remove().run();
            worker = true;
            try { voteTasks.remove().run(); } finally { worker = false; }
            platform.remove().run();
            verify(serverData, never()).updateLastVoteParty();
        }));
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void voteAfterFailedPreparationRetainsNextPartyPersonalBoundary() {
        UUID otherId = UUID.fromString("00000000-0000-0000-0000-000000000003");
        VotingPluginUser other = mock(VotingPluginUser.class);
        AtomicInteger otherVotes = new AtomicInteger();
        when(other.getPlugin()).thenReturn(plugin);
        when(other.getUUID()).thenReturn(otherId.toString());
        when(other.getJavaUUID()).thenReturn(otherId);
        when(other.getVotePartyVotes()).thenAnswer(c -> otherVotes.get());
        doAnswer(c -> { otherVotes.set(c.getArgument(0)); return null; }).when(other).setVotePartyVotes(anyInt());
        when(manager.getPublishedCache(otherId)).thenReturn(mock(UserDataCache.class));
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(otherId, true)).thenReturn(other);
        when(user.getUUID()).thenReturn(ID.toString());
        when(config.isVotePartyCountOfflineVotes()).thenReturn(true);
        doAnswer(c -> { votes.set(0); otherVotes.set(0); return null; }).when(coreUsers)
                .removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        votes.set(3);
        when(mysql.getRowsQuery()).thenReturn(null);
        party.check(user, false);
        drain();
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>(List.of(
                new Column("uuid", new DataValueString(otherId.toString())))));
        party.vote(other, true, false, false); // The first vote after the failed attempt has been cleared.
        party.vote(other, true, false, false); // Preparation is now pending again.
        party.vote(user, true, false, true);
        drain();
        assertEquals(0, party.getTotalVotes());
        assertEquals(0, votes.get());
        assertEquals(0, otherVotes.get());
        verify(serverData, times(2)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
        verify(apiRewards, times(1)).giveReward(eq(other), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void genuineForceIncludesRetainedPersonalCreditsButPreservesLaterArrivals() {
        UUID otherId = UUID.fromString("00000000-0000-0000-0000-000000000004");
        VotingPluginUser other = mock(VotingPluginUser.class);
        AtomicInteger otherVotes = new AtomicInteger();
        when(other.getPlugin()).thenReturn(plugin);
        when(other.getUUID()).thenReturn(otherId.toString());
        when(other.getJavaUUID()).thenReturn(otherId);
        when(other.getVotePartyVotes()).thenAnswer(c -> otherVotes.get());
        doAnswer(c -> { otherVotes.set(c.getArgument(0)); return null; }).when(other).setVotePartyVotes(anyInt());
        when(manager.getPublishedCache(otherId)).thenReturn(mock(UserDataCache.class));
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(otherId, true)).thenReturn(other);
        when(user.getUUID()).thenReturn(ID.toString());
        doAnswer(c -> { votes.set(0); otherVotes.set(0); return null; }).when(coreUsers)
                .removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        when(mysql.getRowsQuery()).thenReturn(null);
        party.check(user, false);
        drain();
        for (int index = 0; index < 3; index++) party.addTotal(other);
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>(List.of(
                new Column("uuid", new DataValueString(otherId.toString())))));
        party.giveRewards(user, false);
        party.addTotal(other); // This contribution belongs after force admission.
        drain();
        assertEquals(7, party.getTotalVotes());
        assertEquals(1, otherVotes.get());
        verify(apiRewards, times(1)).giveReward(eq(other), eq(rewards), anyString(), any(RewardOptions.class));
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void acceptedExtraRequiredEditSurvivesRetainedRollbackRecovery() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger extra = new AtomicInteger();
        when(serverData.getVotePartyExtraRequired()).thenAnswer(c -> extra.get());
        doAnswer(c -> { extra.set(c.getArgument(0)); return null; })
                .when(serverData).setVotePartyExtraRequired(anyInt());
        AtomicInteger writes = new AtomicInteger();
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (writes.incrementAndGet() <= 2) throw new IllegalStateException("temporary save outage");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        party.setExtraVotesRequired(8); // Coordinated SetExtraRequired command path.
        party.addTotal(user);
        assertEquals(8, extra.get());
        assertEquals(4, party.getTotalVotes());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void cancelledInitialResetRemainsOwnedForLaterForceCleanup() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        assertFalse(storage.isEmpty(), "reset was accepted but has not executed");
        storage.removeLast(); // Keep earlier eligibility reads; cancel only the accepted reset.
        storageWakeups.get(storageWakeups.size() - 1).cancel(false);
        when(config.isVotePartyEnabled()).thenReturn(false);
        party.giveRewards(user, false);
        drain();
        assertEquals(0, votes.get());
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void repeatedForceCleanupRequestsKeepOneOrderedWakeup() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.giveRewards(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        platform.remove().run();
        int owned = storage.size();
        for (int index = 0; index < 1000; index++) party.giveRewards(user, false);
        assertEquals(owned, storage.size(), "pending cleanup must not enqueue one retry per force request");
        assertTrue(voteTasks.isEmpty());
        drain();
        verify(coreUsers, times(1)).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void cancelledResetRetryCanResumeOnReplacementWorker() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger resets = new AtomicInteger();
        doAnswer(c -> {
            if (resets.incrementAndGet() == 1) throw new IllegalStateException("temporary reset failure");
            votes.set(0);
            return null;
        }).when(coreUsers).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        party.giveRewards(user, false);
        drain();
        party.giveRewards(user, false);
        assertFalse(storage.isEmpty());
        storage.clear(); // Accepted retry cancelled when its executor is retired.
        ScheduledExecutorService replacement = mock(ScheduledExecutorService.class);
        doAnswer(c -> enqueueWakeup(storage, c.getArgument(0))).when(replacement)
                .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        when(manager.getTimer()).thenReturn(replacement);
        party.giveRewards(user, false);
        drain();
        assertEquals(0, votes.get());
        assertEquals(2, resets.get());
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void repeatedChecksKeepOnePendingPlatformCommit() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        assertEquals(1, platform.size());
        for (int index = 0; index < 1000; index++) party.check(user, false);
        assertEquals(1, platform.size());
        assertEquals(1, voteTasks.size(), "only one commit-resumption wakeup may be pending");
        drain();
        assertEquals(1, commitWakeups.size(), "an accepted live platform task must not be duplicated");
        verify(serverData, times(1)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void cancelledScalarRecoveryRemainsBoundedAndCanResume() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger writes = new AtomicInteger();
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (writes.incrementAndGet() <= 2) throw new IllegalStateException("temporary save outage");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        AtomicInteger effects = new AtomicInteger();
        for (int index = 0; index < 1000; index++) assertFalse(party.tryWithVoteAdmission(effects::incrementAndGet));
        assertEquals(1, voteTasks.size());
        voteWakeups.get(voteWakeups.size() - 1).cancel(false);
        voteTasks.clear();
        assertFalse(party.tryWithVoteAdmission(effects::incrementAndGet));
        assertEquals(1, voteTasks.size());
        drain();
        assertTrue(party.tryWithVoteAdmission(effects::incrementAndGet));
        assertEquals(1, effects.get());
        assertEquals(3, party.getTotalVotes());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void cancelledInitialPlatformCommitResumesWithoutDuplicateEffects() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        when(config.getVotePartyGlobalCommands()).thenReturn(new ArrayList<>(List.of("synthetic-party-command")));
        Server commandServer = mock(Server.class);
        bukkit.when(Bukkit::getServer).thenReturn(commandServer);
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        Runnable cancelled = platform.remove(); // Accepted platform commit is cancelled before execution.
        commitWakeups.get(commitWakeups.size() - 1).cancel();
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        assertEquals(0, votes.get());
        cancelled.run(); // Even a late callback from the cancelled handoff must be harmless.
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
        verify(commandServer, times(1)).dispatchCommand(any(), eq("synthetic-party-command"));
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void cancelledContendedCommitCanResumeOnReplacementWorker() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        worker = true;
        try { storage.remove().run(); } finally { worker = false; }
        platform.remove().run();
        worker = true;
        try { voteTasks.remove().run(); } finally { worker = false; }
        assertTrue(party.tryWithVoteAdmission(platform.remove())); // Commit loses the ingress fence without waiting.
        assertFalse(voteTasks.isEmpty());
        voteTasks.clear(); // The retired executor never runs its accepted commit wakeup.
        ScheduledExecutorService replacement = mock(ScheduledExecutorService.class);
        doAnswer(c -> { voteTasks.add(c.getArgument(0)); return null; }).when(replacement).execute(any());
        doAnswer(c -> enqueueWakeup(voteTasks, c.getArgument(0))).when(replacement)
                .schedule(any(Runnable.class), anyLong(), any(java.util.concurrent.TimeUnit.class));
        when(plugin.getVoteTimer()).thenReturn(replacement);
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        assertEquals(0, votes.get());
        verify(serverData, times(1)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void acceptedScalarCommandSurvivesRetainedRollbackRecovery() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        AtomicInteger writes = new AtomicInteger();
        doAnswer(c -> {
            state.set(c.getArgument(0), c.getArgument(1));
            if (writes.incrementAndGet() <= 2) throw new IllegalStateException("temporary save outage");
            return null;
        }).when(serverData).setInt(anyString(), anyInt());
        party.check(user, false);
        drain();
        party.setTotalVotes(8);
        party.addTotal(user);
        assertEquals(9, party.getTotalVotes());
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void failedPreparationPreservesOriginalBoundaryWhenRetryIncludesNewVotes() {
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(null);
        party.check(user, false);
        for (int i = 0; i < 3; i++) party.addTotal(user);
        drain();
        assertEquals(6, party.getTotalVotes());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        drain();
        assertEquals(0, party.getTotalVotes());
        verify(apiRewards, times(2)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void zeroThresholdDoesNotCreateUnboundedAutomaticFollowups() {
        when(config.getVotePartyVotesRequired()).thenReturn(0);
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        drain();
        verify(serverData, times(1)).updateLastVoteParty();
        assertEquals(3, party.getTotalVotes());
    }

    @Test
    void nonResettingPartyRetainsFiniteExplicitChecks() {
        when(config.isVotePartyResetCount()).thenReturn(false);
        when(user.getUUID()).thenReturn(ID.toString());
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        party.check(user, false);
        for (int i = 0; i < 3; i++) {
            party.addTotal(user);
            party.check(user, false);
        }
        drain();
        assertEquals(6, party.getTotalVotes());
        verify(serverData, times(4)).updateLastVoteParty();
        verify(apiRewards, times(1)).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

}
