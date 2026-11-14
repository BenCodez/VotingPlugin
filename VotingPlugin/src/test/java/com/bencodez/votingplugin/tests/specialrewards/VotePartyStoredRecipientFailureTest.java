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
    private final AtomicReference<UserStorageOwner> currentOwner = new AtomicReference<>();
    private final ArrayDeque<Runnable> platform = new ArrayDeque<>();
    private final AtomicInteger votes = new AtomicInteger(2);
    private volatile boolean worker;
    private PluginManager events;
    private final java.util.concurrent.atomic.AtomicBoolean lastParty = new java.util.concurrent.atomic.AtomicBoolean();
    private VotingPluginMain plugin;
    private UserDataManager manager;
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
        misc = mockStatic(MiscUtils.class);
        misc.when(MiscUtils::getInstance).thenReturn(mock(MiscUtils.class));
        party = new VoteParty(plugin);
    }

    @AfterEach
    void closeStatics() { misc.close(); bukkit.close(); }

    private void drain() {
        for (int phase = 0; phase < 12 && (!storage.isEmpty() || !platform.isEmpty() || !voteTasks.isEmpty()); phase++) {
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
        party.check(user, false);
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
            assertThrows(IllegalStateException.class, () -> storage.remove().run()); // Counter wipe.
        } finally { worker = false; }
        while (!platform.isEmpty()) platform.remove().run();
        assertEquals(7, votes.get());
        verify(coreUsers, never()).removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewards), anyString(), any(RewardOptions.class));
    }

    @Test
    void rejectedPlatformDispatchRetainsCountsVotersAndFirstPartyMetadata() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());
        BukkitScheduler scheduler = plugin.getBukkitScheduler();
        doThrow(new RejectedExecutionException("synthetic platform stopped"))
                .when(scheduler).runTask(eq(plugin), any(Runnable.class));
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
    void votesAccumulatedDuringStoredPreparationTriggerNextPartyWithoutAnotherVote() {
        when(mysql.getRowsQuery()).thenReturn(new ArrayList<>());

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
        verify(serverData, times(2)).updateLastVoteParty();
    }


}
