package com.bencodez.votingplugin.tests.specialrewards;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.specialrewards.voteparty.VoteParty;
import com.bencodez.votingplugin.specialrewards.voteparty.VotePartyRewardHandler;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VotePartySharedMysqlRewardTest {
    private static final UUID USER_UUID = UUID.fromString("76b1476f-c0a1-4ab6-b44a-92908a9c7ccb");
    private static final String REWARD_PATH = "VoteParty.Rewards";

    private VotingPluginMain plugin;
    private UserDataManager dataManager;
    private UserManager votingUsers;
    private VotingPluginUser user;
    private VoteParty voteParty;
    private VotePartyRewardHandler rewardHandler;
    private Logger logger;
    private ServerData serverData;
    private RewardHandler apiRewards;
    private YamlConfiguration rewardData;
    private SpecialRewardsConfig config;
    private MockedStatic<Bukkit> bukkit;

    @BeforeEach
    void setUp() {
        plugin = mock(VotingPluginMain.class);
        com.bencodez.advancedcore.api.user.UserManager coreUsers =
                mock(com.bencodez.advancedcore.api.user.UserManager.class);
        dataManager = mock(UserDataManager.class);
        votingUsers = mock(UserManager.class);
        user = mock(VotingPluginUser.class);
        voteParty = mock(VoteParty.class);
        logger = mock(Logger.class);
        serverData = mock(ServerData.class);
        apiRewards = mock(RewardHandler.class);
        rewardData = new YamlConfiguration();
        config = mock(SpecialRewardsConfig.class);

        when(plugin.getUserManager()).thenReturn(coreUsers);
        when(coreUsers.getDataManager()).thenReturn(dataManager);
        when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
        when(plugin.getSpecialRewardsConfig()).thenReturn(config);
        when(plugin.getLogger()).thenReturn(logger);
        when(plugin.getServerData()).thenReturn(serverData);
        when(plugin.getRewardHandler()).thenReturn(apiRewards);
        when(config.getData()).thenReturn(rewardData);
        when(config.getVotePartyRewardsPath()).thenReturn(REWARD_PATH);
        when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(true);
        when(voteParty.getVotedUsers()).thenReturn(Collections.singletonList(USER_UUID.toString()));
        when(votingUsers.getVotingPluginUser(USER_UUID, true)).thenReturn(user);
        when(user.getJavaUUID()).thenReturn(USER_UUID);
        when(user.getPlugin()).thenReturn(plugin);
        when(user.isOnline()).thenReturn(true);

        bukkit = mockStatic(Bukkit.class);
        rewardHandler = new VotePartyRewardHandler(plugin, null, voteParty);
    }

    @AfterEach
    void tearDown() {
        bukkit.close();
    }

    @Test
    void sharedStorageLoadsUnpublishedPlaceholderBeforeReward() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        // An entry existing in the map does NOT mean that its SQL snapshot is ready.
        when(dataManager.containsKey(USER_UUID)).thenReturn(true);
        AtomicReference<UserDataCache> published = new AtomicReference<>();
        when(dataManager.getPublishedCache(USER_UUID)).thenAnswer(call -> published.get());
        doAnswer(call -> {
            published.set(mock(UserDataCache.class));
            return null;
        }).when(dataManager).cacheUser(eq(USER_UUID), isNull());
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);
        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(user, never()).userDataFetechMode(UserDataFetchMode.NO_CACHE);
        verify(voteParty, never()).giveReward(user, false);

        assertEquals(Integer.valueOf(0), deferred.work.get());
        verify(dataManager).cacheUser(USER_UUID, null);
        verify(dataManager, never()).cacheUserIfNeeded(USER_UUID);
        verify(voteParty, never()).giveReward(user, false);

        // The party timestamp changes after handler dispatch, before a queued callback.
        when(serverData.isLastVotePartySameDay()).thenReturn(true);
        deferred.success.accept(0);
        ArgumentCaptor<RewardOptions> options = ArgumentCaptor.forClass(RewardOptions.class);
        verify(apiRewards).giveReward(eq(user), eq(rewardData), eq(REWARD_PATH), options.capture());
        assertEquals("true", options.getValue().getPlaceholders().get("FirstVotePartyToday"));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void publishedSharedSnapshotIsStillReadOnStorageWorker() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);

        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(dataManager, never()).getPublishedCache(USER_UUID);
        verify(voteParty, never()).giveReward(user, false);
        assertEquals(Integer.valueOf(0), deferred.work.get());
        verify(dataManager, never()).cacheUser(eq(USER_UUID), isNull());
        deferred.success.accept(0);
        verify(apiRewards).giveReward(eq(user), eq(rewardData), eq(REWARD_PATH), any(RewardOptions.class));
    }

    @Test
    void triggeringUserIsAlsoSwitchedFromNoCacheMode() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", user);

        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(user, never()).userDataFetechMode(UserDataFetchMode.NO_CACHE);
        verify(votingUsers, never()).getVotingPluginUser(USER_UUID, true);
        verify(dataManager, never()).getPublishedCache(USER_UUID);
        assertEquals(Integer.valueOf(0), deferred.work.get());
        deferred.success.accept(0);
        verify(apiRewards).giveReward(eq(user), eq(rewardData), eq(REWARD_PATH), any(RewardOptions.class));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void fileStoragePreservesNoCacheAndImmediateDelivery() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(false);

        dispatch("giveVotedPlayers", null);

        verify(user).userDataFetechMode(UserDataFetchMode.NO_CACHE);
        verify(voteParty).giveReward(user, false);
        verify(dataManager, never()).getPublishedCache(USER_UUID);
    }

    @Test
    void missingSnapshotAfterWorkerLoadDoesNotGiveDefaultValuedReward() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);

        assertNull(deferred.work.get());
        verify(dataManager).cacheUser(USER_UUID, null);
        deferred.success.accept(null);

        verify(voteParty, never()).giveReward(user, false);
        verify(logger).warning(contains("VoteParty reward not sent"));
    }

    @Test
    void perUserVoteThresholdIsEvaluatedBeforeCountersReset() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(config.getVotePartyUserVotesRequired()).thenReturn(2);
        when(user.getVotePartyVotes()).thenReturn(2);
        AtomicReference<UserDataCache> published = new AtomicReference<>();
        when(dataManager.getPublishedCache(USER_UUID)).thenAnswer(call -> published.get());
        doAnswer(call -> {
            published.set(mock(UserDataCache.class));
            return null;
        }).when(dataManager).cacheUser(eq(USER_UUID), isNull());
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);
        assertEquals(Integer.valueOf(2), deferred.work.get());
        when(user.getVotePartyVotes()).thenReturn(0); // VoteParty.resetUserCounts ran
        deferred.success.accept(2);

        verify(user, times(1)).getVotePartyVotes();
        verify(apiRewards).giveReward(eq(user), eq(rewardData), eq(REWARD_PATH), any(RewardOptions.class));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void ineligibleVoterIsNotRewardedAfterDeferredLoad() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(config.getVotePartyUserVotesRequired()).thenReturn(2);
        when(user.getVotePartyVotes()).thenReturn(1);
        AtomicReference<UserDataCache> published = new AtomicReference<>();
        when(dataManager.getPublishedCache(USER_UUID)).thenAnswer(call -> published.get());
        doAnswer(call -> {
            published.set(mock(UserDataCache.class));
            return null;
        }).when(dataManager).cacheUser(eq(USER_UUID), isNull());
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);
        assertEquals(Integer.valueOf(1), deferred.work.get());
        deferred.success.accept(1);

        verify(apiRewards, never()).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
    }

    @Test
    void publishedCacheInvalidatedDuringWorkerCheckDoesNotDeliver() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        UserDataCache published = mock(UserDataCache.class);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(published, null);
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);
        assertNull(deferred.work.get());
        deferred.success.accept(null);

        verify(apiRewards, never()).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
        verify(logger).warning(contains("shared user data is unavailable"));
    }

    @Test
    void inlineFallbackWithoutOwnerSchedulesGlobalCompletion() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));
        BukkitScheduler scheduler = mock(BukkitScheduler.class);
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);

        // AdvancedCore returns false for a caller already on a storage worker.
        dispatch("giveVotedPlayers", null);
        org.mockito.ArgumentCaptor<Runnable> completion =
                org.mockito.ArgumentCaptor.forClass(Runnable.class);
        verify(scheduler).runTask(eq(plugin), completion.capture());
        verify(dataManager).getPublishedCache(USER_UUID);
        verify(apiRewards, never()).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));

        completion.getValue().run();
        verify(apiRewards).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
    }

    @Test
    void inlineFallbackWithOwnerSchedulesEntityCompletion() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));
        Player owner = mock(Player.class);
        when(owner.getUniqueId()).thenReturn(USER_UUID);
        when(votingUsers.getVotingPluginUser(owner)).thenReturn(user);
        when(votingUsers.getAllUUIDs()).thenReturn(new ArrayList<>());
        bukkit.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(owner));
        BukkitScheduler scheduler = mock(BukkitScheduler.class);
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);

        dispatch("giveAllPlayers", null);
        org.mockito.ArgumentCaptor<Runnable> completion =
                org.mockito.ArgumentCaptor.forClass(Runnable.class);
        verify(scheduler).runTask(eq(plugin), completion.capture(), eq(owner));
        verify(apiRewards, never()).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));

        completion.getValue().run();
        verify(apiRewards).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
    }

    @Test
    void deferredCompletionWithUnknownOwnerReschedulesToOnlineEntity() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));
        Deferred deferred = deferSharedRead();
        Player online = mock(Player.class);
        BukkitScheduler scheduler = mock(BukkitScheduler.class);
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);

        dispatch("giveVotedPlayers", null);
        assertEquals(Integer.valueOf(0), deferred.work.get());
        bukkit.when(() -> Bukkit.getPlayer(USER_UUID)).thenReturn(online);
        deferred.success.accept(0);

        org.mockito.ArgumentCaptor<Runnable> ownerTask =
                org.mockito.ArgumentCaptor.forClass(Runnable.class);
        verify(scheduler).runTask(eq(plugin), ownerTask.capture(), eq(online));
        verify(apiRewards, never()).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
        ownerTask.getValue().run();
        verify(apiRewards).giveReward(eq(user), eq(rewardData),
                eq(REWARD_PATH), any(RewardOptions.class));
    }

    @Test
    void databaseFailureIsLoggedAndDoesNotDispatchReward() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        Deferred deferred = deferSharedRead();

        dispatch("giveVotedPlayers", null);
        assertNotNull(deferred.failure);
        deferred.failure.accept(new IllegalStateException("database down"));

        verify(logger).warning(contains("database down"));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void workerRejectionIsLoggedWithoutDispatchingReward() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        doThrow(new RejectedExecutionException("storage worker stopped"))
                .when(dataManager).deferSharedStorageResult(any(), any(), any(),
                        nullable(Entity.class));

        dispatch("giveVotedPlayers", null);

        verify(logger).warning(contains("storage worker stopped"));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void votedOnlinePlayerUsesOwnerSchedulerForCompletion() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.isPlatformOwnedThread()).thenReturn(true);
        Player owner = mock(Player.class);
        bukkit.when(() -> Bukkit.getPlayer(USER_UUID)).thenReturn(owner);
        deferSharedRead();

        dispatch("giveVotedPlayers", null);

        verify(dataManager).deferSharedStorageResult(any(), any(), any(), eq(owner));
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void storageWorkerDoesNotLookUpBukkitPlayerForVotedRecipients() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.isPlatformOwnedThread()).thenReturn(false);
        deferSharedRead();

        dispatch("giveVotedPlayers", null);

        bukkit.verify(() -> Bukkit.getPlayer(USER_UUID), never());
        verify(dataManager).deferSharedStorageResult(any(), any(), any(), isNull());
        verify(voteParty, never()).giveReward(user, false);
    }

    @Test
    void giveAllPlayersAlsoDefersAndUsesOwnerScheduler() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        Player owner = mock(Player.class);
        when(owner.getUniqueId()).thenReturn(USER_UUID);
        when(votingUsers.getVotingPluginUser(owner)).thenReturn(user);
        when(votingUsers.getAllUUIDs()).thenReturn(new ArrayList<>());
        bukkit.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(owner));
        deferSharedRead();

        dispatch("giveAllPlayers", null);

        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(dataManager).deferSharedStorageResult(any(), any(), any(), eq(owner));
        verify(voteParty, never()).giveReward(user, false);
    }

    private Deferred deferSharedRead() {
        Deferred deferred = new Deferred();
        doAnswer(call -> {
            deferred.work = call.getArgument(0);
            deferred.success = call.getArgument(1);
            deferred.failure = call.getArgument(2);
            return true;
        }).when(dataManager).deferSharedStorageResult(any(), any(), any(),
                nullable(Entity.class));
        return deferred;
    }

    private void dispatch(String method, VotingPluginUser triggeringUser) throws Exception {
        Method entry = VotePartyRewardHandler.class.getDeclaredMethod(
                method, VotingPluginUser.class, boolean.class);
        entry.setAccessible(true);
        entry.invoke(rewardHandler, triggeringUser, false);
    }

    private static final class Deferred {
        private Supplier<Integer> work;
        private Consumer<Integer> success;
        private Consumer<Throwable> failure;
    }
}
