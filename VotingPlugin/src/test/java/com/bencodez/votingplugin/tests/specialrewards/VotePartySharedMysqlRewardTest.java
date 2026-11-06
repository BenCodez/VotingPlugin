package com.bencodez.votingplugin.tests.specialrewards;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.doAnswer;
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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.specialrewards.voteparty.VoteParty;
import com.bencodez.votingplugin.specialrewards.voteparty.VotePartyRewardHandler;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VotePartySharedMysqlRewardTest {
    private static final UUID USER_UUID = UUID.fromString("76b1476f-c0a1-4ab6-b44a-92908a9c7ccb");

    private VotingPluginMain plugin;
    private UserDataManager dataManager;
    private UserManager votingUsers;
    private VotingPluginUser user;
    private VoteParty voteParty;
    private VotePartyRewardHandler rewardHandler;
    private Logger logger;
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
        SpecialRewardsConfig config = mock(SpecialRewardsConfig.class);

        when(plugin.getUserManager()).thenReturn(coreUsers);
        when(coreUsers.getDataManager()).thenReturn(dataManager);
        when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
        when(plugin.getSpecialRewardsConfig()).thenReturn(config);
        when(plugin.getLogger()).thenReturn(logger);
        when(config.isVotePartyGiveOnlinePlayersOnly()).thenReturn(true);
        when(voteParty.getVotedUsers()).thenReturn(Collections.singletonList(USER_UUID.toString()));
        when(votingUsers.getVotingPluginUser(USER_UUID, true)).thenReturn(user);
        when(user.getJavaUUID()).thenReturn(USER_UUID);
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

        assertTrue(deferred.work.get());
        verify(dataManager).cacheUser(USER_UUID, null);
        verify(dataManager, never()).cacheUserIfNeeded(USER_UUID);
        verify(voteParty, never()).giveReward(user, false);

        deferred.success.accept(true);
        verify(voteParty).giveReward(user, false);
    }

    @Test
    void publishedSharedSnapshotDoesNotNeedWorkerReload() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));

        dispatch("giveVotedPlayers", null);

        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(voteParty).giveReward(user, false);
        verify(dataManager, never()).cacheUser(eq(USER_UUID), isNull());
        verify(dataManager, never()).deferSharedStorageResult(any(), any(), any(),
                nullable(Entity.class));
    }

    @Test
    void triggeringUserIsAlsoSwitchedFromNoCacheMode() throws Exception {
        when(dataManager.hasSharedSqlBackend()).thenReturn(true);
        when(dataManager.getPublishedCache(USER_UUID)).thenReturn(mock(UserDataCache.class));

        dispatch("giveVotedPlayers", user);

        verify(user).userDataFetechMode(UserDataFetchMode.DEFAULT);
        verify(user, never()).userDataFetechMode(UserDataFetchMode.NO_CACHE);
        verify(votingUsers, never()).getVotingPluginUser(USER_UUID, true);
        verify(voteParty).giveReward(user, false);
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

        assertFalse(deferred.work.get());
        verify(dataManager).cacheUser(USER_UUID, null);
        deferred.success.accept(false);

        verify(voteParty, never()).giveReward(user, false);
        verify(logger).warning(contains("VoteParty reward not sent"));
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
        Player owner = mock(Player.class);
        bukkit.when(() -> Bukkit.getPlayer(USER_UUID)).thenReturn(owner);
        deferSharedRead();

        dispatch("giveVotedPlayers", null);

        verify(dataManager).deferSharedStorageResult(any(), any(), any(), eq(owner));
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
        private Supplier<Boolean> work;
        private Consumer<Boolean> success;
        private Consumer<Throwable> failure;
    }
}
