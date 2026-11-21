package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;

import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import org.bukkit.Bukkit;
import org.bukkit.plugin.PluginManager;
import org.bukkit.configuration.MemoryConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votemilestones.*;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

class MilestoneEvaluationConcurrencyTest {
    @Test void checkThenNormalVoteCannotSubmitTheSameLimitedRewardTwice() throws Exception { overlap(true, 0, false); }
    @Test void normalVoteThenCheckCannotSubmitTheSameLimitedRewardTwice() throws Exception { overlap(false, 0, false); }
    @Test void checkAndForceUseTheSameLimitFence() throws Exception { overlap(true, 1, false); }
    @Test void distinctMilestoneLimitUpdatesForTheSameUserAreNotLost() throws Exception { overlap(true, 0, true); }

    @Test void checkAndForceGroupUseTheSameLimitFence() throws Exception { overlap(true, 2, false); }
    @Test void limitResetCannotBeOverwrittenByAnInFlightCheck() throws Exception { overlap(true, 3, false); }

    void overlap(boolean manualFirst, int secondOperation, boolean distinct) throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(new YamlConfiguration());
        when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>());
        var manager = new VoteMilestonesManager(plugin);
        var milestones = new LinkedHashMap<String, VoteMilestone>();
        milestones.put("ten", milestone("ten", 10));
        if (distinct) milestones.put("twenty", milestone("twenty", 20));
        Constructor<VoteMilestonesConfig> ctor = VoteMilestonesConfig.class.getDeclaredConstructor(Map.class); ctor.setAccessible(true);
        Field config = VoteMilestonesManager.class.getDeclaredField("config"); config.setAccessible(true); config.set(manager, ctor.newInstance(milestones));
        UUID id = UUID.randomUUID();
        var firstUser = mock(VotingPluginUser.class);
        var secondUser = mock(VotingPluginUser.class);
        var data = mock(UserData.class);
        var state = new AtomicReference<String>();
        var secondRead = new CountDownLatch(1);
        var secondThread = new AtomicReference<Thread>();
        when(data.getString("VoteMilestoneLimits")).thenAnswer(c -> {
            if (Thread.currentThread() == secondThread.get()) secondRead.countDown();
            return state.get();
        });
        doAnswer(c -> { state.set(c.getArgument(1)); return null; }).when(data).setString(eq("VoteMilestoneLimits"), anyString());
        for (var user : List.of(firstUser, secondUser)) {
            when(user.getJavaUUID()).thenReturn(id);
            when(user.getPlayerName()).thenReturn("Example"); when(user.getUserData()).thenReturn(data);
        }
        when(firstUser.getTotal(TopVoter.AllTime)).thenReturn(10);
        when(secondUser.getTotal(TopVoter.AllTime)).thenReturn(distinct ? 20 : 10);
        var firstSubmitted = new CountDownLatch(1);
        var releaseFirst = new CountDownLatch(1);
        var submissions = new AtomicInteger();
        var rewards = plugin.getRewardHandler();
        doAnswer(c -> {
            if (submissions.incrementAndGet() == 1) {
                firstSubmitted.countDown();
                assertTrue(releaseFirst.await(5, TimeUnit.SECONDS));
            }
            return null;
        }).when(rewards).giveReward(any(), any(), anyString(), any());
        var failure = new AtomicReference<Throwable>();
        Thread first = new Thread(() -> run(failure, () -> {
            if (manualFirst) manager.checkCurrentVoteTotals(firstUser); else manager.handleVote(firstUser, null, null);
        }));
        Thread second = new Thread(() -> run(failure, () -> {
            if (secondOperation == 1) manager.forceMilestone(secondUser, "ten", false, null);
            else if (secondOperation == 2) manager.forceGroup(secondUser, "Default", false, null);
            else if (secondOperation == 3) manager.resetLimits(secondUser, "Default");
            else if (manualFirst) manager.handleVote(secondUser, null, null);
            else manager.checkCurrentVoteTotals(secondUser);
        }));
        secondThread.set(second);
        try {
            first.start(); assertTrue(firstSubmitted.await(5, TimeUnit.SECONDS));
            second.start();
            // Wait for a concrete event: a stale limit read (old implementation) or
            // monitor contention (fixed implementation), never a timing-based absence.
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (secondRead.getCount() != 0 && second.getState() != Thread.State.BLOCKED && System.nanoTime() < deadline) Thread.yield();
            assertEquals(Thread.State.BLOCKED, second.getState(), "Second evaluation must wait before reading limits");
            assertEquals(1, secondRead.getCount());
        } finally {
            releaseFirst.countDown(); first.join(5000); second.join(5000);
            if (first.isAlive()) first.interrupt(); if (second.isAlive()) second.interrupt();
        }
        assertFalse(first.isAlive()); assertFalse(second.isAlive());
        assertNull(failure.get(), () -> String.valueOf(failure.get()));
        assertEquals(distinct ? 2 : 1, submissions.get());
        if (secondOperation == 3) assertEquals("", state.get());
        else assertTrue(state.get().contains("ten|"));
        if (distinct) assertTrue(state.get().contains("twenty|"));
    }
    static void run(AtomicReference<Throwable> failure, Runnable action) {
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
            action.run();
        } catch (Throwable ex) { failure.compareAndSet(null, ex); }
    }
    static VoteMilestone milestone(String id, int value) {
        var config = new MemoryConfiguration(); config.set("At", value);
        return new VoteMilestone(id, true, VoteMilestoneTotal.ALLTIME_VOTES, AtMatcher.fromConfig(config, "At"), null,
                "VoteMilestones." + id + ".Rewards", VoteMilestoneGroupSelect.ALL, "Default",
                new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0));
    }
}
