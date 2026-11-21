package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;

import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.Bukkit;
import org.bukkit.configuration.MemoryConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.*;

import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.specialrewards.votemilestones.*;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

class ManualMilestoneEligibilityTest {
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final UserData data = mock(UserData.class);
    final AtomicReference<String> limits = new AtomicReference<>();
    VoteMilestonesManager manager;
    @BeforeEach void setup() {
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(new YamlConfiguration());
        when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>());
        when(user.getJavaUUID()).thenReturn(UUID.randomUUID());
        when(user.getPlayerName()).thenReturn("Example"); when(user.getUserData()).thenReturn(data);
        when(data.getString("VoteMilestoneLimits")).thenAnswer(c -> limits.get());
        doAnswer(c -> { limits.set(c.getArgument(1)); return null; }).when(data).setString(eq("VoteMilestoneLimits"), anyString());
        manager = new VoteMilestonesManager(plugin);
    }
    void config(Map<String, VoteMilestone> milestones) throws Exception {
        Constructor<VoteMilestonesConfig> c = VoteMilestonesConfig.class.getDeclaredConstructor(Map.class); c.setAccessible(true);
        Field f = VoteMilestonesManager.class.getDeclaredField("config"); f.setAccessible(true); f.set(manager, c.newInstance(milestones));
    }
    VoteMilestone milestone(String id, VoteMilestoneTotal total, String path, VoteMilestoneLimit limit) {
        MemoryConfiguration cfg = new MemoryConfiguration(); cfg.set("At", 10);
        return new VoteMilestone(id, true, total, AtMatcher.fromConfig(cfg, "At"), null, path,
                VoteMilestoneGroupSelect.ALL, "Default", limit);
    }
    void check() throws Exception {
        // Reflection keeps the same regression executable against the pre-command production source.
        VoteMilestonesManager.class.getMethod("checkCurrentVoteTotals", VotingPluginUser.class).invoke(manager, user);
    }
    @Test void exactThresholdUsesNormalLimitsAndDoesNotMutateOtherProgress() throws Exception {
        config(Map.of("ten", milestone("ten", VoteMilestoneTotal.ALLTIME_VOTES, "VoteMilestones.ten.Rewards",
                new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0))));
        when(user.getTotal(TopVoter.AllTime)).thenReturn(11); check();
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
        when(user.getTotal(TopVoter.AllTime)).thenReturn(10);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
            check(); check();
        }
        verify(plugin.getRewardHandler(), times(1)).giveReward(eq(user), any(), eq("VoteMilestones.ten.Rewards"), any());
        assertNotNull(limits.get());
        verify(user, never()).setTotal(any(), anyInt());
        verifyNoInteractions(plugin.getVoteParty(), plugin.getDateVoteMilestones(), plugin.getVoteStreakHandler());
    }
    @Test void legacyMilestonesIncludedButCumulativeFirstVotePointsAndSitesExcluded() throws Exception {
        var root = new YamlConfiguration();
        root.set("MileStones.10.Enabled", true); root.set("Cumulative.10.Enabled", true);
        root.set("FirstVote.Enabled", true); root.set("AllSites.Enabled", true);
        var compiled = new LinkedHashMap<>(VoteMilestonesMigrator.compileLegacy(root, 10, false, false));
        compiled.put("sites", milestone("sites", VoteMilestoneTotal.ALLSITES_TODAY, "VoteMilestones.sites.Rewards", VoteMilestoneLimit.none()));
        compiled.put("points", milestone("points", VoteMilestoneTotal.POINTS, "VoteMilestones.points.Rewards", VoteMilestoneLimit.none()));
        config(compiled); when(user.getTotal(TopVoter.AllTime)).thenReturn(10);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class)); check();
        }
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), eq("MileStones.10.Rewards"), any());
        verify(plugin.getRewardHandler(), times(1)).giveReward(any(), any(), anyString(), any());
        verify(user, never()).getPoints(); verify(user, never()).getUniqueVoteSitesToday();
    }
    @Test void unlimitedMilestoneKeepsItsExistingConfiguredRepeatPolicy() throws Exception {
        config(Map.of("ten", milestone("ten", VoteMilestoneTotal.ALLTIME_VOTES,
                "VoteMilestones.ten.Rewards", VoteMilestoneLimit.none())));
        when(user.getTotal(TopVoter.AllTime)).thenReturn(10);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getPluginManager).thenReturn(mock(PluginManager.class));
            check(); check();
        }
        verify(plugin.getRewardHandler(), times(2)).giveReward(eq(user), any(), eq("VoteMilestones.ten.Rewards"), any());
        assertNull(limits.get());
    }
    @Test void manualAllTimeSetterOnlyChangesStoredTotalWithoutRewardChecks() throws Exception {
        var realUser = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        doReturn(data).when(realUser).getUserData();
        Field field = VotingPluginUser.class.getDeclaredField("plugin"); field.setAccessible(true); field.set(realUser, plugin);
        realUser.setTotal(TopVoter.AllTime, 10);
        verify(data).setInt("AllTimeTotal", 10);
        verifyNoInteractions(plugin.getRewardHandler());
    }
    @Test void readFailureStopsBeforeRewards() throws Exception {
        config(Map.of("ten", milestone("ten", VoteMilestoneTotal.ALLTIME_VOTES, "VoteMilestones.ten.Rewards", VoteMilestoneLimit.none())));
        when(user.getTotal(TopVoter.AllTime)).thenThrow(new IllegalStateException());
        assertThrows(InvocationTargetException.class, this::check);
        verify(plugin.getRewardHandler(), never()).giveReward(any(), any(), anyString(), any());
    }
}
