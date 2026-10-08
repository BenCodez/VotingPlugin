package com.bencodez.votingplugin;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import java.nio.file.Path;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.bukkit.configuration.file.YamlConfiguration;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import com.bencodez.votingplugin.user.VotingPluginUser;
class DateVoteIngressOrderingTest {
    @TempDir Path root;
    @Test void firstTransportDeliveryFindsDefinitionsAndRewardHandlesAlreadyPublished() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        String prefix = "DateVoteMilestones.startup.";
        config.set(prefix + "Enabled", true); config.set(prefix + "Timezone", "UTC");
        config.set(prefix + "Start", "2020-01-01T00:00:00"); config.set(prefix + "End", "2099-01-01T00:00:00");
        config.set(prefix + "AccountingServer", "backend");
        config.set(prefix + "Milestones.1.Rewards.Messages.Player", "Thanks");
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        when(plugin.getBungeeSettings().getServer()).thenReturn("backend");
        DateVoteMilestones events = new DateVoteMilestones(plugin);
        when(plugin.getDateVoteMilestones()).thenReturn(events);
        VotingPluginUser user = mock(VotingPluginUser.class); when(user.getJavaUUID()).thenReturn(UUID.randomUUID());
        Runnable ingress = () -> {
            verify(plugin).addDirectlyDefinedRewards(any());
            events.accepted(user, "a", UUID.randomUUID(), System.currentTimeMillis(), true, true, true, false, false);
        };
        ready(plugin);
        doCallRealMethod().when(plugin).initializeDateVoteIngress(any());
        plugin.initializeDateVoteIngress(ingress);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        assertTrue(java.nio.file.Files.exists(root.resolve("date-vote-milestones")));
    }
    private static final String[] HANDLERS = {"voteMilestonesManager", "voteStreakHandler", "voteParty", "specialRewards", "topVoterHandler", "voteShopManager", "placeholders"};
    private static void ready(VotingPluginMain plugin) throws Exception {
        for (String name : HANDLERS) {
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, mock(field.getType()));
        }
    }
    @Test void eachMissingDownstreamHandlerKeepsIngressClosed() throws Exception {
        for (String name : HANDLERS) {
            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); ready(plugin);
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, null);
            doCallRealMethod().when(plugin).initializeDateVoteIngress(any()); Runnable ingress = mock(Runnable.class);
            assertThrows(IllegalStateException.class, () -> plugin.initializeDateVoteIngress(ingress)); verify(ingress, never()).run();
            verify(plugin.getDateVoteMilestones(), never()).reload();
            verify(plugin.getServer().getPluginManager(), never()).registerEvents(any(), eq(plugin));
        }
    }

    @Test void localVotifierAndAcceptedListenersAreRegisteredOnlyAfterDefinitionsAndHandlers() throws Exception {
        for (boolean proxyEnabled : new boolean[] {false, true}) {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        String prefix = "DateVoteMilestones.local.";
        config.set(prefix + "Enabled", true); config.set(prefix + "Timezone", "UTC");
        config.set(prefix + "Start", "2020-01-01T00:00:00"); config.set(prefix + "End", "2099-01-01T00:00:00");
        config.set(prefix + "Milestones.1.Rewards.Messages.Player", "Thanks");
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getDataFolder()).thenReturn(root.toFile());
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.isVotifierLoaded()).thenReturn(true);
        DateVoteMilestones events = new DateVoteMilestones(plugin);
        when(plugin.getDateVoteMilestones()).thenReturn(events);
        VotingPluginUser user = mock(VotingPluginUser.class); when(user.getJavaUUID()).thenReturn(UUID.randomUUID());
        var listeners = new java.util.ArrayList<org.bukkit.event.Listener>();
        var pluginManager = plugin.getServer().getPluginManager();
        doAnswer(invocation -> {
            verify(plugin).addDirectlyDefinedRewards(any());
            listeners.add(invocation.getArgument(0));
            if (invocation.getArgument(0) instanceof com.bencodez.votingplugin.listeners.VotiferEvent)
                assertTrue(events.accepted(user, "a", UUID.randomUUID(), System.currentTimeMillis(), true, proxyEnabled, proxyEnabled, false, false));
            return null;
        }).when(pluginManager).registerEvents(any(), eq(plugin));
        ready(plugin); doCallRealMethod().when(plugin).initializeDateVoteIngress(any());
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(proxyEnabled);
        when(plugin.getBungeeSettings().getServer()).thenReturn("backend");
        config.set(prefix + "AccountingServer", "backend");
        var opened = new java.util.concurrent.atomic.AtomicBoolean();
        Runnable proxy = () -> {
            assertEquals(1, listeners.size());
            assertInstanceOf(com.bencodez.votingplugin.listeners.PlayerVoteListener.class, listeners.getFirst());
            opened.set(true);
        };
        try (var overflow = mockConstruction(com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue.class)) {
            plugin.initializeDateVoteIngress(proxy);
            assertEquals(1, overflow.constructed().size());
        }
        assertEquals(2, listeners.size());
        assertInstanceOf(com.bencodez.votingplugin.listeners.PlayerVoteListener.class, listeners.get(0));
        assertInstanceOf(com.bencodez.votingplugin.listeners.VotiferEvent.class, listeners.get(1));
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        assertEquals(proxyEnabled, opened.get());
        }
    }

}
