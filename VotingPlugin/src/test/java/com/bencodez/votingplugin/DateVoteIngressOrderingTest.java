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
    @Test void firstTransportDeliveryFindsDefinitionsAndRewardHandlesAlreadyPublished() {
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
        doCallRealMethod().when(plugin).initializeDateVoteIngress(any());
        plugin.initializeDateVoteIngress(ingress);
        verify(plugin.getRewardHandler()).giveReward(eq(user), any(), startsWith("DateVoteMilestonesRuntime."), any());
        assertTrue(java.nio.file.Files.exists(root.resolve("date-vote-milestones")));
    }
}
