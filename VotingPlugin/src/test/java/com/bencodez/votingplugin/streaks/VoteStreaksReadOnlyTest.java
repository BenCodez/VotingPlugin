package com.bencodez.votingplugin.streaks;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.util.List;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VoteStreaksReadOnlyTest {
    private VoteStreakHandler handler(VotingPluginMain plugin, YamlConfiguration config) {
        when(plugin.getSpecialRewardsConfig().getData()).thenReturn(config);
        when(plugin.getLogger()).thenReturn(java.util.logging.Logger.getAnonymousLogger());
        var handler = new VoteStreakHandler(plugin); handler.reload(); return handler;
    }
    private YamlConfiguration config() {
        var c = new YamlConfiguration();
        c.set("VoteStreaks.ProgressGroups.daily.Type", "DAILY");
        c.set("VoteStreaks.ProgressGroups.daily.VotesRequired", 2);
        for (String id : List.of("first", "next", "disabled")) {
            c.set("VoteStreaks.ProgressGroups.daily.Milestones." + id + ".Enabled", !id.equals("disabled"));
            c.set("VoteStreaks.ProgressGroups.daily.Milestones." + id + ".Amount", id.equals("first") ? 3 : 5);
        }
        return c;
    }
    @Test void sharedMilestonesReadOneExistingStateWithoutWritingOrGivingRewards() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); var handler = handler(plugin, config());
        var user = mock(VotingPluginUser.class); when(user.getDayVoteStreak()).thenReturn(3); when(user.getBestDayVoteStreak()).thenReturn(8);
        when(user.getVoteStreakState("VoteStreakGroup_DAILY_daily")).thenReturn("2026-01-10|3|2|true||0||8");
        var data = handler.snapshotForDisplay(user);
        assertEquals(3, data.periods().getFirst().current()); assertEquals(8, data.periods().getFirst().best());
        assertEquals(List.of("first", "next"), data.milestones().stream().map(VoteStreaksSnapshot.Milestone::id).toList());
        assertTrue(data.milestones().stream().allMatch(row -> row.current() == 3 && row.best() == 8));
        verify(user, times(1)).getVoteStreakState("VoteStreakGroup_DAILY_daily");
        verify(user, never()).setVoteStreakState(anyString(), anyString());
        verifyNoInteractions(plugin.getRewardHandler());
    }
    @Test void oldSnapshotSurvivesReloadAndDisabledDefinitionsDisappear() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); var config = config(); var handler = handler(plugin, config);
        var user = mock(VotingPluginUser.class); var old = handler.snapshotForDisplay(user);
        config.set("VoteStreaks.ProgressGroups.daily.Milestones.first.Enabled", false); handler.reload(); var current = handler.snapshotForDisplay(user);
        assertEquals(2, old.milestones().size()); assertEquals(List.of("next"), current.milestones().stream().map(VoteStreaksSnapshot.Milestone::id).toList());
        verify(user, never()).setVoteStreakState(anyString(), anyString());
    }
    @Test void visibleRowsAndStorageReadsAreBounded() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); var config = new YamlConfiguration();
        for (int i = 0; i < 520; i++) {
            config.set("VoteStreaks.row" + i + ".Type", "DAILY");
            config.set("VoteStreaks.row" + i + ".Enabled", true);
            config.set("VoteStreaks.row" + i + ".Requirements.Amount", 3);
        }
        var handler = handler(plugin, config); var user = mock(VotingPluginUser.class); var view = handler.snapshotForDisplay(user);
        assertEquals(512, view.milestones().size()); assertTrue(view.truncated());
        verify(user, times(512)).getVoteStreakState(anyString());
    }
}
