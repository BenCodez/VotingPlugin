package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.NextSite;
import com.bencodez.votingplugin.votesites.VoteSite;

class CoolDownTimerGenerationTest {
    @Test void replacedTimerDoesNotLoadOrDeliver() throws Exception {
        Fixture f = new Fixture();
        try {
            f.check.schedulePerSite(f.user, true);
            f.check.schedulePerSite(f.user, true);
            f.tasks.get(0).run();
            verify(f.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(f.uuid, false);
            f.tasks.get(1).run();
            verify(f.plugin.getVotingPluginUserManager()).getVotingPluginUser(f.uuid, false);
            Field generations = CoolDownCheck.class.getDeclaredField("perSiteGenerations");
            generations.setAccessible(true);
            assertFalse(((java.util.Map<?, ?>) generations.get(f.check)).containsKey(f.uuid));
        } finally { f.check.shutdown(); }
    }

    @Test void replacementWhileLoadingStopsOldTimerBeforeSiteCheck() throws Exception {
        Fixture f = new Fixture();
        try {
            f.check.schedulePerSite(f.user, true);
            var manager = f.plugin.getVotingPluginUserManager();
            doAnswer(call -> { f.check.schedulePerSite(f.user, true); return f.fresh; })
                .when(manager).getVotingPluginUser(f.uuid, false);
            f.tasks.get(0).run();
            verify(f.fresh, never()).getCoolDownCheckSiteList();
            assertEquals(2, f.tasks.size());
        } finally { f.check.shutdown(); }
    }

    @Test void shutdownRetiresAcceptedTimer() throws Exception {
        Fixture f = new Fixture();
        f.check.schedulePerSite(f.user, true);
        f.check.shutdown();
        f.tasks.get(0).run();
        verify(f.plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(f.uuid, false);
        f.check.schedulePerSite(f.user, true);
        assertEquals(1, f.tasks.size());
    }

    @Test void timerFailureLogsCorrelatedUncertainOutcomeAndPropagates() throws Exception {
        Fixture f = new Fixture();
        try {
            f.check.schedulePerSite(f.user, true);
            var data = f.fresh.getData();
            when(data.getString(eq("PlayerName"), nullable(com.bencodez.advancedcore.api.user.UserDataFetchMode.class))).thenThrow(new IllegalStateException("private detail"));
            f.tasks.get(0).run();
            f.tasks.get(1).run();
            f.tasks.get(2).run();
            IllegalStateException thrown = assertThrows(IllegalStateException.class, () -> f.tasks.get(3).run());
            assertEquals("private detail", thrown.getMessage());
            verify(f.plugin.getLogger()).warning(argThat((String message) -> message.contains("occurrence=")
                && message.contains("player=" + f.uuid) && message.contains("site=site")
                && message.contains("time=") && message.contains("outcome is uncertain")
                && !message.contains("private detail")));
        } finally { f.check.shutdown(); }
    }

    @Test void failedRefreshRetriesThenChecksWithoutReplaying() throws Exception {
        Fixture f = new Fixture();
        try {
            var data = f.fresh.getData();
            when(data.getString(eq("PlayerName"), nullable(com.bencodez.advancedcore.api.user.UserDataFetchMode.class)))
                .thenThrow(new IllegalStateException("unavailable")).thenReturn("CurrentName");
            f.check.schedulePerSite(f.user, true);
            f.tasks.get(0).run();
            verify(f.fresh, never()).getCoolDownCheckSiteList();
            assertEquals(2, f.tasks.size());
            verify(f.timer).schedule(any(Runnable.class), eq(30L), eq(TimeUnit.SECONDS));
            f.tasks.get(1).run();
            verify(f.fresh).setPlayerName("CurrentName");
            verify(f.fresh).getCoolDownCheckSiteList();
        } finally { f.check.shutdown(); }
    }

    @Test void replacedOrStoppedRetryCannotLoad() throws Exception {
        Fixture f = new Fixture();
        try {
            when(f.fresh.getData().getString(eq("PlayerName"), nullable(com.bencodez.advancedcore.api.user.UserDataFetchMode.class)))
                .thenThrow(new IllegalStateException("unavailable"));
            f.check.schedulePerSite(f.user, true);
            f.tasks.get(0).run();
            f.check.schedulePerSite(f.user, true);
            f.tasks.get(1).run();
            verify(f.plugin.getVotingPluginUserManager(), times(1)).getVotingPluginUser(f.uuid, false);
            f.check.shutdown();
            f.tasks.get(2).run();
            verify(f.plugin.getVotingPluginUserManager(), times(1)).getVotingPluginUser(f.uuid, false);
        } finally { f.check.shutdown(); }
    }

    @Test void retiredCacheFailureIsVisibleAndDoesNotChangeSiteFlags() throws Exception {
        Fixture f = new Fixture();
        try {
            f.check.schedulePerSite(f.user, true);
            when(f.fresh.getCoolDownCheckSiteList()).thenThrow(new IllegalStateException("cache retired"));
            assertThrows(IllegalStateException.class, () -> f.tasks.get(0).run());
            verify(f.fresh, never()).setCoolDownCheckSite(any());
            assertEquals(1, f.tasks.size());
            verify(f.plugin.getLogger()).warning(argThat((String message) -> message.contains("player=" + f.uuid)
                && message.contains("site=site") && message.contains("outcome is uncertain")
                && !message.contains("cache retired")));
        } finally { f.check.shutdown(); }
    }

    private static final class Fixture {
        final UUID uuid = UUID.randomUUID();
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final VotingPluginUser fresh = mock(VotingPluginUser.class);
        final VoteSite site = mock(VoteSite.class);
        final ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        final ArrayList<Runnable> tasks = new ArrayList<>();
        final CoolDownCheck check;
        Fixture() throws Exception {
            Config config = mock(Config.class);
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getConfigFile()).thenReturn(config);
            when(config.isPerSiteCoolDownEvents()).thenReturn(true);
            when(site.getKey()).thenReturn("site");
            when(user.getJavaUUID()).thenReturn(uuid);
            when(user.getPlayerName()).thenReturn("PriorName");
            when(user.getNextSiteAvailable()).thenReturn(new NextSite(site, 60));
            when(fresh.getData()).thenReturn(mock(UserData.class));
            when(fresh.getCoolDownCheckSiteList()).thenReturn(new HashMap<>(java.util.Map.of("site", true)));
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(fresh);
            doAnswer(call -> { tasks.add(call.getArgument(0)); return mock(ScheduledFuture.class); })
                .when(timer).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
            check = new CoolDownCheck(plugin);
            Field field = CoolDownCheck.class.getDeclaredField("timer");
            field.setAccessible(true);
            ((ScheduledExecutorService) field.get(check)).shutdownNow();
            field.set(check, timer);
        }
    }
}
