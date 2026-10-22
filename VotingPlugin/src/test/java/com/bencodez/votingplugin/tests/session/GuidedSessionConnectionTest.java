package com.bencodez.votingplugin.tests.session;

import com.bencodez.votingplugin.session.GuidedVotingSessions;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerQuitEvent;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.votesites.VoteSite;

class GuidedSessionConnectionTest {
    @Test void queuedOldCommandCannotDisplaceReopenedGuide() { checkRetiredConnection(false); }
    @Test void queuedOldAutomaticRefreshCannotDisplaceReopenedGuide() { checkRetiredConnection(true); }

    private void checkRetiredConnection(boolean automatic) {
        {
            var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            UUID uuid = UUID.randomUUID();
            Player old = mock(Player.class), current = mock(Player.class);
            var user = mock(VotingPluginUser.class);
            var site = mock(VoteSite.class);
            var timer = mock(ScheduledExecutorService.class);
            var config = new YamlConfiguration(); config.set("GuidedVotingSession.Enabled", true);
            when(plugin.getConfigFile().getData()).thenReturn(config);
            when(plugin.getOptions().isOnlineMode()).thenReturn(true);
            when(plugin.getTimer()).thenReturn(timer);
            when(plugin.getServer().getPlayer(uuid)).thenReturn(old);
            for (Player player : List.of(old, current)) {
                when(player.getUniqueId()).thenReturn(uuid);
                when(player.getName()).thenReturn("test-player");
                when(player.isOnline()).thenReturn(true);
                when(player.hasPermission(anyString())).thenReturn(true);
                when(player.spigot()).thenReturn(mock(Player.Spigot.class));
            }
            when(plugin.getCommandLoader().canUseGuidedSession(any(Player.class))).thenReturn(true);
            when(site.getKey()).thenReturn("site-a"); when(site.getDisplayName()).thenReturn("Site A");
            when(site.getPermissionToView()).thenReturn(""); when(site.isEnabled()).thenReturn(true);
            when(site.getVoteURL(true)).thenReturn("https://example.org/vote");
            when(plugin.getVoteSiteManager().getVoteSites()).thenReturn(List.of(site));
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, "test-player")).thenReturn(user);
            when(user.getLastVotes()).thenReturn(new java.util.HashMap<>());
            when(user.getJavaUUID()).thenReturn(uuid);
            when(user.getPlayerName()).thenReturn("test-player");
            when(user.canVoteSite(eq(site), anyLong())).thenReturn(true);
            var owners = new ArrayDeque<Runnable>(); var workers = new ArrayDeque<Runnable>();
            doAnswer(i -> { workers.add(i.getArgument(0)); return null; }).when(timer).execute(any());
            try (var scheduler = mockStatic(BukkitCompletionScheduler.class)) {
                scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), any(Player.class), any(Runnable.class),
                        any(Runnable.class), any(Runnable.class))).thenAnswer(i -> { owners.add(i.getArgument(2)); return null; });
                var sessions = SessionDialogViewAccess.sessions(plugin, () -> null);
                sessions.command(old, ""); owners.remove().run(); workers.remove().run(); owners.remove().run();
                if (automatic) {
                    var event = new PlayerPostVoteEvent(site, user, true, false, Long.MAX_VALUE, true,
                            "site-a", uuid, "test-player", UUID.randomUUID());
                    sessions.credited(event);
                } else sessions.command(old, "check");
                assertEquals(1, owners.size()); Runnable stale = owners.remove();
                sessions.disconnected(new PlayerQuitEvent(old, ""));
                when(plugin.getServer().getPlayer(uuid)).thenReturn(current);
                sessions.command(current, "check"); owners.remove().run(); workers.remove().run(); owners.remove().run();
                clearInvocations(old, current, user);
                stale.run();
                assertTrue(workers.isEmpty(), "stale callback admitted work for a retired connection: " + automatic);
                assertTrue(owners.isEmpty());
                verifyNoInteractions(current, user);
                verify(old, never()).sendMessage(anyString());
                sessions.clear();
            }
        }
    }
}
