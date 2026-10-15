package com.bencodez.votingplugin.hologram;

import static org.mockito.Mockito.*;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

class HologramMenuSchedulerTest {
    @Test void disabledPluginCleanupRunsInlineOnBukkitOwner() {
        try (var bukkit = mockStatic(Bukkit.class); var completion = mockStatic(BukkitCompletionScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            var player = mock(Player.class);
            var close = mock(Runnable.class);
            var retired = mock(Runnable.class);
            when(plugin.isEnabled()).thenReturn(false);
            bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
            new HologramMenuScheduler(plugin).player(player, close, retired);
            verify(close).run();
            verifyNoInteractions(retired);
            completion.verifyNoInteractions();
        }
    }

    @Test void workerCallbackStillUsesPlayerSchedulerInsteadOfInlineEntityAccess() {
        try (var bukkit = mockStatic(Bukkit.class); var completion = mockStatic(BukkitCompletionScheduler.class)) {
            var plugin = mock(VotingPluginMain.class);
            var player = mock(Player.class);
            var close = mock(Runnable.class);
            var retired = mock(Runnable.class);
            bukkit.when(Bukkit::isPrimaryThread).thenReturn(false);
            new HologramMenuScheduler(plugin).player(player, close, retired);
            verifyNoInteractions(close);
            completion.verify(() -> BukkitCompletionScheduler.run(plugin, player, close, retired, retired));
        }
    }
}
