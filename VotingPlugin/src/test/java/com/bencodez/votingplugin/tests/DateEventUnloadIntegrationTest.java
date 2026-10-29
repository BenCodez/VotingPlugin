package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.streaks.VoteStreaksMenu;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import java.util.HashSet;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.bukkit.event.HandlerList;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

class DateEventUnloadIntegrationTest {
    private void field(Object target, String name, Object value) throws Exception {
        var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(target, value);
    }
    private ScheduledExecutorService stoppedExecutor() throws Exception {
        var executor = mock(ScheduledExecutorService.class);
        when(executor.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(true);
        return executor;
    }
    @Test void unloadClosesDateIngressAndRejectsLaterStreakMenuOpens() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        doReturn(true).when(plugin).isEnabled();
        field(plugin, "unloadInProgress", new java.util.concurrent.atomic.AtomicBoolean());
        var listener = mock(com.bencodez.votingplugin.listeners.VotiferEvent.class);
        field(plugin, "localVotifierListener", listener);
        field(plugin, "localVotifierIngressReady", true);
        field(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        field(plugin, "backendHostedControlLifecycle", stoppedExecutor());
        field(plugin, "backendHostedControlManagers", new HashSet<>());
        field(plugin, "backendControlConnectorLifecycle", stoppedExecutor());
        field(plugin, "voteTimer", stoppedExecutor());
        var signs = mock(Signs.class);
        when(signs.storeSignsForShutdown()).thenReturn(java.util.concurrent.CompletableFuture.completedFuture(null));
        field(plugin, "signs", signs);
        var menu = new VoteStreaksMenu(plugin); field(plugin, "voteStreaksMenu", menu);
        var player = mock(Player.class);
        try (var handlers = mockStatic(HandlerList.class); var scheduler = mockStatic(BukkitCompletionScheduler.class)) {
            scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), eq(player), any(Runnable.class), any(Runnable.class), any(Runnable.class)))
                    .thenAnswer(call -> { call.getArgument(2, Runnable.class).run(); return null; });
            plugin.onUnLoad();
            verify(listener).stop();
            var proxy = mock(Runnable.class);
            assertThrows(IllegalStateException.class, () -> DateVoteIngressAccess.initializeDateVoteIngress(plugin, proxy));
            verifyNoInteractions(proxy);
            menu.open(player);
            verifyNoInteractions(player);
            verify(signs).storeSignsForShutdown();
        }
    }
}
