package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.signs.Signs;
import com.bencodez.votingplugin.streaks.VoteStreaksMenu;
import com.bencodez.votingplugin.topvoter.TopVoterDisplayFilter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import java.util.LinkedHashMap;
import java.util.HashSet;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import org.bukkit.event.HandlerList;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

class DisplayUnloadIntegrationTest {
    private void field(Object target, String name, Object value) throws Exception {
        var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(target, value);
    }
    private ScheduledExecutorService stoppedExecutor() throws Exception {
        var executor = mock(ScheduledExecutorService.class);
        when(executor.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(true);
        return executor;
    }
    @Test void unloadSettlesPendingLeaderboardEvidenceAndRejectsLaterStreakMenuOpens() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        field(plugin, "unloadInProgress", new java.util.concurrent.atomic.AtomicBoolean());
        var discord = mock(com.bencodez.votingplugin.discord.DiscordHandler.class);
        field(plugin, "discordHandler", discord);
        doReturn(true).when(plugin).isEnabled();
        var generation = new AtomicLong();
        field(plugin, "topVoterDisplayGeneration", generation);
        field(plugin, "placeholderPlayerPresence", new PlaceholderPlayerPresence());
        field(plugin, "backendHostedControlLifecycle", stoppedExecutor());
        field(plugin, "backendHostedControlManagers", new HashSet<>());
        field(plugin, "backendControlConnectorLifecycle", stoppedExecutor());
        field(plugin, "voteTimer", stoppedExecutor());
        var signs = mock(Signs.class); doReturn(signs).when(plugin).getSigns();
        field(plugin, "signs", signs);
        var pending = new CompletableFuture<PlayerPresence>();
        var filter = new TopVoterDisplayFilter(row -> pending);
        field(plugin, "topVoterDisplayFilter", filter);
        var menu = new VoteStreaksMenu(plugin); field(plugin, "voteStreaksMenu", menu);
        var rankings = new LinkedHashMap<TopVoterPlayer, Integer>();
        rankings.put(new TopVoterPlayer(UUID.randomUUID(), "Example", 1L), 10);
        var display = filter.projectAsync(rankings, true).toCompletableFuture();
        assertFalse(display.isDone());
        var player = mock(Player.class);
        try (var handlers = mockStatic(HandlerList.class); var scheduler = mockStatic(BukkitCompletionScheduler.class)) {
            scheduler.when(() -> BukkitCompletionScheduler.run(eq(plugin), eq(player), any(Runnable.class), any(Runnable.class), any(Runnable.class)))
                    .thenAnswer(call -> { call.getArgument(2, Runnable.class).run(); return null; });
            plugin.onUnLoad();
            verify(discord).close();
            verify(discord).retireLeaderboardPublications();
            assertTrue(display.isDone(), "Unload must settle the retired filter's pending display wait");
            assertEquals(rankings, display.getNow(null));
            pending.complete(PlayerPresence.NEVER_JOINED);
            assertEquals(rankings, filter.project(rankings, true), "Late history results cannot hide rows after retirement");
            assertEquals(1, generation.get());
            menu.open(player);
            verifyNoInteractions(player);
            verify(signs).storeSigns();
        }
    }
}
