package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BiConsumer;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;

class ExperimentalDialogsTest {
    static final class Backend implements ExperimentalDialogs.Backend {
        BiConsumer<UUID,String> callback;
        int shows, releases, closes;
        @Override public Runnable show(Player player, String title, String body, List<ExperimentalDialogs.Button> buttons,
                BiConsumer<UUID,String> callback) { this.callback = callback; shows++; return () -> releases++; }
        @Override public void close(Player player) { closes++; }
    }
    static final class Fixture {
        final UUID id = UUID.randomUUID();
        final AtomicLong clock = new AtomicLong();
        final ExperimentalSessions sessions = new ExperimentalSessions(clock::get, failure -> fail(failure));
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final Player player = mock(Player.class);
        final HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        final Queue<Runnable> owner = new ArrayDeque<>();
        final Backend backend = new Backend();
        final ExperimentalDialogs dialogs = new ExperimentalDialogs(plugin, scheduler, sessions, backend);
        final AtomicInteger actions = new AtomicInteger();
        ExperimentalSessions.Session session = sessions.open(id, ExperimentalGUIType.NATIVE_DIALOG, 64, 60);
        Fixture() {
            when(plugin.isEnabled()).thenReturn(true);
            when(player.isOnline()).thenReturn(true);
            when(player.hasPermission(anyString())).thenReturn(true);
            doAnswer(inv -> { owner.add(inv.getArgument(1)); return null; }).when(scheduler).player(any(), any(), any());
        }
        void render() { dialogs.render(session, player, "Voting", "Streak", List.of(
                new ExperimentalDialogs.Button("Vote", "Available", "https://example.test", null),
                new ExperimentalDialogs.Button("Next", "Navigation", null, "next")), action -> actions.incrementAndGet()); }
        void drain() { while (!owner.isEmpty()) owner.remove().run(); }
    }
    @Test void unrelatedOwnerIsIgnoredAndCorrectCallbackRunsOnceOnPlayerScheduler() {
        Fixture f = new Fixture(); f.render();
        f.backend.callback.accept(UUID.randomUUID(), "next");
        assertTrue(f.owner.isEmpty());
        f.backend.callback.accept(f.id, "next"); f.backend.callback.accept(f.id, "next");
        assertEquals(0, f.actions.get()); f.drain(); assertEquals(1, f.actions.get());
    }
    @Test void replacementFrameAndSessionFenceQueuedCallbacksWithoutClearingUnownedDialogs() {
        Fixture f = new Fixture(); f.render();
        var old = f.backend.callback; old.accept(f.id, "next");
        f.render(); assertEquals(1, f.backend.releases);
        f.drain(); assertEquals(0, f.actions.get()); old.accept(f.id, "next"); assertTrue(f.owner.isEmpty());
        f.session = f.sessions.open(f.id, ExperimentalGUIType.NATIVE_DIALOG, 64, 60);
        f.render(); f.drain(); assertEquals(0, f.backend.closes);
        f.sessions.close(f.session); f.drain(); assertEquals(0, f.backend.closes);
        assertEquals(3, f.backend.releases);
    }
    @Test void revocationTimeoutReloadAndRepeatedCleanupCannotExecuteActions() {
        Fixture f = new Fixture(); f.render();
        f.backend.callback.accept(f.id, "next");
        when(f.player.hasPermission(anyString())).thenReturn(false);
        f.drain(); assertEquals(0, f.actions.get());
        when(f.player.hasPermission(anyString())).thenReturn(true);
        f.clock.set(60_000_000_000L); f.backend.callback.accept(f.id, "next"); assertTrue(f.owner.isEmpty());
        f.sessions.clear(false); f.sessions.clear(false); f.drain();
        assertEquals(1, f.backend.releases); assertEquals(0, f.backend.closes);
        f.session = f.sessions.open(f.id, ExperimentalGUIType.NATIVE_DIALOG, 64, 60); f.render();
        f.sessions.clear(true); f.drain(); assertEquals(2, f.backend.releases);
    }
    @Test void urlAndPresentationBoundsFailClosed() {
        assertThrows(IllegalArgumentException.class, () -> new ExperimentalDialogs.Button("x", "x", "javascript:alert(1)", null));
        assertThrows(IllegalArgumentException.class, () -> new ExperimentalDialogs.Button("x", "x", null, null));
        assertThrows(IllegalArgumentException.class, () -> new ExperimentalDialogs.Button("x", "x", "https://example.test", "next"));
        Fixture f = new Fixture();
        var button = new ExperimentalDialogs.Button("x", "x", null, "next");
        assertThrows(IllegalArgumentException.class, () -> f.dialogs.render(f.session, f.player, "x", "x",
                java.util.Collections.nCopies(9, button), action -> fail()));
        assertEquals(0, f.backend.shows);
    }
}
