package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;

class ExperimentalSessionsTest {
    final AtomicLong clock = new AtomicLong();
    final AtomicInteger errors = new AtomicInteger();
    final ExperimentalSessions sessions = new ExperimentalSessions(clock::get, failure -> errors.incrementAndGet());
    final UUID player = UUID.randomUUID();

    @Test void replacingEveryStyleInvalidatesOldCallbacksWithoutClosingSuccessor() {
        ExperimentalSessions.Session previous = null;
        AtomicInteger released = new AtomicInteger();
        for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
            var next = sessions.open(player, type, 1, 60);
            assertNotNull(next);
            next.own(released::incrementAndGet, failure -> fail(failure));
            assertTrue(next.activate());
            assertTrue(sessions.current(next));
            if (previous != null) {
                assertNotEquals(previous.id, next.id);
                assertFalse(sessions.current(previous));
                assertFalse(previous.activate());
                sessions.close(previous); // A late timeout must never remove the successor.
                assertTrue(sessions.current(next));
            }
            previous = next;
        }
        sessions.close(player);
        sessions.close(player);
        assertEquals(8, released.get());
        assertTrue(sessions.snapshot().isEmpty());
    }

    @Test void limitsDoNotPreventReplacementOrCrossPlayerIsolation() {
        var first = sessions.open(player, ExperimentalGUIType.NPC, 1, 60);
        UUID other = UUID.randomUUID();
        assertNull(sessions.open(other, ExperimentalGUIType.RADIAL, 1, 60));
        var replacement = sessions.open(player, ExperimentalGUIType.RADIAL, 1, 60);
        assertFalse(sessions.current(first));
        sessions.close(other);
        assertTrue(sessions.current(replacement));
    }

    @Test void timeoutIncludesExactBoundaryAndHandlesNanoTimeWrap() {
        clock.set(Long.MAX_VALUE - 500_000_000L);
        var menu = sessions.open(player, ExperimentalGUIType.HOLOGRAM, 64, 1);
        clock.addAndGet(999_999_999L);
        assertTrue(sessions.current(menu));
        clock.incrementAndGet();
        assertFalse(sessions.current(menu));
        sessions.expire();
        assertEquals(ExperimentalSessions.State.CLOSED, menu.state());
        assertTrue(sessions.snapshot().isEmpty());
    }

    @Test void partialInitializationAndLateResourcesAreCleanedOnceDespiteFailures() {
        var menu = sessions.open(player, ExperimentalGUIType.REWARD_SHOWCASE, 64, 60);
        AtomicInteger released = new AtomicInteger();
        menu.own(() -> { throw new IllegalStateException("fixture removal failure"); }, failure -> fail(failure));
        menu.own(released::incrementAndGet, failure -> fail(failure));
        sessions.close(menu);
        menu.own(released::incrementAndGet, failure -> fail(failure));
        sessions.close(menu);
        assertEquals(1, errors.get());
        assertEquals(2, released.get());
        assertFalse(menu.activate());
    }

    @Test void reloadAllowsNewSessionsButShutdownRejectsThem() {
        var old = sessions.open(player, ExperimentalGUIType.ANIMATED_INVENTORY, 64, 60);
        sessions.clear(false);
        assertFalse(sessions.current(old));
        var next = sessions.open(player, ExperimentalGUIType.STREAK_TRACK, 64, 60);
        assertNotNull(next);
        sessions.clear(true);
        assertFalse(sessions.current(next));
        assertNull(sessions.open(player, ExperimentalGUIType.NATIVE_DIALOG, 64, 60));
    }

    @Test void repeatedOpenCloseDoesNotRetainResourcesOrSessions() {
        AtomicInteger released = new AtomicInteger();
        for (int i = 0; i < 1_000; i++) {
            var menu = sessions.open(player, ExperimentalGUIType.VOTING_TERMINAL, 64, 60);
            menu.own(released::incrementAndGet, failure -> fail(failure));
            sessions.close(menu);
            assertTrue(sessions.snapshot().isEmpty());
        }
        assertEquals(1_000, released.get());
    }

    @Test void resourceAndSettingBoundsAreEnforced() {
        assertThrows(IllegalArgumentException.class,
                () -> sessions.open(player, ExperimentalGUIType.NPC, 65, 60));
        assertThrows(IllegalArgumentException.class,
                () -> sessions.open(player, ExperimentalGUIType.NPC, 64, 61));
        var menu = sessions.open(player, ExperimentalGUIType.NPC, 64, 60);
        for (int i = 0; i < 128; i++) menu.own(() -> {}, failure -> fail(failure));
        AtomicInteger rejectedResource = new AtomicInteger();
        assertThrows(IllegalStateException.class,
                () -> menu.own(rejectedResource::incrementAndGet, failure -> fail(failure)));
        assertEquals(1, rejectedResource.get());
    }
}
