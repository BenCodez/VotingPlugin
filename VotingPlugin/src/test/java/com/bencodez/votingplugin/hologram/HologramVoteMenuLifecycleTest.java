package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.UUID;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerChangedWorldEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;

class HologramVoteMenuLifecycleTest {
    @SuppressWarnings("unchecked")
    private static Map<UUID, HologramVoteMenu.Session> sessions(HologramVoteMenu menu) throws Exception {
        Field field = HologramVoteMenu.class.getDeclaredField("sessions");
        field.setAccessible(true);
        return (Map<UUID, HologramVoteMenu.Session>) field.get(menu);
    }

    @Test void quitAndWorldChangeOwnOnlyTheMatchingPlayerSession() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(invocation -> { invocation.getArgument(1, Runnable.class).run(); return null; })
                .when(scheduler).region(any(), any());
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler);
        Player first = mock(Player.class), second = mock(Player.class);
        UUID firstId = UUID.randomUUID(), secondId = UUID.randomUUID();
        when(first.getUniqueId()).thenReturn(firstId);
        when(second.getUniqueId()).thenReturn(secondId);
        HologramVoteSettings settings = new HologramVoteSettings(true, 2.5, 60, 5);
        HologramVoteMenu.Session firstSession = new HologramVoteMenu.Session(first,
                new Location(mock(World.class), 0, 64, 0), settings);
        HologramVoteMenu.Session secondSession = new HologramVoteMenu.Session(second,
                new Location(mock(World.class), 0, 64, 0), settings);
        Entity firstEntity = mock(Entity.class), secondEntity = mock(Entity.class);
        when(firstEntity.getUniqueId()).thenReturn(UUID.randomUUID());
        when(secondEntity.getUniqueId()).thenReturn(UUID.randomUUID());
        firstSession.entities.add(firstEntity);
        secondSession.entities.add(secondEntity);
        sessions(menu).put(firstId, firstSession);
        sessions(menu).put(secondId, secondSession);

        PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
        when(quit.getPlayer()).thenReturn(first);
        menu.quit(quit);
        assertTrue(firstSession.closed.get());
        assertFalse(secondSession.closed.get());
        verify(firstEntity).remove();
        verify(secondEntity, never()).remove();

        PlayerChangedWorldEvent changed = mock(PlayerChangedWorldEvent.class);
        when(changed.getPlayer()).thenReturn(second);
        menu.changedWorld(changed);
        assertTrue(secondSession.closed.get());
        verify(secondEntity).remove();
    }

    @Test void clearIsIdempotentAndShutdownPreventsLaterOwnership() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(invocation -> { invocation.getArgument(1, Runnable.class).run(); return null; })
                .when(scheduler).region(any(), any());
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler);
        Player player = mock(Player.class);
        UUID id = UUID.randomUUID();
        when(player.getUniqueId()).thenReturn(id);
        HologramVoteMenu.Session session = new HologramVoteMenu.Session(player,
                new Location(mock(World.class), 0, 64, 0), new HologramVoteSettings(true, 2.5, 60, 5));
        Entity entity = mock(Entity.class);
        when(entity.getUniqueId()).thenReturn(UUID.randomUUID());
        session.entities.add(entity);
        sessions(menu).put(id, session);
        menu.clear();
        menu.clear();
        verify(entity, times(1)).remove();
        assertTrue(session.closed.get());
        menu.shutdown();
        assertTrue(sessions(menu).isEmpty());
    }
}
