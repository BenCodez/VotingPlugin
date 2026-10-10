package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

import java.util.UUID;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

class HologramVoteSessionTest {
    @Test void sessionExpiryIsDeterministicAtBoundaryAndProtectsOwnerSnapshot() {
        Player player = mock(Player.class);
        UUID owner = UUID.randomUUID();
        org.mockito.Mockito.when(player.getUniqueId()).thenReturn(owner);
        HologramVoteMenu.Session session = new HologramVoteMenu.Session(player,
                new Location(mock(World.class), 1, 2, 3), new HologramVoteSettings(true, 2.5, 1, 5));
        assertFalse(session.expired(session.expiresAt - 1));
        assertTrue(session.expired(session.expiresAt));
        assertEquals(owner, session.owner);
        session.sites = java.util.List.of(new HologramVoteModel.Site("site", "Name", null, false, 0));
        assertEquals("site", session.sites.get(0).key());
    }
}
