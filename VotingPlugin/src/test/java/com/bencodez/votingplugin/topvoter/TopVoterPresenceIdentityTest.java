package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.*;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import java.util.ArrayDeque;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.advancedcore.api.user.presence.PlayerPresenceService;
import org.junit.jupiter.api.Test;

class TopVoterPresenceIdentityTest {
    private static final UUID ONLINE = UUID.fromString("d4519fc4-a782-4d11-8235-617209238441");
    private UUID offline(String name) {
        return UUID.nameUUIDFromBytes(("OfflinePlayer:" + name).getBytes(StandardCharsets.UTF_8));
    }

    @Test void onlineIdentityKeepsExactOfflineMigrationEvidenceWithoutProfileLookup() {
        var row = new TopVoterPlayer(ONLINE, "Synthetic", 1L);
        var identities = TopVoterPresenceIdentity.identities(row, true, name -> "");
        assertEquals(java.util.List.of(ONLINE, offline("Synthetic")), identities);
        var tasks = new ArrayDeque<Runnable>();
        try (var service = new PlayerPresenceService(tasks::add,
                id -> id.equals(offline("Synthetic")) ? PlayerPresence.JOINED : PlayerPresence.NEVER_JOINED)) {
            var result = service.checkIdentitiesAsync(identities);
            while (!tasks.isEmpty()) tasks.remove().run();
            assertEquals(PlayerPresence.JOINED, result.toCompletableFuture().join());
        }
    }

    @Test void nativeOfflineUuidUsesExactNameAndAmbiguousNormalizedIdentityStaysUnknown() {
        var exact = new TopVoterPlayer(offline("Synthetic"), "Synthetic", 1L);
        assertEquals(java.util.List.of(exact.getUuid()), TopVoterPresenceIdentity.identities(exact, false, name -> ""));
        var normalized = new TopVoterPlayer(offline("synthetic"), "Synthetic", 1L);
        var aliases = TopVoterPresenceIdentity.identities(normalized, false, name -> normalized.getUuid().toString());
        assertEquals(normalized.getUuid(), aliases.getFirst()); assertTrue(aliases.contains(offline("Synthetic")));
        assertTrue(aliases.contains(null));
        try (var service = new PlayerPresenceService(Runnable::run, id -> PlayerPresence.NEVER_JOINED)) {
            assertEquals(PlayerPresence.UNKNOWN, service.checkIdentitiesAsync(aliases).toCompletableFuture().join());
        }
    }

    @Test void incompleteIdentityMappingsDoNotBecomeNegativeHistory() {
        var row = new TopVoterPlayer(ONLINE, "Synthetic", 1L);
        assertTrue(TopVoterPresenceIdentity.identities(row, true, name -> UUID.randomUUID().toString()).contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(row, true, name -> "1-1-1-1-1").contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(row, true, name -> { throw new IllegalStateException("synthetic"); }).contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(new TopVoterPlayer(offline("Synthetic"), "Synthetic", 1L), true, name -> "").contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(new TopVoterPlayer(new UUID(0, 1), "Synthetic", 1L), true, name -> "").contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(new TopVoterPlayer(
                UUID.fromString("00000000-0000-4000-0000-000000000001"), "Synthetic", 1L), true, name -> "").contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(null, true, name -> "").contains(null));
        assertTrue(TopVoterPresenceIdentity.identities(new TopVoterPlayer(ONLINE, " Synthetic ", 1L), true, name -> "").contains(null));
    }
}
