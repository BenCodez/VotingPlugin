package com.bencodez.votingplugin.tests.backendproxy.presence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession;

public class BackendPlayerPresenceSessionTest {

	@Test
	public void testCreatesNormalizedSession() {
		BackendPlayerPresenceSession session = BackendPlayerPresenceSession.create(" Player ",
				"550e8400-e29b-41d4-a716-446655440000");

		assertNotNull(session);
		assertEquals("Player", session.getPlayerName());
		assertEquals("550e8400-e29b-41d4-a716-446655440000", session.getUuid());
		assertNotNull(session.getConnectionId());
	}

	@Test
	public void testRejectsInvalidIdentity() {
		assertNull(BackendPlayerPresenceSession.create("", "550e8400-e29b-41d4-a716-446655440000"));
		assertNull(BackendPlayerPresenceSession.create("Player", "not-a-uuid"));
	}

	@Test
	public void testPlayerKeysAreCaseInsensitiveAndTrimmed() {
		assertEquals(BackendPlayerPresenceSession.playerKey(" Player "),
				BackendPlayerPresenceSession.playerKey("player"));
		assertEquals("", BackendPlayerPresenceSession.playerKey(null));
	}

	@Test
	public void testNewConnectionsGetNewConnectionIds() {
		BackendPlayerPresenceSession first = BackendPlayerPresenceSession.create("Player",
				"550e8400-e29b-41d4-a716-446655440000");
		BackendPlayerPresenceSession second = BackendPlayerPresenceSession.create("Player",
				"550e8400-e29b-41d4-a716-446655440000");

		assertNotNull(first);
		assertNotNull(second);
		assertNotEquals(first.getConnectionId(), second.getConnectionId());
	}
}
