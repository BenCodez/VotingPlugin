package com.bencodez.votingplugin.events;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.user.VotingPluginUser;

class VoteShopPurchaseEventTest {
	@Test
	void entityLanePurchaseCanBeMarkedSynchronousWithoutChangingTheLegacyConstructor() {
		UUID uuid = UUID.randomUUID();
		VotingPluginUser user = mock(VotingPluginUser.class);

		assertFalse(new VoteShopPurchaseEvent(uuid, "voter", user, "reward", 5, false).isAsynchronous());
		assertTrue(new VoteShopPurchaseEvent(uuid, "voter", user, "reward", 5).isAsynchronous());
	}
}
