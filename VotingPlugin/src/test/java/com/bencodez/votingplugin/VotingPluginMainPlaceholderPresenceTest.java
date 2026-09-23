package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.player.UuidLookup;

class VotingPluginMainPlaceholderPresenceTest {
	@Test
	void cachedStorageIdentityWinsDuringColdPresenceRefresh() {
		Player player = mock(Player.class);
		UuidLookup lookup = mock(UuidLookup.class);
		UUID bukkitUuid = UUID.randomUUID();
		UUID storageUuid = UUID.randomUUID();
		when(player.getName()).thenReturn("Player");
		when(player.getUniqueId()).thenReturn(bukkitUuid);
		when(lookup.getCachedUUID("Player")).thenReturn(storageUuid.toString());

		try (var uuidLookup = mockStatic(UuidLookup.class)) {
			uuidLookup.when(UuidLookup::getInstance).thenReturn(lookup);
			assertEquals(storageUuid, VotingPluginMain.placeholderStorageUuid(player));
		}
	}

	@Test
	void bukkitIdentityIsUsedWhenNoStorageIdentityWasCaptured() {
		Player player = mock(Player.class);
		UuidLookup lookup = mock(UuidLookup.class);
		UUID bukkitUuid = UUID.randomUUID();
		when(player.getName()).thenReturn("Player");
		when(player.getUniqueId()).thenReturn(bukkitUuid);
		when(lookup.getCachedUUID("Player")).thenReturn("");

		try (var uuidLookup = mockStatic(UuidLookup.class)) {
			uuidLookup.when(UuidLookup::getInstance).thenReturn(lookup);
			assertEquals(bukkitUuid, VotingPluginMain.placeholderStorageUuid(player));
		}
	}
}
