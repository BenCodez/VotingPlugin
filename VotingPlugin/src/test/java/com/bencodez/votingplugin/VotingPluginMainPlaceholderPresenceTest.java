package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

import java.util.UUID;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;

class VotingPluginMainPlaceholderPresenceTest {
	@Test
	void periodicUserRefreshUsesStorageWorkerInsteadOfVoteAdmissionExecutor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		java.util.concurrent.ScheduledExecutorService votes = mock(java.util.concurrent.ScheduledExecutorService.class);
		UserManager userManager = mock(UserManager.class);
		UserDataManager dataManager = mock(UserDataManager.class);
		doReturn(userManager).when(plugin).getUserManager();
		when(userManager.getDataManager()).thenReturn(dataManager);
		when(dataManager.getTimer()).thenReturn(storage);
		doReturn(votes).when(plugin).getVoteTimer();
		doAnswer(call -> {
			call.getArgument(0, java.util.function.Consumer.class).accept(java.util.Map.of());
			return null;
		}).when(plugin).captureOnlineTopVoterIgnore(any());

		plugin.basicBungeeUpdate();

		verify(storage).execute(any(Runnable.class));
		verify(votes, never()).execute(any(Runnable.class));
	}
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
			assertEquals(storageUuid, VotingPluginMain.placeholderStorageUuid(player, false));
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
			assertEquals(bukkitUuid, VotingPluginMain.placeholderStorageUuid(player, false));
		}
	}

	@Test
	void onlineModeIgnoresAStaleCachedNameMapping() {
		Player player = mock(Player.class);
		UuidLookup lookup = mock(UuidLookup.class);
		UUID bukkitUuid = UUID.randomUUID();
		when(player.getUniqueId()).thenReturn(bukkitUuid);

		try (var uuidLookup = mockStatic(UuidLookup.class)) {
			uuidLookup.when(UuidLookup::getInstance).thenReturn(lookup);
			assertEquals(bukkitUuid, VotingPluginMain.placeholderStorageUuid(player, true));
		}
	}
}
