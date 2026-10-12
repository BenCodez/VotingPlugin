package com.bencodez.votingplugin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.UserManager;
import com.bencodez.advancedcore.api.user.usercache.UserDataManager;

class VotingPluginMainPlaceholderPresenceTest {
	@Test
	void periodicUserRefreshUsesStorageWorkerInsteadOfVoteAdmissionExecutor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		doReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class, RETURNS_DEEP_STUBS)).when(plugin).getOptions();
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
		}).when(plugin).captureOnlineTopVoterIgnore(any(), any());

		plugin.basicBungeeUpdate();

		verify(storage).execute(any(Runnable.class));
		verify(votes, never()).execute(any(Runnable.class));
	}

	@Test
	void periodicUserRefreshKeepsOnlyOneStorageJobInFlight() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		doReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class, RETURNS_DEEP_STUBS)).when(plugin).getOptions();
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		UserManager userManager = mock(UserManager.class);
		UserDataManager dataManager = mock(UserDataManager.class);
		doReturn(userManager).when(plugin).getUserManager();
		when(userManager.getDataManager()).thenReturn(dataManager);
		when(dataManager.getTimer()).thenReturn(storage);
		doAnswer(call -> {
			call.getArgument(0, java.util.function.Consumer.class).accept(java.util.Map.of());
			return null;
		}).when(plugin).captureOnlineTopVoterIgnore(any(), any());
		ArgumentCaptor<Runnable> refresh = ArgumentCaptor.forClass(Runnable.class);

		plugin.basicBungeeUpdate();
		plugin.basicBungeeUpdate();
		verify(plugin, times(1)).captureOnlineTopVoterIgnore(any(), any());
		verify(storage).execute(refresh.capture());

		refresh.getValue().run();
		plugin.basicBungeeUpdate();
		verify(plugin, times(2)).captureOnlineTopVoterIgnore(any(), any());
	}

	@Test
	void periodicUserRefreshKeepsAdmissionUntilReplayAndGenericRewardsComplete() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		doReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class, RETURNS_DEEP_STUBS)).when(plugin).getOptions();
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		com.bencodez.votingplugin.user.UserManager users = mock(com.bencodez.votingplugin.user.UserManager.class);
		com.bencodez.advancedcore.api.user.UserManager coreUsers = mock(com.bencodez.advancedcore.api.user.UserManager.class);
		com.bencodez.advancedcore.api.user.usercache.UserDataManager data =
				mock(com.bencodez.advancedcore.api.user.usercache.UserDataManager.class);
		com.bencodez.votingplugin.user.VotingPluginUser user = mock(com.bencodez.votingplugin.user.VotingPluginUser.class);
		UUID uuid = UUID.randomUUID();
		CompletableFuture<Void> replay = new CompletableFuture<>();
		ArgumentCaptor<Runnable> refresh = ArgumentCaptor.forClass(Runnable.class);
		doReturn(users).when(plugin).getVotingPluginUserManager();
		doReturn(coreUsers).when(plugin).getUserManager();
		when(coreUsers.getDataManager()).thenReturn(data);
		when(data.getTimer()).thenReturn(storage);
		when(users.getVotingPluginUser(uuid, false)).thenReturn(user);
		when(user.offVoteWithCapturedTopVoterIgnoreAsync(false)).thenReturn(replay);
		doAnswer(call -> {
			call.getArgument(0, java.util.function.Consumer.class).accept(java.util.Map.of(uuid, false));
			return null;
		}).when(plugin).captureOnlineTopVoterIgnore(any(), any());

		plugin.basicBungeeUpdate();
		verify(storage).execute(refresh.capture());
		refresh.getValue().run();
		plugin.basicBungeeUpdate();
		verify(plugin, times(1)).captureOnlineTopVoterIgnore(any(), any());
		verify(user, never()).checkOfflineRewards();

		replay.complete(null);
		verify(user).checkOfflineRewards();
		plugin.basicBungeeUpdate();
		verify(plugin, times(2)).captureOnlineTopVoterIgnore(any(), any());
	}

	@Test
	void periodicRefreshKeepsEarlierReplayOwnedWhenLaterUserPreparationFails() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		doReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class, RETURNS_DEEP_STUBS)).when(plugin).getOptions();
		java.util.concurrent.ScheduledExecutorService storage = mock(java.util.concurrent.ScheduledExecutorService.class);
		com.bencodez.votingplugin.user.UserManager users = mock(com.bencodez.votingplugin.user.UserManager.class);
		com.bencodez.advancedcore.api.user.UserManager coreUsers = mock(com.bencodez.advancedcore.api.user.UserManager.class);
		com.bencodez.advancedcore.api.user.usercache.UserDataManager data =
				mock(com.bencodez.advancedcore.api.user.usercache.UserDataManager.class);
		com.bencodez.votingplugin.user.VotingPluginUser first = mock(com.bencodez.votingplugin.user.VotingPluginUser.class);
		com.bencodez.votingplugin.user.VotingPluginUser second = mock(com.bencodez.votingplugin.user.VotingPluginUser.class);
		UUID firstUuid = UUID.randomUUID();
		UUID secondUuid = UUID.randomUUID();
		CompletableFuture<Void> replay = new CompletableFuture<>();
		ArgumentCaptor<Runnable> refresh = ArgumentCaptor.forClass(Runnable.class);
		doReturn(users).when(plugin).getVotingPluginUserManager();
		doReturn(coreUsers).when(plugin).getUserManager();
		when(coreUsers.getDataManager()).thenReturn(data);
		when(data.getTimer()).thenReturn(storage);
		when(users.getVotingPluginUser(firstUuid, false)).thenReturn(first);
		when(users.getVotingPluginUser(secondUuid, false)).thenReturn(second);
		doThrow(new IllegalStateException("later user preparation failed")).when(second).cache();
		when(first.offVoteWithCapturedTopVoterIgnoreAsync(false)).thenReturn(replay);
		doAnswer(call -> {
			java.util.Map<UUID, Boolean> captured = new java.util.LinkedHashMap<>();
			captured.put(firstUuid, false);
			captured.put(secondUuid, false);
			call.getArgument(0, java.util.function.Consumer.class).accept(captured);
			return null;
		}).when(plugin).captureOnlineTopVoterIgnore(any(), any());

		plugin.basicBungeeUpdate();
		verify(storage).execute(refresh.capture());
		refresh.getValue().run();
		plugin.basicBungeeUpdate();
		verify(plugin, times(1)).captureOnlineTopVoterIgnore(any(), any());
		verify(first, never()).checkOfflineRewards();

		replay.complete(null);
		verify(first).checkOfflineRewards();
		plugin.basicBungeeUpdate();
		verify(plugin, times(2)).captureOnlineTopVoterIgnore(any(), any());
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
