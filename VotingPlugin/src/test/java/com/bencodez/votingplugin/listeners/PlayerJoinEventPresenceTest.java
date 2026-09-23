package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerQuitEvent;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class PlayerJoinEventPresenceTest {
	@Test
	void advancedCoreLoginRekeysPresenceToTheAuthoritativeStorageUuid() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.isMySQLOkay()).thenReturn(true);
		BungeeSettings bungee = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		Player player = mock(Player.class);
		UUID playerUuid = UUID.randomUUID();
		UUID storageUuid = UUID.randomUUID();
		when(player.getUniqueId()).thenReturn(playerUuid);
		presence.playerOnline(playerUuid, player);

		UserManager userManager = mock(UserManager.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getJavaUUID()).thenReturn(storageUuid);
		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(userManager.getVotingPluginUser(storageUuid.toString())).thenReturn(user);
		PlaceHolders placeholders = mock(PlaceHolders.class);
		when(plugin.getPlaceholders()).thenReturn(placeholders);
		doAnswer(call -> {
			assertTrue(presence.isOnline(storageUuid));
			assertFalse(presence.isOnline(playerUuid));
			return null;
		}).when(placeholders).onUpdate(user, true);

		com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent login =
				mock(com.bencodez.advancedcore.listeners.AdvancedCoreLoginEvent.class);
		when(login.getUuid()).thenReturn(storageUuid.toString());
		when(login.getPlayer()).thenReturn(player);
		when(login.getUser()).thenReturn(mock(com.bencodez.advancedcore.api.user.AdvancedCoreUser.class));

		new PlayerJoinEvent(plugin).onPlayerLogin(login);

		assertTrue(presence.isOnline(storageUuid));
		assertFalse(presence.isOnline(playerUuid));
	}

	@Test
	void bukkitJoinAndQuitUpdatePresenceBeforeAsyncQuitCleanup() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.isEnabled()).thenReturn(true);
		BungeeSettings bungee = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		when(bungee.isUseBungeecoord()).thenReturn(false);
		ScheduledExecutorService loginTimer = mock(ScheduledExecutorService.class);
		when(plugin.getLoginTimer()).thenReturn(loginTimer);
		doAnswer(call -> null).when(loginTimer).execute(any(Runnable.class));

		UUID uuid = UUID.randomUUID();
		UUID storageUuid = UUID.randomUUID();
		Player player = mock(Player.class);
		when(player.getUniqueId()).thenReturn(uuid);
		UserManager userManager = mock(UserManager.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(userManager.getVotingPluginUser(player)).thenReturn(user);
		when(user.getJavaUUID()).thenReturn(storageUuid);
		org.bukkit.event.player.PlayerJoinEvent join = mock(org.bukkit.event.player.PlayerJoinEvent.class);
		when(join.getPlayer()).thenReturn(player);
		PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
		when(quit.getPlayer()).thenReturn(player);
		PlayerJoinEvent listener = new PlayerJoinEvent(plugin);

		listener.onPlayerJoin(join);
		assertTrue(presence.isOnline(storageUuid));
		assertFalse(presence.isOnline(uuid));
		listener.onPlayerQuit(quit);
		assertFalse(presence.isOnline(storageUuid),
				"quit must publish the storage UUID offline before asynchronous storage cleanup");
	}

	@Test
	void staleQuitCannotRetireOrClearAReplacementOwner() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlaceholderPlayerPresence presence = new PlaceholderPlayerPresence();
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.isEnabled()).thenReturn(true);
		BungeeSettings bungee = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		ScheduledExecutorService loginTimer = mock(ScheduledExecutorService.class);
		when(plugin.getLoginTimer()).thenReturn(loginTimer);
		doAnswer(call -> null).when(loginTimer).execute(any(Runnable.class));
		PlaceHolders placeholders = mock(PlaceHolders.class);
		when(plugin.getPlaceholders()).thenReturn(placeholders);
		UserManager userManager = mock(UserManager.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		UUID storageUuid = UUID.randomUUID();
		Player retired = mock(Player.class);
		Player replacement = mock(Player.class);
		when(retired.getUniqueId()).thenReturn(UUID.randomUUID());
		VotingPluginUser retiredUser = mock(VotingPluginUser.class);
		when(retiredUser.getJavaUUID()).thenReturn(storageUuid);
		when(userManager.getVotingPluginUser(retired)).thenReturn(retiredUser);
		presence.playerOnline(storageUuid, retired);
		presence.playerOnline(storageUuid, replacement);
		PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
		when(quit.getPlayer()).thenReturn(retired);

		new PlayerJoinEvent(plugin).onPlayerQuit(quit);

		assertTrue(presence.isOnline(storageUuid));
		verify(placeholders, never()).onLogout(storageUuid);
	}

	@Test
	void reconnectBetweenRetirementAndCleanupPreservesReplacementCaches() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		PlaceholderPlayerPresence presence = mock(PlaceholderPlayerPresence.class);
		when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
		when(plugin.isEnabled()).thenReturn(true);
		BungeeSettings bungee = mock(BungeeSettings.class);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		ScheduledExecutorService loginTimer = mock(ScheduledExecutorService.class);
		when(plugin.getLoginTimer()).thenReturn(loginTimer);
		doAnswer(call -> null).when(loginTimer).execute(any(Runnable.class));
		PlaceHolders placeholders = mock(PlaceHolders.class);
		when(plugin.getPlaceholders()).thenReturn(placeholders);
		UUID storageUuid = UUID.randomUUID();
		Player retired = mock(Player.class);
		when(presence.storageUuid(retired)).thenReturn(storageUuid);
		when(presence.playerOffline(storageUuid, retired)).thenReturn(true);
		when(presence.runIfOffline(eq(storageUuid), any(Runnable.class))).thenReturn(false);
		PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
		when(quit.getPlayer()).thenReturn(retired);

		new PlayerJoinEvent(plugin).onPlayerQuit(quit);

		verify(presence).runIfOffline(eq(storageUuid), any(Runnable.class));
		verify(placeholders, never()).onLogout(storageUuid);
	}
}
