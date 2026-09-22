package com.bencodez.votingplugin.listeners;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerQuitEvent;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;

class PlayerJoinEventPresenceTest {
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
		Player player = mock(Player.class);
		when(player.getUniqueId()).thenReturn(uuid);
		org.bukkit.event.player.PlayerJoinEvent join = mock(org.bukkit.event.player.PlayerJoinEvent.class);
		when(join.getPlayer()).thenReturn(player);
		PlayerQuitEvent quit = mock(PlayerQuitEvent.class);
		when(quit.getPlayer()).thenReturn(player);
		PlayerJoinEvent listener = new PlayerJoinEvent(plugin);

		listener.onPlayerJoin(join);
		assertTrue(presence.isOnline(uuid));
		listener.onPlayerQuit(quit);
		assertFalse(presence.isOnline(uuid), "quit must publish offline before asynchronous storage cleanup");
	}
}
