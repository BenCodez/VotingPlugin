package com.bencodez.votingplugin.commands.gui.admin;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.Server;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

class AdminVoteVotePlayerTest {
	@Test
	void dispatchesFromPlayerThreadBeforeVoteWorkerAdmission() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Server server = mock(Server.class);
		PluginManager pluginManager = mock(PluginManager.class);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		Player player = mock(Player.class);
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "example.org", false);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(pluginManager);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);

		new AdminVoteVotePlayer(plugin, player, "Steve").dispatchVote(player, event);

		verify(pluginManager).callEvent(event);
		verify(voteTimer, never()).submit(any(Runnable.class));
	}
}
