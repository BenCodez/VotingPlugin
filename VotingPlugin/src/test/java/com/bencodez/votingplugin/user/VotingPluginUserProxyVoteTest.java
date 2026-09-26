package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.bukkit.Server;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.BungeeSettings;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class VotingPluginUserProxyVoteTest {
	@Test
	void backendDeliveryIsNotAcknowledgedWhenPostAdmissionProcessingFails() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BungeeSettings bungee = mock(BungeeSettings.class);
		VoteSiteManager sites = mock(VoteSiteManager.class);
		Server server = mock(Server.class);
		PluginManager pluginManager = mock(PluginManager.class);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(plugin.getBungeeSettings()).thenReturn(bungee);
		when(bungee.isUseBungeecoord()).thenReturn(true);
		when(plugin.getVoteSiteManager()).thenReturn(sites);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(pluginManager);
		when(base.getPlayerName()).thenReturn("Player");
		when(base.getUserData()).thenReturn(mock(UserData.class));
			doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setProcessingFailed(true);
			event.setReplayUnsafe(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		VotingPluginUser user = new VotingPluginUser(plugin, base);

		assertThrows(IllegalStateException.class, () -> user.bungeeVotePluginMessagingAccepted(
				"service", 100L, null, true, true, true, 1, UUID.randomUUID()));
	}
}
