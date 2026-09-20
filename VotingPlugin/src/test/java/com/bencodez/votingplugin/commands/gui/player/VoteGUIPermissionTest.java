package com.bencodez.votingplugin.commands.gui.player;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VoteGUIPermissionTest {

	@Test
	void ownGuiUsesVotingPluginIdentityWhenOfflineUuidDiffersFromBukkitUuid() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		Player player = mock(Player.class);
		VotingPluginUser viewingUser = mock(VotingPluginUser.class);
		VotingPluginUser viewedUser = mock(VotingPluginUser.class);
		UUID bukkitUuid = UUID.fromString("11111111-1111-1111-1111-111111111111");
		String offlineUuid = "22222222-2222-2222-2222-222222222222";

		when(player.getUniqueId()).thenReturn(bukkitUuid);
		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(userManager.getVotingPluginUser(player)).thenReturn(viewingUser);
		when(viewingUser.getUUID()).thenReturn(offlineUuid);
		when(viewedUser.getUUID()).thenReturn(offlineUuid);

		assertNotEquals(player.getUniqueId().toString(), viewedUser.getUUID());
		assertEquals("VotingPlugin.Commands.Vote.GUI",
				VoteGUI.getRequiredPermission(plugin, player, viewedUser));
	}

	@Test
	void nullUserUuidsNeverGrantSelfGuiPermission() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		Player player = mock(Player.class);
		VotingPluginUser viewingUser = mock(VotingPluginUser.class);
		VotingPluginUser viewedUser = mock(VotingPluginUser.class);

		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(userManager.getVotingPluginUser(player)).thenReturn(viewingUser);
		when(viewingUser.getUUID()).thenReturn(null);
		when(viewedUser.getUUID()).thenReturn(null);

		assertEquals("VotingPlugin.Commands.Vote.GUI.Other",
				VoteGUI.getRequiredPermission(plugin, player, viewedUser));
	}

	@Test
	void otherPlayerGuiStillRequiresOtherPermission() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager userManager = mock(UserManager.class);
		Player player = mock(Player.class);
		VotingPluginUser viewingUser = mock(VotingPluginUser.class);
		VotingPluginUser viewedUser = mock(VotingPluginUser.class);

		when(plugin.getVotingPluginUserManager()).thenReturn(userManager);
		when(userManager.getVotingPluginUser(player)).thenReturn(viewingUser);
		when(viewingUser.getUUID()).thenReturn("22222222-2222-2222-2222-222222222222");
		when(viewedUser.getUUID()).thenReturn("33333333-3333-3333-3333-333333333333");

		assertEquals("VotingPlugin.Commands.Vote.GUI.Other",
				VoteGUI.getRequiredPermission(plugin, player, viewedUser));
	}
}
