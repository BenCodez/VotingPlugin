package com.bencodez.votingplugin.commands;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.bukkit.permissions.Permission;
import org.bukkit.permissions.PermissionDefault;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.command.PlayerCommandHandler;

class CommandLoaderBulkPermissionTest {
	@Test
	void registersBulkPermissionAsDefaultFalseWithoutImplicitPermissionChildren() {
		PluginManager manager = mock(PluginManager.class);
		PlayerCommandHandler handler = mock(PlayerCommandHandler.class);
		when(handler.getAdditionalPermissions())
				.thenReturn(List.of("VotingPlugin.Commands.AdminVote.SetPoints.All"));
		when(handler.getAllPermissionOverrides()).thenReturn(List.of(AdminAuthorization.ADMIN_PERMISSION));
		when(manager.getPermission("VotingPlugin.Commands.AdminVote.SetPoints.All")).thenReturn(null);

		CommandLoader.registerAdditionalPermissions(manager, handler);

		ArgumentCaptor<Permission> registered = ArgumentCaptor.forClass(Permission.class);
		verify(manager).addPermission(registered.capture());
		assertEquals("VotingPlugin.Commands.AdminVote.SetPoints.All", registered.getValue().getName());
		assertEquals(PermissionDefault.FALSE, registered.getValue().getDefault());
		verify(manager, never()).getPermission("VotingPlugin.Commands.AdminVote.SetPoints");
		verify(manager, never()).getPermission(AdminAuthorization.ADMIN_PERMISSION);
	}
}
