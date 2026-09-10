package com.bencodez.votingplugin.commands;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.bukkit.command.CommandSender;
import org.junit.jupiter.api.Test;

class AdminAuthorizationTest {

	@Test
	void ordinaryRemovePointsPermissionDoesNotAuthorizeBulkRemoval() {
		CommandSender sender = mock(CommandSender.class);
		when(sender.hasPermission("VotingPlugin.Commands.AdminVote.RemovePoints")).thenReturn(true);

		assertFalse(AdminAuthorization.canRemovePointsFromAll(sender));
	}

	@Test
	void dedicatedOrAdminPermissionAuthorizesBulkRemoval() {
		CommandSender bulk = mock(CommandSender.class);
		when(bulk.hasPermission(AdminAuthorization.REMOVE_POINTS_ALL_PERMISSION)).thenReturn(true);
		CommandSender admin = mock(CommandSender.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);

		assertTrue(AdminAuthorization.canRemovePointsFromAll(bulk));
		assertTrue(AdminAuthorization.canRemovePointsFromAll(admin));
	}

	@Test
	void negativeBulkAddRequiresBulkRemovalAuthorization() {
		CommandSender ordinary = mock(CommandSender.class);
		CommandSender bulk = mock(CommandSender.class);
		when(bulk.hasPermission(AdminAuthorization.REMOVE_POINTS_ALL_PERMISSION)).thenReturn(true);
		CommandSender admin = mock(CommandSender.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);

		assertTrue(AdminAuthorization.canAddPointsToAll(ordinary, 100));
		assertFalse(AdminAuthorization.canAddPointsToAll(ordinary, -100));
		assertTrue(AdminAuthorization.canAddPointsToAll(bulk, -100));
		assertTrue(AdminAuthorization.canAddPointsToAll(admin, -100));
	}

	@Test
	void negativeBulkAddAdminOverrideHonorsMultiplePermissionPolicy() {
		CommandSender admin = mock(CommandSender.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);
		CommandSender dedicated = mock(CommandSender.class);
		when(dedicated.hasPermission(AdminAuthorization.REMOVE_POINTS_ALL_PERMISSION)).thenReturn(true);

		assertTrue(AdminAuthorization.canAddPointsToAll(admin, -100, true));
		assertFalse(AdminAuthorization.canAddPointsToAll(admin, -100, false));
		assertTrue(AdminAuthorization.canAddPointsToAll(dedicated, -100, false));
	}

	@Test
	void configEditorsRequireTheirOwnPermissionOrAdminOverride() {
		String permission = "VotingPlugin.Commands.AdminVote.Edit.SpecialRewards";
		CommandSender denied = mock(CommandSender.class);
		CommandSender editor = mock(CommandSender.class);
		when(editor.hasPermission(permission)).thenReturn(true);
		CommandSender admin = mock(CommandSender.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);

		assertFalse(AdminAuthorization.canEditConfig(denied, permission));
		assertTrue(AdminAuthorization.canEditConfig(editor, permission));
		assertTrue(AdminAuthorization.canEditConfig(admin, permission));
	}

	@Test
	void commandAdmissionAlwaysHonorsAdminOverride() {
		String permission = "VotingPlugin.Commands.AdminVote.AddPoints";
		CommandSender denied = mock(CommandSender.class);
		CommandSender command = mock(CommandSender.class);
		when(command.hasPermission(permission)).thenReturn(true);
		CommandSender admin = mock(CommandSender.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);

		assertFalse(AdminAuthorization.hasCommandOrAdmin(denied, permission));
		assertTrue(AdminAuthorization.hasCommandOrAdmin(command, permission));
		assertTrue(AdminAuthorization.hasCommandOrAdmin(admin, permission));
	}
}
