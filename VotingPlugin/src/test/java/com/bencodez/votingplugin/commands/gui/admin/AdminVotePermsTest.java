package com.bencodez.votingplugin.commands.gui.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;

class AdminVotePermsTest {
	@Test
	void listsDedicatedAllPermissionForPlayerCommands() {
		PlayerCommandHandler handler = mock(PlayerCommandHandler.class);
		when(handler.getPerm()).thenReturn("VotingPlugin.Commands.AdminVote.RemovePoints|VotingPlugin.Admin");

		assertEquals(Collections.singletonList("VotingPlugin.Commands.AdminVote.RemovePoints.All"),
				AdminVotePerms.additionalPermissions(handler));
	}

	@Test
	void ordinaryCommandsDoNotInventAdditionalPermissions() {
		assertEquals(Collections.emptyList(), AdminVotePerms.additionalPermissions(mock(CommandHandler.class)));
	}

	@Test
	void unrelatedPlayerCommandsDoNotAdvertiseUnusedAllPermission() {
		PlayerCommandHandler handler = mock(PlayerCommandHandler.class);
		when(handler.getPerm()).thenReturn("VotingPlugin.Commands.AdminVote.SetPoints|VotingPlugin.Admin");
		assertEquals(Collections.emptyList(), AdminVotePerms.additionalPermissions(handler));
	}
}
