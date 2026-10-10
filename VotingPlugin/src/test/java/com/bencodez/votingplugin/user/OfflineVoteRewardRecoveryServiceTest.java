package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;

class OfflineVoteRewardRecoveryServiceTest {
	private static final UUID UUID_VALUE = UUID.fromString("77163a45-d3a9-4600-827e-f475b099c846");
	private VotingPluginMain plugin;
	private VotingPluginUser user;
	private OfflineVoteRewardRecoveryService service;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(UUID_VALUE, false)).thenReturn(user);
		when(plugin.getLogger()).thenReturn(mock(Logger.class));
		when(user.getPendingOfflineVoteRewardBatch()).thenReturn(new ArrayList<>(List.of("Site1")));
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>(List.of("Site1")));
		ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(worker);
		doAnswer(call -> {
			call.getArgument(0, Runnable.class).run();
			return null;
		}).when(worker).execute(any(Runnable.class));
		service = new OfflineVoteRewardRecoveryService(plugin);
	}

	@Test
	void previewIssuesSingleUseStateBoundTokenBeforeConsoleRecovery() {
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);

		assertTrue(result.get().contains("pending=1"));
		assertTrue(result.get().contains("prefixMatches=true"));
		String marker = "delivered ";
		int start = result.get().indexOf(marker) + marker.length();
		String token = result.get().substring(start, result.get().indexOf(' ', start));
		assertEquals(36, token.length());

		service.handle("Console", UUID_VALUE.toString(), "delivered", "wrong-token", result::set);
		assertTrue(result.get().contains("Invalid or expired"));
		verify(user, never()).reconcileOfflineVoteRewardBatch(any(), any(), any());

		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("recovery applied"));
		verify(user).reconcileOfflineVoteRewardBatch(eq(List.of("Site1")),
				eq(List.of("Site1")), eq("delivered"));

		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("Invalid or expired"));
	}

	@Test
	void activeDeliveryBlocksPreviewOrManualMutation() {
		when(user.isOfflineVoteRewardReplayActive()).thenReturn(true);
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);
		assertTrue(result.get().contains("delivery is active"));
		assertFalse(result.get().contains("delivered "));
		verify(user, never()).reconcileOfflineVoteRewardBatch(any(), any(), any());
	}

	@Test
	void alreadyRemovedPrefixCanBeReconciledWithoutDroppingNewVotes() {
		when(user.getOfflineVotes()).thenReturn(new ArrayList<>(List.of("NewVote")));
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);
		assertTrue(result.get().contains("prefixMatches=false"));

		String marker = "delivered ";
		int start = result.get().indexOf(marker) + marker.length();
		String token = result.get().substring(start, result.get().indexOf(' ', start));
		service.handle("Console", UUID_VALUE.toString(), "already-cleared", token, result::set);
		verify(user).reconcileOfflineVoteRewardBatch(eq(List.of("Site1")), eq(List.of("NewVote")),
				eq("already-cleared"));
	}

	@Test
	void missingOrInvalidUuidCannotMutateStorage() {
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", "invalid-uuid", "retry", "token", result::set);
		assertTrue(result.get().startsWith("Invalid UUID"));
		verify(user, never()).reconcileOfflineVoteRewardBatch(any(), any(), any());
	}
	@Test
	void sharedSqlRecoveryRequiresRepeatedIdenticalCommand() {
		when(plugin.getUserManager().getDataManager().hasSharedSqlBackend()).thenReturn(true);
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);
		String marker = "delivered ";
		int start = result.get().indexOf(marker) + marker.length();
		String token = result.get().substring(start, result.get().indexOf(' ', start));

		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("ALL backend servers"));
		verify(user, never()).reconcileOfflineVoteRewardBatch(any(), any(), any());

		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("recovery applied"));
		verify(user).reconcileOfflineVoteRewardBatch(eq(List.of("Site1")),
				eq(List.of("Site1")), eq("delivered"));
	}

	@Test
	void sharedSqlConfirmationCannotTransferActorOrPreviewToken() {
		when(plugin.getUserManager().getDataManager().hasSharedSqlBackend()).thenReturn(true);
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);
		String token = result.get().split("delivered ")[1].split(" ")[0];
		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		service.handle("Rcon", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("No changes made"));
		service.handle("Console", UUID_VALUE.toString(), "status", "", result::set);
		String replacement = result.get().split("delivered ")[1].split(" ")[0];
		service.handle("Console", UUID_VALUE.toString(), "delivered", token, result::set);
		assertTrue(result.get().contains("Invalid or expired"));
		service.handle("Console", UUID_VALUE.toString(), "delivered", replacement, result::set);
		assertTrue(result.get().contains("No changes made"));
		verify(user, never()).reconcileOfflineVoteRewardBatch(any(), any(), any());
		service.handle("Console", UUID_VALUE.toString(), "delivered", replacement, result::set);
		verify(user).reconcileOfflineVoteRewardBatch(any(), any(), eq("delivered"));
	}

}
