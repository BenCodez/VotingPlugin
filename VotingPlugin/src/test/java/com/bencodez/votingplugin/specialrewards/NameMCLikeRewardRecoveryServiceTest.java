package com.bencodez.votingplugin.specialrewards;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

class NameMCLikeRewardRecoveryServiceTest {
	private static final UUID PLAYER_UUID = UUID.fromString("77163a45-d3a9-4600-827e-f475b099c846");

	private VotingPluginMain plugin;
	private VotingPluginUser user;
	private NameMCLikeRewardRecoveryService service;
	private Set<UUID> inFlight;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		user = mock(VotingPluginUser.class);
		inFlight = new HashSet<>();
		NameMCLikeCheckerTask checker = mock(NameMCLikeCheckerTask.class);
		when(checker.getInFlight()).thenReturn(inFlight);
		when(plugin.getNameMCLikeCheckerTask()).thenReturn(checker);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(PLAYER_UUID, false)).thenReturn(user);
		when(plugin.getLogger()).thenReturn(mock(Logger.class));
		when(user.isNameMCLikeRewardPending()).thenReturn(true);
		when(user.hasClaimedNameMCLikeReward()).thenReturn(false);

		ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
		when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(worker);
		doAnswer(call -> {
			call.getArgument(0, Runnable.class).run();
			return null;
		}).when(worker).execute(any(Runnable.class));

		service = new NameMCLikeRewardRecoveryService(plugin);
	}

	private String previewToken() {
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "status", "", result::set);
		assertTrue(result.get().contains("pending=true"));
		String marker = " delivered ";
		int at = result.get().indexOf(marker);
		assertTrue(at >= 0, "Preview must describe explicit delivered reconciliation");
		int start = at + marker.length();
		return result.get().substring(start, result.get().indexOf(' ', start));
	}

	@Test
	void verifiedDeliveryConfirmsClaimAndClearsPendingExactlyOnce() {
		String token = previewToken();
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "delivered", token, outcome::set);

		assertTrue(outcome.get().contains("recovery applied"));
		verify(user).setClaimedNameMCLikeReward(true);
		verify(user).setNameMCLikeRewardPending(false);
		service.handle("Console", PLAYER_UUID.toString(), "delivered", token, outcome::set);
		assertTrue(outcome.get().contains("Invalid or expired"));
	}

	@Test
	void safeRetryReleasesPendingOnlyAfterVerification() {
		String token = previewToken();
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "retry", token, outcome::set);
		assertTrue(outcome.get().contains("recovery applied"));
		verify(user, never()).setClaimedNameMCLikeReward(true);
		verify(user).setNameMCLikeRewardPending(false);
	}

	@Test
	void activeDeliveryBlocksPreviewAndChanges() {
		inFlight.add(PLAYER_UUID);
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "status", "", outcome::set);
		assertTrue(outcome.get().contains("still active"));
		verify(user, never()).setNameMCLikeRewardPending(false);
		verify(user, never()).setClaimedNameMCLikeReward(true);
	}

	@Test
	void changedClaimStateRejectsStalePreview() {
		String token = previewToken();
		when(user.hasClaimedNameMCLikeReward()).thenReturn(true);
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "delivered", token, outcome::set);
		assertTrue(outcome.get().contains("recovery failed"));
		verify(user, never()).setClaimedNameMCLikeReward(true);
		verify(user, never()).setNameMCLikeRewardPending(false);
	}

	@Test
	void claimedPendingStateCannotBeManuallyRetried() {
		when(user.hasClaimedNameMCLikeReward()).thenReturn(true);
		String token = previewToken();
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "retry", token, outcome::set);
		assertTrue(outcome.get().contains("recovery failed"));
		verify(user, never()).setNameMCLikeRewardPending(false);
	}

	@Test
	void invalidUuidOrInvalidTokenCannotChangeClaims() {
		AtomicReference<String> outcome = new AtomicReference<>();
		service.handle("Console", "invalid", "delivered", "abc", outcome::set);
		assertTrue(outcome.get().startsWith("Invalid UUID"));
		service.handle("Console", PLAYER_UUID.toString(), "retry", "abc", outcome::set);
		assertTrue(outcome.get().contains("Invalid or expired"));
		verify(user, never()).setNameMCLikeRewardPending(false);
	}
	@Test
	void sharedSqlNameMCRecoveryRequiresExplicitNetworkQuiescence() {
		when(plugin.getUserManager().getDataManager().hasSharedSqlBackend()).thenReturn(true);
		String token = previewToken();
		AtomicReference<String> result = new AtomicReference<>();
		service.handle("Console", PLAYER_UUID.toString(), "retry", token, result::set);
		assertTrue(result.get().contains("ALL backend servers"));
		verify(user, never()).setNameMCLikeRewardPending(false);

		service.handle("Console", PLAYER_UUID.toString(), "retry", token, true, result::set);
		assertTrue(result.get().contains("recovery applied"));
		verify(user).setNameMCLikeRewardPending(false);
	}

}
