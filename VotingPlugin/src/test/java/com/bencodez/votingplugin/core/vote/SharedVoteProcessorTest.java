package com.bencodez.votingplugin.core.vote;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

class SharedVoteProcessorTest {
    private final Object site = new Object();
    private final Object user = new Object();

    @SuppressWarnings("unchecked")
    private SharedVoteProcessor.Operations<Object, Object> accepted() {
        SharedVoteProcessor.Operations<Object, Object> ops = mock(SharedVoteProcessor.Operations.class);
        when(ops.enabled()).thenReturn(true);
        when(ops.incomingName()).thenReturn("Ben");
        when(ops.properName("Ben")).thenReturn("Ben");
        when(ops.validate("Ben", false)).thenReturn(new SharedVoteProcessor.Validation(true, "Ben", "CACHE", "ok", false));
        when(ops.resolveSite()).thenReturn(site);
        when(ops.siteEnabled(site)).thenReturn(true);
        when(ops.resolveUser("Ben")).thenReturn(user);
        when(ops.countingPolicy()).thenReturn(new SharedVotePolicy(false, true, false, false, false));
        when(ops.addTotals()).thenReturn(true);
        when(ops.realVote()).thenReturn(true);
        when(ops.serviceSite()).thenReturn("Example");
        when(ops.siteKey(site)).thenReturn("ExampleKey");
        when(ops.userId(user)).thenReturn("user-id");
        when(ops.userUuid(user)).thenReturn(UUID.randomUUID());
		when(ops.configuredPointAmount()).thenReturn(3);
		when(ops.configuredPointCap()).thenReturn(100);
		when(ops.prepareAccounting(eq(user), any(UUID.class), anyBoolean(), anyBoolean(), anyInt(), anyInt()))
				.thenAnswer(invocation -> new SharedVoteProcessor.AccountingAdmission(
						invocation.getArgument(2), invocation.getArgument(3), true,
						invocation.getArgument(4), invocation.getArgument(5), "Points", false));
        return ops;
    }

    @Test
    void onlineVoteKeepsProductionPhaseOrderAndCounts() {
        var ops = accepted();
		UUID voteId = UUID.randomUUID();
		when(ops.incomingVoteId()).thenReturn(voteId);
        when(ops.userOnline(user)).thenReturn(true);
        when(ops.processRewards()).thenReturn(true);
        when(ops.broadcastEnabled()).thenReturn(true);
        when(ops.hasBroadcastHandler()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(123L);
        when(ops.voteNumber()).thenReturn(1);
        when(ops.closeInventoryOnVote()).thenReturn(true);
        when(ops.limitMonthlyVotes()).thenReturn(true);
        when(ops.userMonthTotal(user)).thenReturn(20);
        when(ops.currentDayOfMonth()).thenReturn(1);
        when(ops.enabledSiteCount()).thenReturn(2);

        SharedVoteProcessor.process(ops);

        InOrder order = inOrder(ops);
        order.verify(ops).lastVoteTime(user, site);
        order.verify(ops).prepareAccounting(eq(user), any(UUID.class), eq(true), eq(true), eq(3), eq(100));
        order.verify(ops).cache(user);
        order.verify(ops).updateName(user);
        order.verify(ops).markReplayUnsafe(voteId);
        order.verify(ops).voteParty(eq(user), eq(false), any(UUID.class), eq(true));
        order.verify(ops).broadcast(any(UUID.class), eq("Ben"), any(), eq(true));
        order.verify(ops).setTime(user, site, 123L);
        order.verify(ops).playerVote(user, site, true, false);
        order.verify(ops).sendVoteEffects(user, true);
        order.verify(ops).closeInventory(user);
        order.verify(ops).addTotal(eq(user), any(UUID.class));
        order.verify(ops).addTotalDaily(eq(user), any(UUID.class));
        order.verify(ops).addTotalWeekly(eq(user), any(UUID.class));
        order.verify(ops).addPoints(user, voteId, 3, 100, "Points");
        order.verify(ops).checkDayVoteStreak(eq(user), eq(false), any(UUID.class));
        order.verify(ops).setMonthTotal(user, 2);
        order.verify(ops).milestones(eq(user), any(UUID.class), eq(false));
        order.verify(ops).cooldown(user, site);
        order.verify(ops).voteStreak(eq(user), eq(123L), any(UUID.class));
        order.verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(123L), any(UUID.class), eq(false));
        order.verify(ops).updatePlaceholders(user);
        order.verify(ops).setUpdate();
        order.verify(ops).completeDelivery(voteId);
        order.verify(ops).finishAccounting(any(UUID.class));
    }

	@Test
	void durableProducerRetainsReplayFenceUntilItsOwnAcknowledgement() {
		var ops = accepted();
		UUID voteId = UUID.randomUUID();
		when(ops.incomingVoteId()).thenReturn(voteId);
		when(ops.deferDeliveryCompletion()).thenReturn(true);

		SharedVoteProcessor.process(ops);

		verify(ops).markReplayUnsafe(voteId);
		verify(ops, never()).completeDelivery(voteId);
	}

	@Test
	void votePartyFailureKeepsTheFenceForQuarantine() {
		var ops = accepted();
		UUID voteId = UUID.randomUUID();
		when(ops.incomingVoteId()).thenReturn(voteId);
		doThrow(new IllegalStateException("reward failed"))
				.when(ops).voteParty(user, false, voteId, true);

		assertThrows(IllegalStateException.class, () -> SharedVoteProcessor.process(ops));

		InOrder order = inOrder(ops);
		order.verify(ops).markReplayUnsafe(voteId);
		order.verify(ops).voteParty(user, false, voteId, true);
		verify(ops, never()).completeDelivery(voteId);
		verify(ops).finishAccounting(voteId);
	}

    @Test
    void totalsUseTheEligibilityCapturedAtAdmission() {
        var ops = accepted();
        when(ops.userOnline(user)).thenReturn(true, false);

        SharedVoteProcessor.process(ops);

        verify(ops).prepareAccounting(eq(user), any(UUID.class), eq(true), eq(true), eq(3), eq(100));
        verify(ops).addTotal(eq(user), any(UUID.class));
        verify(ops).addTotalDaily(eq(user), any(UUID.class));
        verify(ops).addTotalWeekly(eq(user), any(UUID.class));
    }

	@Test
	void replayUsesPersistedAccountingEligibilityInsteadOfTheCurrentProposal() {
		var ops = accepted();
		when(ops.addTotals()).thenReturn(false);
		when(ops.prepareAccounting(eq(user), any(UUID.class), eq(false), eq(false), eq(3), eq(100)))
				.thenReturn(new SharedVoteProcessor.AccountingAdmission(true, true, false, 7, 50, "Points", false));

		SharedVoteProcessor.process(ops);

		verify(ops).addTotal(eq(user), any(UUID.class));
		verify(ops).addTotalDaily(eq(user), any(UUID.class));
		verify(ops).addTotalWeekly(eq(user), any(UUID.class));
		verify(ops).voteParty(eq(user), eq(false), any(UUID.class), eq(false));
	}

	@Test
	void replayUsesPersistedPointsEligibilityWhenTotalsWereNotAdmitted() {
		var ops = accepted();
		when(ops.addTotals()).thenReturn(false);
		when(ops.prepareAccounting(eq(user), any(UUID.class), eq(false), eq(false), eq(3), eq(100)))
				.thenReturn(new SharedVoteProcessor.AccountingAdmission(false, true, false, 7, 50, "Points", false));

		SharedVoteProcessor.process(ops);

		verify(ops).addPoints(eq(user), any(UUID.class), eq(7), eq(50), eq("Points"));
		verify(ops, never()).addTotal(eq(user), any(UUID.class));
	}

	@Test
	void recoveredReplayUnsafeVoteStopsBeforeAnyEffect() {
		var ops = accepted();
		UUID voteId = UUID.randomUUID();
		when(ops.incomingVoteId()).thenReturn(voteId);
		when(ops.prepareAccounting(eq(user), eq(voteId), anyBoolean(), anyBoolean(), anyInt(), anyInt()))
				.thenReturn(new SharedVoteProcessor.AccountingAdmission(true, true, true, 3, 100, "Points", true));

		assertThrows(SharedVoteReplayUnsafeException.class, () -> SharedVoteProcessor.process(ops));

		verify(ops).restoreReplayUnsafe();
		verify(ops, never()).cache(user);
		verify(ops, never()).markReplayUnsafe(any(UUID.class));
		verify(ops, never()).broadcast(any(), any(), any(), anyBoolean());
	}

    @Test
    void proxyVoteUsesHistoricalOnlineForDeliveryButCurrentOnlineForTotals() {
        var ops = accepted();
        UUID proxyId = UUID.randomUUID();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.hasProxyTextTotals()).thenReturn(true);
        when(ops.incomingVoteId()).thenReturn(proxyId);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.broadcastEnabled()).thenReturn(true);
        when(ops.hasBroadcastHandler()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).canVoteSite(user, site);
        verify(ops).broadcast(any(UUID.class), eq("Ben"), any(), eq(true));
        verify(ops).playerVote(user, site, true, false);
        verify(ops, never()).addOfflineVote(any(), any());
        verify(ops, never()).addTotal(eq(user), any(UUID.class));
        verify(ops).addPoints(eq(user), any(UUID.class), anyInt(), anyInt(), anyString());
        ArgumentCaptor<UUID> id = ArgumentCaptor.forClass(UUID.class);
        verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(321L), id.capture(), eq(false));
        assertEquals(proxyId, id.getValue());
    }

    @Test
    void proxyVoteWithoutTextTotalsSkipsMonthlyLimitEnforcement() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.limitMonthlyVotes()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).proxyMonthTotal();
        verify(ops, never()).userMonthTotal(user);
        verify(ops, never()).setMonthTotal(any(), anyInt());
    }

    @Test
    void proxyVoteWithTextTotalsUsesProxyMonthTotalForMonthlyLimitEnforcement() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.hasProxyTextTotals()).thenReturn(true);
        when(ops.limitMonthlyVotes()).thenReturn(true);
        when(ops.proxyMonthTotal()).thenReturn(20);
        when(ops.currentDayOfMonth()).thenReturn(1);
        when(ops.enabledSiteCount()).thenReturn(2);

        SharedVoteProcessor.process(ops);

        verify(ops).proxyMonthTotal();
        verify(ops, never()).userMonthTotal(user);
        verify(ops).setMonthTotal(user, 2);
    }

    @Test
    void rejectedVoteDelayGivesWaitRewardBeforeAnyAcceptedEffects() {
        var ops = accepted();
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).giveWaitRewards(site, user, false, false);
        verify(ops, never()).cache(user);
        verify(ops, never()).postVote(any(), any(), any(), anyLong(), any(), eq(false));
    }

    @Test
    void offlineVoteQueuesBeforeAccountingAndMarksPostEventCached() {
        var ops = accepted();
        when(ops.processRewards()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(456L);

        SharedVoteProcessor.process(ops);

        InOrder order = inOrder(ops);
        order.verify(ops).addOfflineVote(user, "ExampleKey");
        order.verify(ops).addPoints(eq(user), any(UUID.class), anyInt(), anyInt(), anyString());
        order.verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(456L), any(UUID.class), eq(true));
        verify(ops, never()).playerVote(any(), any(), eq(false), eq(false));
        verify(ops, never()).addTotal(eq(user), any(UUID.class));
        verify(ops).clearCache(user);
    }

    @Test
    void invalidIdentityStopsBeforeSiteAndCanRemoveProxyUser() {
        var ops = accepted();
        when(ops.validate("Ben", false)).thenReturn(new SharedVoteProcessor.Validation(false, null, "NONE", "unknown", false));
        when(ops.proxyVote()).thenReturn(true);
        when(ops.removeInvalidProxyUsers()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).removeInvalidUser("Ben");
        verify(ops, never()).resolveSite();
        verify(ops, never()).cache(user);
    }

    @Test
    void disabledSiteStopsBeforeUserResolution() {
        var ops = accepted();
        when(ops.siteEnabled(site)).thenReturn(false);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).resolveUser(any());
        verify(ops, never()).cache(user);
    }

    @Test
    void offlineQueueLimitPreservesUncachedPostEvent() {
        var ops = accepted();
        when(ops.incomingTime()).thenReturn(457L);
        when(ops.offlineVotesLimitEnabled()).thenReturn(true);
        when(ops.offlineVotes(user, site)).thenReturn(3);
        when(ops.offlineVotesLimitAmount()).thenReturn(2);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).addOfflineVote(any(), any());
        verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(457L), any(UUID.class), eq(false));
        verify(ops).addPoints(eq(user), any(UUID.class), anyInt(), anyInt(), anyString());
    }
}
