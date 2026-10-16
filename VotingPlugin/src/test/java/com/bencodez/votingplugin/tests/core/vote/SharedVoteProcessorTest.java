package com.bencodez.votingplugin.tests.core.vote;

import com.bencodez.votingplugin.core.vote.SharedVotePolicy;
import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
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
        return ops;
    }

    @Test
    void onlineVoteKeepsProductionPhaseOrderAndCounts() {
        var ops = accepted();
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
        order.verify(ops).cache(user);
        order.verify(ops).updateName(user);
        order.verify(ops).voteParty(user, true, false, true);
        order.verify(ops).broadcast(any(UUID.class), eq("Ben"), any(), eq(true));
        order.verify(ops).setTime(user, site, 123L);
        order.verify(ops).playerVote(user, site, true, false);
        order.verify(ops).sendVoteEffects(user, true);
        order.verify(ops).closeInventory(user);
        order.verify(ops).addTotal(user);
        order.verify(ops).addTotalDaily(user);
        order.verify(ops).addTotalWeekly(user);
        order.verify(ops).addPoints(user);
        order.verify(ops).checkDayVoteStreak(user, false);
        order.verify(ops).setMonthTotal(user, 2);
        order.verify(ops).milestones(eq(user), any(UUID.class), eq(false));
        order.verify(ops).cooldown(user, site);
        order.verify(ops).voteStreak(eq(user), eq(123L), any(UUID.class));
        order.verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(123L), any(UUID.class), eq(false));
        order.verify(ops).updatePlaceholders(user);
        order.verify(ops).setUpdate();
    }

    @Test
    void proxyVoteUsesHistoricalOnlineForDeliveryButCurrentOnlineForTotals() {
        var ops = accepted();
        UUID proxyId = UUID.randomUUID();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.hasProxyTextTotals()).thenReturn(true);
        when(ops.proxyVoteId()).thenReturn(proxyId);
        when(ops.identifiedQueuedProxyVote()).thenReturn(true);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.broadcastEnabled()).thenReturn(true);
        when(ops.hasBroadcastHandler()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).lastVoteTime(user, site);
        verify(ops, never()).canVoteSite(user, site);
        verify(ops).broadcast(any(UUID.class), eq("Ben"), any(), eq(true));
        verify(ops).playerVote(user, site, true, false);
        verify(ops, never()).addOfflineVote(any(), any());
        verify(ops, never()).addTotal(user);
        verify(ops).addPoints(user);
        ArgumentCaptor<UUID> id = ArgumentCaptor.forClass(UUID.class);
        verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(321L), id.capture(), eq(false));
        assertEquals(proxyId, id.getValue());
    }

    @Test
    void legacyProxyVoteRetainsTimestampFallbackDuringRollingUpgrade() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).playerVote(user, site, false, false);
        verify(ops, never()).giveWaitRewards(eq(site), eq(user), anyBoolean(), eq(true));
    }

    @Test
    void modernUnknownProxyVoteCannotUseTimestampFallbackEvenWhenTimesMatch() {
        var ops = accepted();
        UUID proxyId = UUID.randomUUID();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.proxyVoteId()).thenReturn(proxyId);
        when(ops.proxyQueueClassificationKnown()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).giveWaitRewards(eq(site), eq(user), anyBoolean(), eq(true));
        verify(ops, never()).playerVote(any(), any(), anyBoolean(), anyBoolean());
    }

    @Test
    void upstreamValidatedModernVoteStillHonorsStricterBackendDelayWhenNotQueued() {
        var ops = accepted();
        UUID proxyId = UUID.randomUUID();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.proxyVoteId()).thenReturn(proxyId);
        when(ops.proxyQueueClassificationKnown()).thenReturn(true);
        when(ops.proxyDelayValidationKnown()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).giveWaitRewards(eq(site), eq(user), anyBoolean(), eq(true));
        verify(ops, never()).playerVote(any(), any(), anyBoolean(), anyBoolean());
    }

    @Test
    void identifiedQueuedModernVoteCanBypassDelayRegardlessOfLastVotesOrder() {
        var ops = accepted();
        UUID proxyId = UUID.randomUUID();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.proxyVoteId()).thenReturn(proxyId);
        when(ops.identifiedQueuedProxyVote()).thenReturn(true);
        when(ops.proxyDelayValidationKnown()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(999L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).lastVoteTime(user, site);
        verify(ops, never()).canVoteSite(user, site);
        verify(ops).playerVote(user, site, false, false);
        verify(ops, never()).giveWaitRewards(eq(site), eq(user), anyBoolean(), eq(true));
    }

    @Test
    void explicitUnvalidatedProxyVoteCannotUseLegacyTimestampFallback() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.proxyQueueClassificationKnown()).thenReturn(true);
        when(ops.proxyDelayValidationKnown()).thenReturn(true);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);
        when(ops.processRewards()).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).giveWaitRewards(eq(site), eq(user), anyBoolean(), eq(true));
        verify(ops, never()).playerVote(any(), any(), anyBoolean(), anyBoolean());
    }

    @Test
    void preMarkerProxyBacklogRetainsLegacyTimestampFallbackWithStableId() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.proxyVoteId()).thenReturn(UUID.randomUUID());
        when(ops.proxyDelayValidationKnown()).thenReturn(false);
        when(ops.incomingTime()).thenReturn(321L);
        when(ops.lastVoteTime(user, site)).thenReturn(321L);
        when(ops.waitUntilVoteDelay(site)).thenReturn(true);
        when(ops.canVoteSite(user, site)).thenReturn(false);

        SharedVoteProcessor.process(ops);

        verify(ops, never()).canVoteSite(user, site);
        verify(ops).playerVote(user, site, false, false);
    }

    @Test
    void targetedProxyVotePreservesHistoricalOfflineStateWhenPlayerIsNowOnline() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.targetedProxyVote()).thenReturn(true);
        when(ops.wasOnline()).thenReturn(false);
        when(ops.userOnline(user)).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).voteParty(user, true, false, false);
    }

    @Test
    void allServerProxyVoteRequiresLocalPresenceWhenHistoricallyOnline() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.targetedProxyVote()).thenReturn(false);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.userOnline(user)).thenReturn(false);

        SharedVoteProcessor.process(ops);

        verify(ops).voteParty(user, true, false, false);
    }

    @Test
    void normalNonProxyVoteStillUsesCurrentBackendPresence() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(false);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.userOnline(user)).thenReturn(false);

        SharedVoteProcessor.process(ops);

        verify(ops).voteParty(user, true, false, false);
    }

    @Test
    void delayedTargetedProxyVotePreservesVoteTimeOnlineStateAfterDisconnect() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.targetedProxyVote()).thenReturn(true);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.userOnline(user)).thenReturn(false);

        SharedVoteProcessor.process(ops);

        verify(ops).voteParty(user, true, false, true);
    }

    @Test
    void allServerProxyVoteCountsWhenHistoricallyAndLocallyOnline() {
        var ops = accepted();
        when(ops.proxyVote()).thenReturn(true);
        when(ops.targetedProxyVote()).thenReturn(false);
        when(ops.wasOnline()).thenReturn(true);
        when(ops.userOnline(user)).thenReturn(true);

        SharedVoteProcessor.process(ops);

        verify(ops).voteParty(user, true, false, true);
    }

    @Test
    void allServerVoteAdvancesOnlyTheBackendWherePlayerIsLocallyOnline() {
        var onlineBackend = accepted();
        when(onlineBackend.proxyVote()).thenReturn(true);
        when(onlineBackend.targetedProxyVote()).thenReturn(false);
        when(onlineBackend.wasOnline()).thenReturn(true);
        when(onlineBackend.userOnline(user)).thenReturn(true);
        var otherBackend = accepted();
        when(otherBackend.proxyVote()).thenReturn(true);
        when(otherBackend.targetedProxyVote()).thenReturn(false);
        when(otherBackend.wasOnline()).thenReturn(true);
        when(otherBackend.userOnline(user)).thenReturn(false);

        SharedVoteProcessor.process(onlineBackend);
        SharedVoteProcessor.process(otherBackend);

        verify(onlineBackend).voteParty(user, true, false, true);
        verify(otherBackend).voteParty(user, true, false, false);
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
        order.verify(ops).addPoints(user);
        order.verify(ops).postVote(eq(site), eq(user), eq("Ben"), eq(456L), any(UUID.class), eq(true));
        verify(ops, never()).playerVote(any(), any(), eq(false), eq(false));
        verify(ops, never()).addTotal(user);
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
        verify(ops).addPoints(user);
    }

    @TestFactory
    Stream<DynamicTest> delayAuthorizationDoesNotConfuseTimestampsWithQueuedIdentity() {
        record Delivery(String name, boolean proxy, boolean queued, boolean queueKnown,
                boolean delayKnown, boolean identified, long incoming, long stored, boolean accepted) { }
        return List.of(
                new Delivery("direct matching timestamp", false, false, false, false, true, 321L, 321L, false),
                new Delivery("direct with proxy queue flags", false, true, true, true, true, 321L, 321L, false),
                new Delivery("modern live matching timestamp", true, false, true, false, true, 321L, 321L, false),
                new Delivery("upstream validated live matching timestamp", true, false, true, true, true, 321L, 321L, false),
                new Delivery("queued without identity", true, true, true, true, false, 321L, 321L, false),
                new Delivery("explicit validation without queue classification", true, false, false, true, true, 321L, 321L, false),
                new Delivery("identified queue matching timestamp", true, true, true, true, true, 321L, 321L, true),
                new Delivery("identified queue older than LastVotes", true, true, true, true, true, 321L, 999L, true),
                new Delivery("identified queue newer than LastVotes", true, true, true, true, true, 999L, 321L, true),
                new Delivery("legacy missing identity matching timestamp", true, false, false, false, false, 321L, 321L, true),
                new Delivery("legacy identified matching timestamp", true, false, false, false, true, 321L, 321L, true),
                new Delivery("legacy mismatched timestamp", true, false, false, false, true, 320L, 321L, false),
                new Delivery("legacy zero timestamp", true, false, false, false, false, 0L, 0L, false))
                .stream().map(delivery -> DynamicTest.dynamicTest(delivery.name(), () -> {
                    var ops = accepted();
                    UUID voteId = delivery.identified() ? UUID.randomUUID() : null;
                    when(ops.proxyVote()).thenReturn(delivery.proxy());
                    when(ops.proxyVoteId()).thenReturn(voteId);
                    when(ops.identifiedQueuedProxyVote()).thenReturn(delivery.queued());
                    when(ops.proxyQueueClassificationKnown()).thenReturn(delivery.queueKnown());
                    when(ops.proxyDelayValidationKnown()).thenReturn(delivery.delayKnown());
                    when(ops.incomingTime()).thenReturn(delivery.incoming());
                    when(ops.lastVoteTime(user, site)).thenReturn(delivery.stored());
                    when(ops.waitUntilVoteDelay(site)).thenReturn(true);
                    when(ops.canVoteSite(user, site)).thenReturn(false);
                    when(ops.userOnline(user)).thenReturn(true);
                    when(ops.wasOnline()).thenReturn(true);
                    when(ops.processRewards()).thenReturn(true);

                    SharedVoteProcessor.process(ops);

                    assertAcceptedEffects(ops, delivery.accepted() ? 1 : 0);
                    verify(ops, times(delivery.accepted() ? 0 : 1))
                            .giveWaitRewards(site, user, true, delivery.proxy());
                    if (delivery.accepted() && voteId != null) {
                        verify(ops).voteStreak(user, delivery.incoming(), voteId);
                        verify(ops).postVote(site, user, "Ben", delivery.incoming(), voteId, false);
                    }
                }));
    }

    @TestFactory
    Stream<DynamicTest> directVotesKeepDocumentedDelayExceptions() {
        return Stream.of("delay disabled", "delay elapsed", "fake vote", "bypass permission")
                .map(exception -> DynamicTest.dynamicTest(exception, () -> {
                    var ops = accepted();
                    when(ops.waitUntilVoteDelay(site)).thenReturn(!exception.equals("delay disabled"));
                    when(ops.canVoteSite(user, site)).thenReturn(exception.equals("delay elapsed"));
                    when(ops.realVote()).thenReturn(!exception.equals("fake vote"));
                    when(ops.bypassWaitPermission(user)).thenReturn(exception.equals("bypass permission"));
                    when(ops.userOnline(user)).thenReturn(true);
                    when(ops.processRewards()).thenReturn(true);

                    SharedVoteProcessor.process(ops);

                    verify(ops).playerVote(user, site, true, false);
                    verify(ops).cooldown(user, site);
                    verify(ops).postVote(eq(site), eq(user), eq("Ben"), anyLong(), any(UUID.class), eq(false));
                    verify(ops, never()).giveWaitRewards(any(), any(), anyBoolean(), anyBoolean());
                }));
    }

    private void assertAcceptedEffects(SharedVoteProcessor.Operations<Object, Object> ops, int count) {
        verify(ops, times(count)).cache(user);
        verify(ops, times(count)).updateName(user);
        verify(ops, times(count)).voteParty(eq(user), anyBoolean(), anyBoolean(), anyBoolean());
        verify(ops, times(count)).setTime(eq(user), eq(site), anyLong());
        verify(ops, times(count)).playerVote(eq(user), eq(site), anyBoolean(), anyBoolean());
        verify(ops, never()).addOfflineVote(any(), any());
        verify(ops, times(count)).addTotal(user);
        verify(ops, times(count)).addTotalDaily(user);
        verify(ops, times(count)).addTotalWeekly(user);
        verify(ops, times(count)).addPoints(user);
        verify(ops, times(count)).checkDayVoteStreak(eq(user), anyBoolean());
        verify(ops, times(count)).milestones(eq(user), any(UUID.class), anyBoolean());
        verify(ops, times(count)).cooldown(user, site);
        verify(ops, times(count)).voteStreak(eq(user), anyLong(), any(UUID.class));
        verify(ops, times(count)).postVote(eq(site), eq(user), eq("Ben"), anyLong(), any(UUID.class), eq(false));
        verify(ops, times(count)).setUpdate();
    }
}
