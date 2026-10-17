package com.bencodez.votingplugin.tests.core.session;

import com.bencodez.votingplugin.core.session.GuidedVoteSession;
import com.bencodez.votingplugin.core.session.ProxyVoteSequence;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

class ProxyVoteSequenceTest {
    private GuidedVoteSession session() {
        var session = new GuidedVoteSession(Long.MAX_VALUE, Long.MAX_VALUE);
        session.bindCandidates(List.of(new GuidedVoteSession.Site("ore", "Site", "https://example.org", true, 0)));
        session.refresh(List.of(new GuidedVoteSession.Site("ore", "Site", "https://example.org", true, 0)));
        return session;
    }
    @Test void oldInFlightAndNewDelayedVotesUseOriginalOrderNotClocksOrDeliveryOrder() {
        var source = new ProxyVoteSequence(UUID.randomUUID());
        UUID old = source.next(); var fence = source.fence(); UUID fresh = source.next();
        var guide = session(); guide.bindProxyFence(fence);
        guide.acceptedProxy("ore", old); assertEquals(0, guide.view("").received());
        guide.acceptedProxy("ore", fresh); guide.acceptedProxy("ore", fresh);
        assertEquals(1, guide.view("").received());
        assertEquals(8, fresh.version()); assertEquals(2, fresh.variant());
        assertEquals(fresh, UUID.fromString(fresh.toString()));
    }
    @Test void newReceiptMayOvertakeItsHandshakeAndWrongSourceCannotConfirm() {
        var source = new ProxyVoteSequence(UUID.randomUUID()); var fence = source.fence();
        UUID fresh = source.next(); var guide = session(); guide.acceptedProxy("ore", fresh);
        guide.bindProxyFence(new ProxyVoteSequence(UUID.randomUUID()).fence());
        assertEquals(0, guide.view("").received());
        guide.bindProxyFence(fence); assertEquals(1, guide.view("").received());
    }
    @Test void outOfOrderNewVotesAndSiteIdentityDoNotDoubleCount() {
        var source = new ProxyVoteSequence(UUID.randomUUID()); var fence = source.fence();
        UUID first = source.next(), later = source.next(); var guide = session(); guide.bindProxyFence(fence);
        guide.acceptedProxy("unrelated", later); assertEquals(0, guide.view("").received());
        guide.acceptedProxy("ore", later); guide.acceptedProxy("ore", first);
        assertEquals(1, guide.view("").received());
    }
    @Test void restartIncarnationAndLegacyIdsStayUnconfirmed() {
        var oldSource = new ProxyVoteSequence(UUID.randomUUID()); UUID old = oldSource.next();
        var current = new ProxyVoteSequence(UUID.randomUUID()); var guide = session(); guide.bindProxyFence(current.fence());
        guide.acceptedProxy("ore", old); guide.acceptedProxy("ore", UUID.randomUUID());
        assertEquals(0, guide.view("").received()); guide.acceptedProxy("ore", current.next());
        assertEquals(1, guide.view("").received());
    }
    @Test void wireNegotiationPreservesFullPrecisionAndIgnoresUnknownVersion() {
        var source = new ProxyVoteSequence(UUID.randomUUID()); source.next();
        var request = new VotingPluginWire.SessionFenceRequest(UUID.randomUUID(), UUID.randomUUID(), "backend", "origin");
        var envelope = VotingPluginWire.sessionFenceReply(request, source.fence());
        var decoded = VotingPluginWire.readSessionFenceReply(envelope);
        assertNotNull(decoded); assertEquals(request, decoded.request()); assertEquals(source.fence(), decoded.fence());
        var unknown = com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.builder(envelope.getSubChannel())
                .schema(envelope.getSchema());
        envelope.getFields().forEach(unknown::put); unknown.put(VotingPluginWire.K_SESSION_FENCE_VERSION, 2);
        assertNull(VotingPluginWire.readSessionFenceReply(unknown.build()));
        assertThrows(IllegalArgumentException.class, () -> new ProxyVoteSequence.Fence(UUID.randomUUID(), 0));
    }
    @Test void statusReplyDoesNotAdvanceAnExistingSessionFence() {
        var source = new ProxyVoteSequence(UUID.randomUUID()); var guide = session(); guide.bindProxyFence(source.fence());
        UUID occurrence = source.next(); guide.bindProxyFence(source.fence()); guide.acceptedProxy("ore", occurrence);
        assertEquals(1, guide.view("").received());
    }
}
