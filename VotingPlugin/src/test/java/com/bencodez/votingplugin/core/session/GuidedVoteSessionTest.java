package com.bencodez.votingplugin.core.session;

import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;

class GuidedVoteSessionTest {
    private GuidedVoteSession.Site site(String key, boolean eligible) {
        return new GuidedVoteSession.Site(key, key, "https://example.org/" + key, eligible, 0);
    }
    @Test void onlyRealNewMatchingAcceptedVotesConfirmSites() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", true), site("b", true)));
        UUID id = UUID.randomUUID();
        s.accepted("unrelated", id, 101, true, false);
        s.accepted("a", id, 99, true, false);
        s.accepted("a", id, 101, false, false);
        s.accepted("a", id, 101, true, true);
        assertEquals(0, s.view("check").received());
        s.accepted("a", id, 101, true, false);
        s.accepted("a", id, 101, true, false);
        s.accepted("a", UUID.randomUUID(), 100, true, false);
        assertEquals(1, s.view("check").received());
        assertFalse(s.view("check").complete());
    }
    @Test void delayedNewVoteAndOutOfOrderDeliveryDoNotRegressProgress() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", true)));
        s.accepted("a", UUID.randomUUID(), 200, true, false);
        s.accepted("a", UUID.randomUUID(), 150, true, false);
        assertTrue(s.view("check").complete());
    }
    @Test void permissionCooldownChangesAndReopeningRetainObservedProgress() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", true), site("b", true), site("cooldown", false)));
        s.accepted("a", UUID.randomUUID(), 101, true, false);
        s.refresh(List.of(site("a", false)));
        var v = s.view("check");
        assertEquals(GuidedVoteSession.Status.RECEIVED, v.entries().get(0).status());
        assertEquals(GuidedVoteSession.Status.UNAVAILABLE, v.entries().get(1).status());
        assertEquals(2, v.entries().size());
        assertEquals(1, s.view("check").received());
    }
    @Test void notificationBeforeInitialSnapshotIsRetained() {
        var s = new GuidedVoteSession(100);
        s.bindCandidates(List.of(site("a", false)));
        s.accepted("a", UUID.randomUUID(), 101, true, false);
        s.refresh(List.of(new GuidedVoteSession.Site("a", "a", "https://example.org/a", false, 101)));
        assertTrue(s.view("check").complete());
    }
    @Test void olderCachedVoteCannotEraseAnEarlyConfirmedReceipt() {
        var s = new GuidedVoteSession(100);
        s.bindCandidates(List.of(site("a", true)));
        s.accepted("a", UUID.randomUUID(), 101, true, false);
        s.accepted("a", UUID.randomUUID(), 99, true, false);
        s.refresh(List.of(new GuidedVoteSession.Site("a", "a", "https://example.org/a", false, 99)));
        assertTrue(s.view("check").complete());
        s.refresh(List.of(new GuidedVoteSession.Site("a", "a", "https://example.org/a", false, 99)));
        assertEquals(1, s.view("check").received());
    }
    @Test void receiptAfterFirstIneligibleSampleRestoresBoundCandidate() {
        var s = new GuidedVoteSession(100);
        s.bindCandidates(List.of(site("a", false)));
        s.refresh(List.of(new GuidedVoteSession.Site("a", "a", "https://example.org/a", false, 101)));
        assertEquals(GuidedVoteSession.Status.UNAVAILABLE, s.view("check").current().status());
        for (int i = 0; i < 101; i++) s.accepted("unrelated" + i, UUID.randomUUID(), 101, true, false);
        s.accepted("a", UUID.randomUUID(), 101, true, false);
        assertTrue(s.view("check").complete());
        assertEquals(1, s.view("check").entries().size());
    }
    @Test void initiallyCoolingDownSiteCannotJoinAfterLaterVote() {
        var s = new GuidedVoteSession(100);
        s.bindCandidates(List.of(site("a", false)));
        s.refresh(List.of(new GuidedVoteSession.Site("a", "a", "https://example.org/a", false, 99)));
        s.accepted("a", UUID.randomUUID(), 101, true, false);
        assertTrue(s.view("check").entries().isEmpty());
    }
    @Test void oneCanonicalOccurrenceCannotConfirmTwoSites() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", true), site("b", true)));
        UUID occurrence = UUID.randomUUID();
        s.accepted("a", occurrence, 101, true, false);
        s.accepted("b", occurrence, 102, true, false);
        assertEquals(1, s.view("check").received());
    }
    @Test void emptyAndFinishedSessionsDoNotInventCompletion() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", false)));
        assertNull(s.view("check").current());
        assertFalse(s.view("finish").complete());
        assertTrue(s.view("check").finished());
    }
    @Test void navigationAndSkipDoNotCountVotes() {
        var s = new GuidedVoteSession(100);
        s.refresh(List.of(site("a", true), site("b", true)));
        assertEquals("b", s.view("next").current().site().key());
        assertEquals("a", s.view("previous").current().site().key());
        assertEquals("b", s.view("skip").current().site().key());
        assertEquals(GuidedVoteSession.Status.SKIPPED, s.view("check").entries().get(0).status());
        assertEquals(0, s.view("finish").received());
    }
    @Test void urlValidationUsesExactSelectedUrlWithoutUnsafeSchemes() {
        assertEquals("https://example.org/a", GuidedVoteSession.httpUrl("https://example.org/a"));
        for (String bad : List.of("", "javascript:alert(1)", "https://", "https://user:secret@example.org", "not a url"))
            assertNull(GuidedVoteSession.httpUrl(bad));
        assertNull(GuidedVoteSession.httpUrl(null));
        assertEquals("https://example.org/vote", GuidedVoteSession.httpUrl("[Text=\"Vote here\",url=\"https://example.org/vote\"]"));
        assertNull(GuidedVoteSession.httpUrl("[Text=\"Vote here\",url=\"javascript:alert(1)\"]"));
        assertNull(GuidedVoteSession.httpUrl("[Text=\"Vote here\",url=\"https://user:pass@example.org\"]"));
    }
}
