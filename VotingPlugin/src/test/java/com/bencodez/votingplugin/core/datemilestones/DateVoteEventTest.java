package com.bencodez.votingplugin.core.datemilestones;
import static org.junit.jupiter.api.Assertions.*;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
class DateVoteEventTest {
    private DateVoteEvent event(String name, long end) {
        return new DateVoteEvent("october", name, true, 100, end, "UTC", List.of(25, 10), Set.of("a"), "owner");
    }
    @Test void boundariesFiltersAndNetworkOwnerAreExplicit() {
        var e = event("October", 200);
        assertFalse(e.matches(99, "a", true, false, ""));
        assertTrue(e.matches(100, "a", true, false, ""));
        assertTrue(e.matches(199, "a", true, true, "owner"));
        assertFalse(e.matches(200, "a", true, true, "owner"));
        assertFalse(e.matches(100, "b", true, false, ""));
        assertFalse(e.matches(100, "a", false, false, ""));
        assertFalse(e.matches(100, "a", true, true, "other"));
    }
    @Test void displayRenamePreservesIdentityButWindowChangeDoesNot() {
        assertEquals(event("October", 200).fingerprint(), event("Renamed", 200).fingerprint());
        assertNotEquals(event("October", 200).fingerprint(), event("October", 201).fingerprint());
    }
    @Test void timezoneAndDstBoundariesRejectAmbiguity() {
        assertEquals(3600000, DateVoteEvent.timestamp("2026-10-01T00:00:00", "UTC")
                - DateVoteEvent.timestamp("2026-10-01T00:00:00", "Europe/London"));
        assertThrows(IllegalArgumentException.class, () -> DateVoteEvent.timestamp("2026-11-01T01:30:00", "America/New_York"));
        assertThrows(IllegalArgumentException.class, () -> DateVoteEvent.timestamp("2026-03-08T02:30:00", "America/New_York"));
        assertThrows(RuntimeException.class, () -> DateVoteEvent.timestamp("not-a-date", "UTC"));
    }
    @Test void siteFilterFingerprintUsesUnambiguousEncoding() {
        var first = new DateVoteEvent("a", "A", true, 100, 200, "UTC", List.of(1), Set.of("a", "b"), "");
        var second = new DateVoteEvent("a", "A", true, 100, 200, "UTC", List.of(1), Set.of("a, b"), "");
        assertNotEquals(first.fingerprint(), second.fingerprint());
    }
    @Test void invalidIdentityWindowAndThresholdsAreRejected() {
        assertThrows(IllegalArgumentException.class, () -> new DateVoteEvent("../x", "x", true, 100, 200, "UTC", List.of(1), Set.of(), ""));
        assertThrows(IllegalArgumentException.class, () -> new DateVoteEvent("x", "x", true, 100, 100, "UTC", List.of(1), Set.of(), ""));
        assertThrows(IllegalArgumentException.class, () -> new DateVoteEvent("x", "x", true, 100, 200, "UTC", List.of(1,1), Set.of(), ""));
    }
}
