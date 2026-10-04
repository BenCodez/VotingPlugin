package com.bencodez.votingplugin.control;
import com.google.gson.JsonObject;
import java.util.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
class OptionalVotifierDiagnosticsTest {
    public static class VotifierProvider {
        private final Object snapshot;
        VotifierProvider(Object snapshot) { this.snapshot = snapshot; }
        public Object getNetworkHealthSnapshot() { return snapshot; }
    }
    public static class Snapshot {
        private final Iterable<?> names;
        Snapshot(Iterable<?> names) { this.names = names; }
        public Boolean getProviderPresent() { return true; }
        public Boolean getListenerInitialized() { return true; }
        public Boolean getForwardingKnown() { return true; }
        public Iterable<?> getForwardingDestinations() { return names; }
    }
    @Test void oldProviderApiIsUnknownAndValidNewApiContainsOnlyAllowlistedFacts() {
        JsonObject old = new JsonObject(); OptionalVotifierDiagnostics.add(old, List.of(new Object())); assertEquals(0, old.size());
        JsonObject current = new JsonObject(); OptionalVotifierDiagnostics.add(current, List.of(new VotifierProvider(new Snapshot(List.of("backend-a")))));
        assertEquals(Set.of("votifierProviderPresent", "votifierListenerInitialized", "votifierForwardingKnown", "forwardingDestinations"), current.keySet());
        assertTrue(current.get("votifierForwardingKnown").getAsBoolean()); assertEquals("backend-a", current.getAsJsonArray("forwardingDestinations").get(0).getAsString());
    }
    @Test void invalidDuplicateOrOversizedApiEvidenceNeverProvesEmptyForwarding() {
        for (Iterable<?> names : List.of(List.of("backend-a", "backend-a"), List.of("a\nb"), java.util.stream.IntStream.range(0, 101).mapToObj(i -> "backend-" + i).toList())) {
            JsonObject current = new JsonObject(); OptionalVotifierDiagnostics.add(current, List.of(new VotifierProvider(new Snapshot(names))));
            assertFalse(current.get("votifierForwardingKnown").getAsBoolean()); assertTrue(current.getAsJsonArray("forwardingDestinations").size() <= 100);
        }
    }
    @Test void consumesCandidateSnapshotWithoutACompileTimeDependency() throws Exception {
        String candidate = System.getProperty("votifier.health.candidate");
        org.junit.jupiter.api.Assumptions.assumeTrue(candidate != null, "Optional paired artifact supplied by integration validation");
        try (var loader = new java.net.URLClassLoader(new java.net.URL[]{java.nio.file.Path.of(candidate).toUri().toURL()}, getClass().getClassLoader())) {
            Class<?> type = loader.loadClass("com.vexsoftware.votifier.diagnostic.VotifierDiagnosticsSnapshot");
            Object snapshot = type.getConstructor(Boolean.class, Boolean.class, Boolean.class, Iterable.class).newInstance(true, true, true, List.of("backend-a"));
            JsonObject facts = new JsonObject(); OptionalVotifierDiagnostics.add(facts, List.of(new VotifierProvider(snapshot)));
            assertTrue(facts.get("votifierForwardingKnown").getAsBoolean()); assertTrue(facts.get("votifierListenerInitialized").getAsBoolean());
        }
    }
}
