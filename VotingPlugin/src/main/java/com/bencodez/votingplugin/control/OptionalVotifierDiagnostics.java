package com.bencodez.votingplugin.control;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.util.*;

/** Optional names-only API: unsupported/partial snapshots never prove empty forwarding. */
public final class OptionalVotifierDiagnostics {
    private OptionalVotifierDiagnostics() { }
    public static void add(JsonObject out, Iterable<?> providers) {
        int count = 0;
        for (Object candidate : providers) {
            if (++count > 128) return;
            if (candidate == null || !candidate.getClass().getName().toLowerCase(Locale.ROOT).contains("votifier")) continue;
            try {
                Object snapshot = candidate.getClass().getMethod("getNetworkHealthSnapshot").invoke(candidate);
                if (snapshot == null) continue;
                JsonObject facts = new JsonObject();
                addBoolean(facts, "votifierProviderPresent", snapshot, "getProviderPresent");
                addBoolean(facts, "votifierListenerInitialized", snapshot, "getListenerInitialized");
                addBoolean(facts, "votifierForwardingKnown", snapshot, "getForwardingKnown");
                Object raw = snapshot.getClass().getMethod("getForwardingDestinations").invoke(snapshot);
                JsonArray names = new JsonArray(); boolean complete = raw instanceof Iterable<?>;
                if (raw instanceof Iterable<?> values) {
                    Set<String> seen = new HashSet<>(); int examined = 0;
                    for (Object value : values) {
                        if (++examined > 100) { complete = false; break; }
                        if (!(value instanceof String name) || name.isBlank() || name.length() > 80
                                || name.codePoints().anyMatch(Character::isISOControl) || !seen.add(name)) { complete = false; continue; }
                        names.add(name);
                    }
                }
                if (!complete) facts.addProperty("votifierForwardingKnown", false);
                facts.add("forwardingDestinations", names);
                facts.entrySet().forEach(field -> out.add(field.getKey(), field.getValue()));
                return;
            } catch (ReflectiveOperationException | RuntimeException ignored) {
                // No secrets or exception messages cross the optional boundary.
            }
        }
    }
    private static void addBoolean(JsonObject out, String field, Object snapshot, String getter) throws ReflectiveOperationException {
        Object value = snapshot.getClass().getMethod(getter).invoke(snapshot);
        if (value instanceof Boolean fact) out.addProperty(field, fact);
    }
}
