package com.bencodez.votingplugin.control;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.util.*;
import java.lang.reflect.Method;

/** Optional names-only API: unsupported/partial snapshots never prove empty forwarding. */
public final class OptionalVotifierDiagnostics {
    private OptionalVotifierDiagnostics() { }
    public static void add(JsonObject out, Iterable<?> providers) {
        List<JsonObject> observations = new ArrayList<>();
        int count = 0; boolean inventoryComplete = true;
        for (Object candidate : providers) {
            if (++count > 128) { inventoryComplete = false; break; }
            if (candidate == null || !candidate.getClass().getName().toLowerCase(Locale.ROOT).contains("votifier")) continue;
            JsonObject facts = new JsonObject();
            try { read(facts, candidate); }
            catch (ReflectiveOperationException | RuntimeException ignored) {
                facts = new JsonObject(); // no exception messages or partial facts cross this boundary
            }
            observations.add(facts);
        }
        if (observations.isEmpty()) return;
        for (String field : List.of("votifierProviderPresent", "votifierListenerInitialized")) {
            boolean positive = observations.stream().anyMatch(f -> f.has(field) && f.get(field).getAsBoolean());
            boolean negative = inventoryComplete && observations.stream().allMatch(f -> f.has(field) && !f.get(field).getAsBoolean());
            if (positive || negative) out.addProperty(field, positive);
        }
        boolean enabled = false, stateKnown = inventoryComplete, namesKnown = inventoryComplete;
        LinkedHashSet<String> names = new LinkedHashSet<>();
        for (JsonObject facts : observations) {
            boolean complete = facts.has("votifierForwardingKnown") && facts.get("votifierForwardingKnown").getAsBoolean()
                    && facts.has("forwardingDestinations");
            namesKnown &= complete;
            if (facts.has("votifierForwardingEnabled")) enabled |= facts.get("votifierForwardingEnabled").getAsBoolean();
            else if (complete) enabled |= !facts.getAsJsonArray("forwardingDestinations").isEmpty();
            else stateKnown = false;
            if (facts.has("forwardingDestinations")) for (var name : facts.getAsJsonArray("forwardingDestinations")) {
                if (names.size() < 100 || names.contains(name.getAsString())) names.add(name.getAsString());
                else namesKnown = false;
            }
        }
        if (enabled || stateKnown) out.addProperty("votifierForwardingEnabled", enabled);
        out.addProperty("votifierForwardingKnown", namesKnown);
        JsonArray values = new JsonArray(); names.forEach(values::add); out.add("forwardingDestinations", values);
    }
    private static void read(JsonObject facts, Object candidate) throws ReflectiveOperationException {
        if (addNuVotifierForwarding(facts, candidate)) return;
        Object snapshot = candidate.getClass().getMethod("getNetworkHealthSnapshot").invoke(candidate);
        if (snapshot == null) return;
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
    }

    /** Reads only NuVotifier's observed private field shape; it never invokes forwarding code. */
    private static boolean addNuVotifierForwarding(JsonObject out, Object candidate)
            throws ReflectiveOperationException {
        String type = candidate.getClass().getName();
        if (!(type.equals("com.vexsoftware.votifier.bungee.NuVotifier")
                || type.equals("com.vexsoftware.votifier.velocity.VotifierPlugin"))) return false;
        var field = candidate.getClass().getDeclaredField("forwardingMethod");
        field.setAccessible(true);
        Object source = field.get(candidate);
        if (source == null) return true; // startup failure, pending setup, or disabled: unknown
        String sourceType = source.getClass().getName();
        if (sourceType.equals("com.vexsoftware.votifier.bungee.PluginMessagingForwardingSource")
                || sourceType.equals("com.vexsoftware.votifier.bungee.OnlineForwardPluginMessagingForwardingSource")
                || sourceType.equals("com.vexsoftware.votifier.velocity.PluginMessagingForwardingSource")
                || sourceType.equals("com.vexsoftware.votifier.velocity.OnlineForwardPluginMessagingForwardingSource")) {
            Class<?> base = source.getClass().getSuperclass();
            if (!base.getName().equals("com.vexsoftware.votifier.support.forwarding.AbstractPluginMessagingForwardingSource")) return true;
            var filterField = base.getDeclaredField("serverFilter"); filterField.setAccessible(true);
            Object filter = filterField.get(source);
            var pluginField = base.getDeclaredField("plugin"); pluginField.setAccessible(true);
            Object plugin = pluginField.get(source);
            if (filter == null || plugin != candidate || !filter.getClass().getName().equals(
                    "com.vexsoftware.votifier.support.forwarding.ServerFilter")) return true;
            Object rawServers = Class.forName("com.vexsoftware.votifier.platform.ProxyVotifierPlugin", false,
                    candidate.getClass().getClassLoader()).getMethod("getAllBackendServers").invoke(plugin);
            if (!(rawServers instanceof Iterable<?> servers)) return true;
            JsonArray names = new JsonArray(); Set<String> seen = new HashSet<>(); int examined = 0;
            Method allowed = filter.getClass().getMethod("isAllowed", String.class);
            Method serverName = Class.forName("com.vexsoftware.votifier.platform.BackendServer", false,
                    plugin.getClass().getClassLoader()).getMethod("getName");
            for (Object server : servers) {
                if (++examined > 100 || server == null) return true;
                Object rawName = serverName.invoke(server);
                if (!(rawName instanceof String name) || name.isBlank() || name.length() > 80
                        || name.codePoints().anyMatch(Character::isISOControl) || !seen.add(name)) return true;
                if (Boolean.TRUE.equals(allowed.invoke(filter, name))) names.add(name);
            }
            out.addProperty("votifierForwardingEnabled", names.size() > 0);
            out.addProperty("votifierForwardingKnown", true);
            out.add("forwardingDestinations", names);
            return true;
        }
        if (!sourceType.equals("com.vexsoftware.votifier.support.forwarding.proxy.ProxyForwardingVoteSource")) return true;
        var targets = source.getClass().getDeclaredField("backendServers");
        targets.setAccessible(true);
        Object raw = targets.get(source);
        if (!(raw instanceof Iterable<?> values)) return true;
        JsonArray names = new JsonArray(); Set<String> seen = new HashSet<>(); int examined = 0;
        for (Object value : values) {
            if (++examined > 100 || value == null || !value.getClass().getName().equals(
                    "com.vexsoftware.votifier.support.forwarding.proxy.ProxyForwardingVoteSource$BackendServer")) return true;
            var name = value.getClass().getDeclaredField("name"); name.setAccessible(true);
            Object rawName = name.get(value);
            if (!(rawName instanceof String text) || text.isBlank() || text.length() > 80
                    || text.codePoints().anyMatch(Character::isISOControl) || !seen.add(text)) return true;
            names.add(text);
        }
        out.addProperty("votifierForwardingEnabled", !names.isEmpty());
        out.addProperty("votifierForwardingKnown", true);
        out.add("forwardingDestinations", names);
        return true;
    }
    private static void addBoolean(JsonObject out, String field, Object snapshot, String getter) throws ReflectiveOperationException {
        Object value = snapshot.getClass().getMethod(getter).invoke(snapshot);
        if (value instanceof Boolean fact) out.addProperty(field, fact);
    }
}
