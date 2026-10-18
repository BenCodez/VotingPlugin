package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import org.bukkit.configuration.file.FileConfiguration;
import com.google.gson.JsonObject;

/** Test-only forwarding access to the non-public production NetworkHealthConfigFacts. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NetworkHealthConfigFactsAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private NetworkHealthConfigFactsAccess(Object delegate) { this.delegate = delegate; }
    public static NetworkHealthConfigFactsAccess view(Object delegate) { return delegate == null ? null : new NetworkHealthConfigFactsAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof NetworkHealthConfigFactsAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static NetworkHealthConfigFactsAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.control.NetworkHealthConfigFacts"))); }
    public NetworkHealthConfigFactsAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public NetworkHealthConfigFactsAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public NetworkHealthConfigFactsAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public static void addBackend(JsonObject out, FileConfiguration config) { ReflectiveAccess.call("com.bencodez.votingplugin.control.NetworkHealthConfigFacts", null, "addBackend", new String[]{"JsonObject", "FileConfiguration"}, new Object[]{out, config}); }
    public static void addSpecialRewards(JsonObject out, FileConfiguration config) { ReflectiveAccess.call("com.bencodez.votingplugin.control.NetworkHealthConfigFacts", null, "addSpecialRewards", new String[]{"JsonObject", "FileConfiguration"}, new Object[]{out, config}); }
    public static void addGlobal(JsonObject out, FileConfiguration config) { ReflectiveAccess.call("com.bencodez.votingplugin.control.NetworkHealthConfigFacts", null, "addGlobal", new String[]{"JsonObject", "FileConfiguration"}, new Object[]{out, config}); }
    public static void addDelay(JsonObject out, Object rawDelay, Object rawMinutes, com.bencodez.simpleapi.time.ParsedDuration parsed) { ReflectiveAccess.call("com.bencodez.votingplugin.control.NetworkHealthConfigFacts", null, "addDelay", new String[]{"JsonObject", "Object", "Object", "ParsedDuration"}, new Object[]{out, rawDelay, rawMinutes, parsed}); }
    public static void addObservedServices(JsonObject out, Iterable<String> observations) { ReflectiveAccess.call("com.bencodez.votingplugin.control.NetworkHealthConfigFacts", null, "addObservedServices", new String[]{"JsonObject", "Iterable"}, new Object[]{out, observations}); }

}
