package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import org.spongepowered.configurate.ConfigurationNode;

/** Test-only forwarding access to the non-public production NeoForgeProxySocketConfiguration. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeProxySocketConfigurationAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private NeoForgeProxySocketConfigurationAccess(Object delegate) { this.delegate = delegate; }
    public static NeoForgeProxySocketConfigurationAccess view(Object delegate) { return delegate == null ? null : new NeoForgeProxySocketConfigurationAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof NeoForgeProxySocketConfigurationAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static NeoForgeProxySocketConfigurationAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration"))); }
    public NeoForgeProxySocketConfigurationAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public NeoForgeProxySocketConfigurationAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public NeoForgeProxySocketConfigurationAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public NeoForgeProxySocketConfigurationAccess(boolean enabled, String server, String proxyName, String proxyHost, int proxyPort, String backendHost, int backendPort, String authenticationKeyFile, boolean communicationEncryption, boolean debug) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", new String[]{"boolean", "String", "String", "String", "int", "String", "int", "String", "boolean", "boolean"}, new Object[]{enabled, server, proxyName, proxyHost, proxyPort, backendHost, backendPort, authenticationKeyFile, communicationEncryption, debug})); }
    public boolean enabled() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "enabled", new String[]{}, new Object[]{}); }
    public String server() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "server", new String[]{}, new Object[]{}); }
    public String proxyName() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "proxyName", new String[]{}, new Object[]{}); }
    public String proxyHost() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "proxyHost", new String[]{}, new Object[]{}); }
    public int proxyPort() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "proxyPort", new String[]{}, new Object[]{}); }
    public String backendHost() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "backendHost", new String[]{}, new Object[]{}); }
    public int backendPort() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "backendPort", new String[]{}, new Object[]{}); }
    public String authenticationKeyFile() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "authenticationKeyFile", new String[]{}, new Object[]{}); }
    public boolean communicationEncryption() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "communicationEncryption", new String[]{}, new Object[]{}); }
    public boolean debug() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", delegate, "debug", new String[]{}, new Object[]{}); }
    public static NeoForgeProxySocketConfigurationAccess load(ConfigurationNode root) { return NeoForgeProxySocketConfigurationAccess.view(ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketConfiguration", null, "load", new String[]{"ConfigurationNode"}, new Object[]{root})); }
}
