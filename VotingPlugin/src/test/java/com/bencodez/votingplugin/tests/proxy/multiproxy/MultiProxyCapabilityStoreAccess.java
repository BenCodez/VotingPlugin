package com.bencodez.votingplugin.tests.proxy.multiproxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.multiproxy.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/** Test-only forwarding access to the non-public production MultiProxyCapabilityStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class MultiProxyCapabilityStoreAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private MultiProxyCapabilityStoreAccess(Object delegate) { this.delegate = delegate; }
    public static MultiProxyCapabilityStoreAccess view(Object delegate) { return delegate == null ? null : new MultiProxyCapabilityStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof MultiProxyCapabilityStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static MultiProxyCapabilityStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore"))); }
    public MultiProxyCapabilityStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public MultiProxyCapabilityStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public MultiProxyCapabilityStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public static State load(Path dataDirectory) throws IOException { return MultiProxyCapabilityStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore", null, "load", new String[]{"Path"}, new Object[]{dataDirectory})); }
    public static void save(Path dataDirectory, Collection<String> peers) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore", null, "save", new String[]{"Path", "Collection"}, new Object[]{dataDirectory, peers}); }
    public static void save(Path dataDirectory, Collection<String> peers, Map<String, Long> discoveryDeadlines) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore", null, "save", new String[]{"Path", "Collection", "Map"}, new Object[]{dataDirectory, peers, discoveryDeadlines}); }
    public static void save(Path dataDirectory, Collection<String> peers, Map<String, Long> discoveryDeadlines, long lastObservedMillis) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore", null, "save", new String[]{"Path", "Collection", "Map", "long"}, new Object[]{dataDirectory, peers, discoveryDeadlines, lastObservedMillis}); }
    public static final class State implements ReflectiveAccess.View {

        private final Object delegate;
        private State(Object delegate) { this.delegate = delegate; }
        public static State view(Object delegate) { return delegate == null ? null : new State(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof State value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public Set<String> peers() { return (Set<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore$State", delegate, "peers", new String[]{}, new Object[]{}); }
        public Map<String, Long> discoveryDeadlines() { return (Map<String, Long>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore$State", delegate, "discoveryDeadlines", new String[]{}, new Object[]{}); }
        public long lastObservedMillis() { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore$State", delegate, "lastObservedMillis", new String[]{}, new Object[]{}); }
        public static State empty() { return MultiProxyCapabilityStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyCapabilityStore$State", null, "empty", new String[]{}, new Object[]{})); }
    }

}
