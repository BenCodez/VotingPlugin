package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.control.*;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Map;
import java.util.UUID;
import com.google.gson.JsonObject;

/** Test-only forwarding access to the non-public production ProxyControlResultStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ProxyControlResultStoreAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private ProxyControlResultStoreAccess(Object delegate) { this.delegate = delegate; }
    public static ProxyControlResultStoreAccess view(Object delegate) { return delegate == null ? null : new ProxyControlResultStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof ProxyControlResultStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static ProxyControlResultStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore"))); }
    public ProxyControlResultStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public ProxyControlResultStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public ProxyControlResultStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public static State load(Path dataDirectory) throws IOException { return ProxyControlResultStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore", null, "load", new String[]{"Path"}, new Object[]{dataDirectory})); }
    public static State loadForRoute(Path dataDirectory, Route route) throws IOException { return ProxyControlResultStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore", null, "loadForRoute", new String[]{"Path", "Route"}, new Object[]{dataDirectory, route})); }
    public static State loadPreferred(Path dataDirectory, Route configuredRoute) throws IOException { return ProxyControlResultStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore", null, "loadPreferred", new String[]{"Path", "Route"}, new Object[]{dataDirectory, configuredRoute})); }
    public static void save(Path dataDirectory, Route route, Map<UUID, StoredResult> results) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore", null, "save", new String[]{"Path", "Route", "Map"}, new Object[]{dataDirectory, route, results}); }
    public static void save(Path dataDirectory, Route route, Map<UUID, StoredResult> results, boolean routeRequired) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore", null, "save", new String[]{"Path", "Route", "Map", "boolean"}, new Object[]{dataDirectory, route, results, routeRequired}); }
    public static final class Route implements ReflectiveAccess.View {
        public Route() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private Route(Object delegate) { this.delegate = delegate; }
        public static Route view(Object delegate) { return delegate == null ? null : new Route(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Route value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public Route(String nodeId, String displayName, String platform, String pluginVersion, URI endpoint, String credentialFile, int heartbeatSeconds, int connectTimeoutMillis, int requestTimeoutMillis) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", new String[]{"String", "String", "String", "String", "URI", "String", "int", "int", "int"}, new Object[]{nodeId, displayName, platform, pluginVersion, endpoint, credentialFile, heartbeatSeconds, connectTimeoutMillis, requestTimeoutMillis})); }
        public String nodeId() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "nodeId", new String[]{}, new Object[]{}); }
        public String displayName() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "displayName", new String[]{}, new Object[]{}); }
        public String platform() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "platform", new String[]{}, new Object[]{}); }
        public String pluginVersion() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "pluginVersion", new String[]{}, new Object[]{}); }
        public URI endpoint() { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "endpoint", new String[]{}, new Object[]{}); }
        public String credentialFile() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "credentialFile", new String[]{}, new Object[]{}); }
        public int heartbeatSeconds() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "heartbeatSeconds", new String[]{}, new Object[]{}); }
        public int connectTimeoutMillis() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "connectTimeoutMillis", new String[]{}, new Object[]{}); }
        public int requestTimeoutMillis() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "requestTimeoutMillis", new String[]{}, new Object[]{}); }
        public String identity() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$Route", delegate, "identity", new String[]{}, new Object[]{}); }
    }

    public static final class StoredResult implements ReflectiveAccess.View {
        public StoredResult() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$StoredResult", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private StoredResult(Object delegate) { this.delegate = delegate; }
        public static StoredResult view(Object delegate) { return delegate == null ? null : new StoredResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof StoredResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public StoredResult(JsonObject result, boolean committed, boolean claimRequired) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$StoredResult", new String[]{"JsonObject", "boolean", "boolean"}, new Object[]{result, committed, claimRequired})); }
        public JsonObject result() { return (JsonObject) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$StoredResult", delegate, "result", new String[]{}, new Object[]{}); }
        public boolean committed() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$StoredResult", delegate, "committed", new String[]{}, new Object[]{}); }
        public boolean claimRequired() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$StoredResult", delegate, "claimRequired", new String[]{}, new Object[]{}); }
    }

    public static final class State implements ReflectiveAccess.View {

        private final Object delegate;
        private State(Object delegate) { this.delegate = delegate; }
        public static State view(Object delegate) { return delegate == null ? null : new State(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof State value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public Route route() { return ProxyControlResultStoreAccess.Route.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$State", delegate, "route", new String[]{}, new Object[]{})); }
        public Map<UUID, StoredResult> results() { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$State", delegate, "results", new String[]{}, new Object[]{}); return rawResult == null ? null : ((java.util.Map<?, ?>) rawResult).entrySet().stream().collect(java.util.stream.Collectors.toMap(entry -> (UUID) entry.getKey(), entry -> ProxyControlResultStoreAccess.StoredResult.view(entry.getValue()))); }
        public boolean routeRequired() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ProxyControlResultStore$State", delegate, "routeRequired", new String[]{}, new Object[]{}); }
    }

}
