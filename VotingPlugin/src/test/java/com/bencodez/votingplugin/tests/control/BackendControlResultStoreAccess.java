package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.Map;
import java.util.UUID;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.HostConfiguration;
import com.google.gson.JsonObject;

/** Test-only forwarding access to the non-public production BackendControlResultStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendControlResultStoreAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private BackendControlResultStoreAccess(Object delegate) { this.delegate = delegate; }
    public static BackendControlResultStoreAccess view(Object delegate) { return delegate == null ? null : new BackendControlResultStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof BackendControlResultStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static BackendControlResultStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.control.BackendControlResultStore"))); }
    public BackendControlResultStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public BackendControlResultStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public BackendControlResultStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public static State load(Path dataDirectory) throws IOException { return BackendControlResultStoreAccess.State.view(ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore", null, "load", new String[]{"Path"}, new Object[]{dataDirectory})); }
    public static void save(Path dataDirectory, Route route, HostConfiguration hosted, Map<UUID, StoredResult> results) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore", null, "save", new String[]{"Path", "Route", "HostConfiguration", "Map"}, new Object[]{dataDirectory, route, hosted, results}); }
    public static final class Route implements ReflectiveAccess.View {
        public Route() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlResultStore$Route", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private Route(Object delegate) { this.delegate = delegate; }
        public static Route view(Object delegate) { return delegate == null ? null : new Route(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Route value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public Route(String nodeId, URI endpoint, String credentialFile, int heartbeatSeconds, int connectTimeoutMillis, int requestTimeoutMillis) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlResultStore$Route", new String[]{"String", "URI", "String", "int", "int", "int"}, new Object[]{nodeId, endpoint, credentialFile, heartbeatSeconds, connectTimeoutMillis, requestTimeoutMillis})); }
        public String nodeId() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "nodeId", new String[]{}, new Object[]{}); }
        public URI endpoint() { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "endpoint", new String[]{}, new Object[]{}); }
        public String credentialFile() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "credentialFile", new String[]{}, new Object[]{}); }
        public int heartbeatSeconds() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "heartbeatSeconds", new String[]{}, new Object[]{}); }
        public int connectTimeoutMillis() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "connectTimeoutMillis", new String[]{}, new Object[]{}); }
        public int requestTimeoutMillis() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$Route", delegate, "requestTimeoutMillis", new String[]{}, new Object[]{}); }
    }

    public static final class StoredResult implements ReflectiveAccess.View {
        public StoredResult() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private StoredResult(Object delegate) { this.delegate = delegate; }
        public static StoredResult view(Object delegate) { return delegate == null ? null : new StoredResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof StoredResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public StoredResult(JsonObject result, boolean restartConnector, boolean committed, boolean claimRequired) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", new String[]{"JsonObject", "boolean", "boolean", "boolean"}, new Object[]{result, restartConnector, committed, claimRequired})); }
        public JsonObject result() { return (JsonObject) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", delegate, "result", new String[]{}, new Object[]{}); }
        public boolean restartConnector() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", delegate, "restartConnector", new String[]{}, new Object[]{}); }
        public boolean committed() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", delegate, "committed", new String[]{}, new Object[]{}); }
        public boolean claimRequired() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$StoredResult", delegate, "claimRequired", new String[]{}, new Object[]{}); }
    }

    public static final class State implements ReflectiveAccess.View {

        private final Object delegate;
        private State(Object delegate) { this.delegate = delegate; }
        public static State view(Object delegate) { return delegate == null ? null : new State(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof State value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public Route route() { return BackendControlResultStoreAccess.Route.view(ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$State", delegate, "route", new String[]{}, new Object[]{})); }
        public HostConfiguration hostedConfiguration() { return (HostConfiguration) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$State", delegate, "hostedConfiguration", new String[]{}, new Object[]{}); }
        public Map<UUID, StoredResult> results() { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlResultStore$State", delegate, "results", new String[]{}, new Object[]{}); return rawResult == null ? null : ((java.util.Map<?, ?>) rawResult).entrySet().stream().collect(java.util.stream.Collectors.toMap(entry -> (UUID) entry.getKey(), entry -> BackendControlResultStoreAccess.StoredResult.view(entry.getValue()))); }
    }

}
