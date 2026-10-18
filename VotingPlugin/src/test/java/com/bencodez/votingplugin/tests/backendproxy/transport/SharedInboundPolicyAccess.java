package com.bencodez.votingplugin.tests.backendproxy.transport;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.transport.*;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Test-only forwarding access to the non-public production SharedInboundPolicy. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedInboundPolicyAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private SharedInboundPolicyAccess(Object delegate) { this.delegate = delegate; }
    public static SharedInboundPolicyAccess view(Object delegate) { return delegate == null ? null : new SharedInboundPolicyAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedInboundPolicyAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedInboundPolicyAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy"))); }
    public SharedInboundPolicyAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedInboundPolicyAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedInboundPolicyAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedInboundPolicyAccess(Class<? extends BackendProxyTransport> transportType, String destination, SharedTransportEnvelopeAuthenticator authenticator, TransportEnvelopeEncryption encryption) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", new String[]{"Class", "String", "SharedTransportEnvelopeAuthenticator", "TransportEnvelopeEncryption"}, new Object[]{transportType, destination, authenticator, encryption})); }
    public Class<? extends BackendProxyTransport> transportType() { return (Class<? extends BackendProxyTransport>) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", delegate, "transportType", new String[]{}, new Object[]{}); }
    public String destination() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", delegate, "destination", new String[]{}, new Object[]{}); }
    public SharedTransportEnvelopeAuthenticator authenticator() { return (SharedTransportEnvelopeAuthenticator) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", delegate, "authenticator", new String[]{}, new Object[]{}); }
    public TransportEnvelopeEncryption encryption() { return (TransportEnvelopeEncryption) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", delegate, "encryption", new String[]{}, new Object[]{}); }
    public boolean hasEquivalentPolicy(SharedInboundPolicyAccess other) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.SharedInboundPolicy", delegate, "hasEquivalentPolicy", new String[]{"SharedInboundPolicy"}, new Object[]{other}); }
}
