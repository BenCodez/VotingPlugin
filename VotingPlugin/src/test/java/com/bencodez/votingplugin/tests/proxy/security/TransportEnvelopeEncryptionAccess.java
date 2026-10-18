package com.bencodez.votingplugin.tests.proxy.security;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.security.*;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.*;

/** Typed test-only access to package-private TransportEnvelopeEncryption seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class TransportEnvelopeEncryptionAccess {
    private TransportEnvelopeEncryptionAccess() { }
    public static TransportEnvelopeEncryption forTesting(byte[] key, Domain domain, boolean enabled) { return (TransportEnvelopeEncryption) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption", null, "forTesting", new String[]{"byte[]", "Domain", "boolean"}, new Object[]{key, domain, enabled}); }
}
