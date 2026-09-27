package com.bencodez.votingplugin.backendproxy.transport;

import java.util.Objects;

import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Immutable authentication and encryption generation for one shared transport. */
record SharedTransportSecurityPolicy(SharedTransportEnvelopeAuthenticator authenticator,
		TransportEnvelopeEncryption encryption) {
	SharedTransportSecurityPolicy {
		Objects.requireNonNull(authenticator, "authenticator");
		Objects.requireNonNull(encryption, "encryption");
	}

	SharedTransportSecurityPolicy replace(SharedTransportEnvelopeAuthenticator replacementAuthenticator,
			TransportEnvelopeEncryption replacementEncryption) {
		return new SharedTransportSecurityPolicy(
				replacementAuthenticator == null ? authenticator : replacementAuthenticator,
				replacementEncryption == null ? encryption : replacementEncryption);
	}
}
