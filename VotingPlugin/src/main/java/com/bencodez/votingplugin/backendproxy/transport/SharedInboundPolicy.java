package com.bencodez.votingplugin.backendproxy.transport;

import java.util.Objects;

import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Immutable comparison view of the broker policy applied before backend dispatch. */
record SharedInboundPolicy(Class<? extends BackendProxyTransport> transportType, String destination,
		SharedTransportEnvelopeAuthenticator authenticator, TransportEnvelopeEncryption encryption) {
	boolean hasEquivalentPolicy(SharedInboundPolicy other) {
		return other != null && transportType == other.transportType && Objects.equals(destination, other.destination)
				&& authenticator != null && authenticator.hasEquivalentInboundPolicy(other.authenticator)
				&& encryption != null && encryption.hasEquivalentInboundPolicy(other.encryption);
	}
}
