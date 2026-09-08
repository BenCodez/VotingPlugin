package com.bencodez.votingplugin.backendproxy.transport;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;

/**
 * Transport used by a backend server to communicate with the VotingPlugin proxy.
 */
public interface BackendProxyTransport {

	void start(GlobalMessageHandler messageHandler);

	/**
	 * Starts the transport, optionally allowing startup-only transient failures to
	 * be retried. Replacement transports disable this so Control validation stays
	 * fail-fast; ordinary startup retains its recovery loop.
	 */
	default void start(GlobalMessageHandler messageHandler, boolean retryInitialization) {
		start(messageHandler);
	}

	void send(JsonEnvelope envelope);

	default void validate() {
	}

	default void prepareForReplacement() {
		close();
	}

	/** Activates transport state that must not become visible before handler publication. */
	default void activateAfterPublication() {
	}

	void close();
}
