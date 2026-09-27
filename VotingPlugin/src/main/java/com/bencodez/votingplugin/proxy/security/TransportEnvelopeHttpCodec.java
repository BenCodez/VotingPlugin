package com.bencodez.votingplugin.proxy.security;

import java.util.Objects;
import java.util.function.Consumer;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.http.HttpEnvelopeWireCodec;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.Decryption;

/** Applies one immutable VotingPlugin encryption policy at the HTTP wire boundary. */
public final class TransportEnvelopeHttpCodec implements HttpEnvelopeWireCodec {
	private final TransportEnvelopeEncryption encryption;
	private final Consumer<String> rejectionLogger;

	public TransportEnvelopeHttpCodec(TransportEnvelopeEncryption encryption) {
		this(encryption, ignored -> { });
	}

	public TransportEnvelopeHttpCodec(TransportEnvelopeEncryption encryption, Consumer<String> rejectionLogger) {
		this.encryption = Objects.requireNonNull(encryption, "encryption");
		this.rejectionLogger = Objects.requireNonNull(rejectionLogger, "rejectionLogger");
	}

	@Override
	public JsonEnvelope encode(JsonEnvelope envelope) {
		return encryption.encrypt(envelope);
	}

	@Override
	public JsonEnvelope decode(JsonEnvelope envelope) {
		Decryption result = encryption.decrypt(envelope);
		if (!result.accepted()) {
			rejectionLogger.accept(result.reason());
			throw new IllegalArgumentException("HTTP envelope rejected by communication encryption policy");
		}
		return result.envelope();
	}
}
