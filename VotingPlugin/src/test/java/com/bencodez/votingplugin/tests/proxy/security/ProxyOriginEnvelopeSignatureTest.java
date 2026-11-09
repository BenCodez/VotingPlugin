package com.bencodez.votingplugin.tests.proxy.security;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyPairGenerator;
import java.util.Base64;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature;
import com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain;

class ProxyOriginEnvelopeSignatureTest {
    @TempDir Path directory;
    ProxyOriginEnvelopeSignature signer;
    ProxyOriginEnvelopeSignature verifier;

    @BeforeEach void keys() throws Exception {
        var key = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        Files.writeString(directory.resolve("proxy-private.key"), Base64.getEncoder().encodeToString(key.getPrivate().getEncoded()));
        Files.writeString(directory.resolve("proxy-public.key"), Base64.getEncoder().encodeToString(key.getPublic().getEncoded()));
        signer = ProxyOriginEnvelopeSignature.loadSigner(directory.resolve("proxy-private.key"));
        verifier = ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("proxy-public.key"));
    }

    @Test void completeSerializedUnicodePayloadRetainsAllDataAndPermitsDurableQueueRetry() {
        var raw = JsonEnvelope.builder("Vote").put("name", "雪😀\r\nα").put("data", "{\"command\":\"data only\"}").build();
        var signed = signer.sign(raw, Domain.MYSQL, "proxy", "backend");
        var decoded = JsonEnvelopeCodec.decode(JsonEnvelopeCodec.encode(signed));
        var accepted = verifier.verify(decoded, Domain.MYSQL, "proxy", "backend");
        assertNotNull(accepted);
        assertEquals(raw.getFields(), accepted.getFields());
        assertNotNull(verifier.verify(decoded, Domain.MYSQL, "proxy", "backend"));
        // Origin proof does not expire retained SQL work; application IDs own deduplication.
        assertNotNull(verifier.verify(signer.sign(raw, Domain.MYSQL, "proxy", "backend"), Domain.MYSQL, "proxy", "backend"));
    }

    @Test void publicKeyHolderWrongRoleTargetDomainAndChangedPayloadHaveNoAuthority() throws Exception {
        var raw = JsonEnvelope.builder("Vote").put("voteId", "synthetic-occurrence").build();
        assertThrows(IllegalStateException.class, () -> verifier.sign(raw, Domain.MYSQL, "proxy", "backend"));
        var signed = signer.sign(raw, Domain.MYSQL, "proxy", "backend");
        assertNull(verifier.verify(raw, Domain.MYSQL, "proxy", "backend"));
        assertNull(verifier.verify(signed, Domain.REDIS, "proxy", "backend"));
        assertNull(verifier.verify(signed, Domain.MYSQL, "other-proxy", "backend"));
        assertNull(verifier.verify(signed, Domain.MYSQL, "proxy", "other-backend"));
        var fields = new java.util.TreeMap<>(signed.getFields());
        fields.put("voteId", "changed");
        assertNull(verifier.verify(new JsonEnvelope(signed.getSubChannel(), signed.getSchema(), fields), Domain.MYSQL, "proxy", "backend"));
        assertNull(verifier.verify(new JsonEnvelope("Other", signed.getSchema(), signed.getFields()), Domain.MYSQL, "proxy", "backend"));
        assertNull(verifier.verify(new JsonEnvelope(signed.getSubChannel(), signed.getSchema() + 1, signed.getFields()), Domain.MYSQL, "proxy", "backend"));
        assertNotNull(verifier.verify(signed, Domain.MYSQL, "proxy", "backend"), "invalid attempts must not poison legitimate input");
    }

    @Test void missingWrongMalformedOversizedAndPrivateVerificationKeysFailClosed() throws Exception {
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("absent")));
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("proxy-private.key")));
        Files.writeString(directory.resolve("invalid"), "invalid-key-data");
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("invalid")));
        Files.writeString(directory.resolve("oversized"), "a".repeat(4097));
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("oversized")));
        var other = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        Files.writeString(directory.resolve("other"), Base64.getEncoder().encodeToString(other.getPublic().getEncoded()));
        var wrong = ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("other"));
        var signed = signer.sign(JsonEnvelope.builder("Vote").build(), Domain.MYSQL, "proxy", "backend");
        assertNull(wrong.verify(signed, Domain.MYSQL, "proxy", "backend"));
        var wrongAlgorithm = KeyPairGenerator.getInstance("Ed448").generateKeyPair();
        Files.writeString(directory.resolve("wrong-algorithm-public"), Base64.getEncoder().encodeToString(wrongAlgorithm.getPublic().getEncoded()));
        Files.writeString(directory.resolve("wrong-algorithm-private"), Base64.getEncoder().encodeToString(wrongAlgorithm.getPrivate().getEncoded()));
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("wrong-algorithm-public")));
        assertThrows(java.io.IOException.class, () -> ProxyOriginEnvelopeSignature.loadSigner(directory.resolve("wrong-algorithm-private")));
    }

    @Test void unknownPartialOrSpoofedSignatureMetadataIsNeverUnsignedCompatibility() {
        for (var fields : java.util.List.of(Map.of("_vpOriginVersion", "1"), Map.of("_vpOriginExtra", "data"))) {
            var value = new JsonEnvelope("Vote", 1, fields);
            assertNull(verifier.verify(value, Domain.MYSQL, "proxy", "backend"));
            assertThrows(IllegalArgumentException.class, () -> signer.sign(value, Domain.MYSQL, "proxy", "backend"));
        }
    }

    @Test void validRepeatedSqlRetriesRemainPossibleButAncientCapturedProofsAndFutureProofsAreRejected() throws Exception {
        var pair = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
        var now = java.time.Instant.parse("2026-01-01T00:00:00Z");
        var constructor = ProxyOriginEnvelopeSignature.class.getDeclaredConstructor(
                java.security.PrivateKey.class, java.security.PublicKey.class, java.time.Clock.class);
        constructor.setAccessible(true);
        var signer = constructor.newInstance(pair.getPrivate(), null, java.time.Clock.fixed(now, java.time.ZoneOffset.UTC));
        var proof = signer.sign(JsonEnvelope.builder("Vote").put("voteId", "stable-id").build(), Domain.MYSQL, "proxy", "backend");
        var retry = constructor.newInstance(null, pair.getPublic(), java.time.Clock.fixed(now.plusSeconds(12 * 3600), java.time.ZoneOffset.UTC));
        assertNotNull(retry.verify(proof, Domain.MYSQL, "proxy", "backend"));
        assertNotNull(retry.verify(proof, Domain.MYSQL, "proxy", "backend"));
        for (long age : new long[] {24 * 3600 + 1, 8 * 24 * 3600, -121}) {
            var verifier = constructor.newInstance(null, pair.getPublic(), java.time.Clock.fixed(now.plusSeconds(age), java.time.ZoneOffset.UTC));
            assertNull(verifier.verify(proof, Domain.MYSQL, "proxy", "backend"));
        }
    }
}
