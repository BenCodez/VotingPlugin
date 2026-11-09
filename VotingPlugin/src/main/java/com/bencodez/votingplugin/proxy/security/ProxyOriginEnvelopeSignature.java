package com.bencodez.votingplugin.proxy.security;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.time.Clock;
import java.util.Base64;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Proxy-only signing authority, separate from shared broker/database membership. */
public final class ProxyOriginEnvelopeSignature {
    public enum Domain { MYSQL, REDIS, MQTT }
    private static final String PREFIX = "_vpOrigin";
    private static final String VERSION = PREFIX + "Version";
    private static final String PROOF = PREFIX + "Signature";
    private static final String TIME = PREFIX + "Time";
    // Shorter than the durable inbox's seven-day released-receipt retention.
    private static final long MAX_AGE_MILLIS = java.util.concurrent.TimeUnit.DAYS.toMillis(1);
    private static final long MAX_FUTURE_SKEW_MILLIS = 120_000L;
    private static final Set<String> METADATA = Set.of(VERSION, TIME, PROOF);
    private final PrivateKey signingKey;
    private final PublicKey verificationKey;
    private final Clock clock;

    private ProxyOriginEnvelopeSignature(PrivateKey signingKey, PublicKey verificationKey, Clock clock) {
        this.signingKey = signingKey;
        this.verificationKey = verificationKey;
        this.clock = clock;
    }

    /** Base64 DER PKCS8 private key; it must remain on the proxy. No key is generated or fetched. */
    public static ProxyOriginEnvelopeSignature loadSigner(Path file) throws IOException {
        try {
            PrivateKey key = KeyFactory.getInstance("Ed25519").generatePrivate(new PKCS8EncodedKeySpec(readKey(file)));
            if (!(key instanceof java.security.interfaces.EdECPrivateKey ed) || !"Ed25519".equals(ed.getParams().getName()))
                throw new GeneralSecurityException("Unsupported signing key");
            return new ProxyOriginEnvelopeSignature(key, null, Clock.systemUTC());
        } catch (GeneralSecurityException | IllegalArgumentException | IOException invalid) {
            throw new IOException("Proxy origin signing key is invalid");
        }
    }

    /** Base64 DER X509 public key; possessing this key grants no signing authority. */
    public static ProxyOriginEnvelopeSignature loadVerifier(Path file) throws IOException {
        try {
            PublicKey key = KeyFactory.getInstance("Ed25519").generatePublic(new X509EncodedKeySpec(readKey(file)));
            if (!(key instanceof java.security.interfaces.EdECPublicKey ed) || !"Ed25519".equals(ed.getParams().getName()))
                throw new GeneralSecurityException("Unsupported verification key");
            return new ProxyOriginEnvelopeSignature(null, key, Clock.systemUTC());
        } catch (GeneralSecurityException | IllegalArgumentException | IOException invalid) {
            throw new IOException("Proxy origin verification key is invalid");
        }
    }

    private static byte[] readKey(Path file) throws IOException {
        if (!Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) throw new IOException("Proxy origin key unavailable");
        try (var input = Files.newInputStream(file, LinkOption.NOFOLLOW_LINKS)) {
            byte[] bytes = input.readNBytes(4097);
            if (bytes.length == 0 || bytes.length > 4096) throw new IOException("Proxy origin key size invalid");
            return Base64.getDecoder().decode(new String(bytes, StandardCharsets.US_ASCII).trim());
        }
    }

    /** Durable local-admission evidence, separate from the fresh wire proof. */
    public String admissionProof(JsonEnvelope record, String proxy, String issuer) {
        if (signingKey == null) throw new IllegalStateException("Proxy signing authority unavailable");
        validateEnvelope(record);
        try {
            Signature signer = Signature.getInstance("Ed25519");
            signer.initSign(signingKey);
            signer.update("VotingPlugin local admission v1\0".getBytes(StandardCharsets.US_ASCII));
            signer.update(canonical(record, record.getFields(), Domain.MYSQL, proxy, issuer));
            return Base64.getUrlEncoder().withoutPadding().encodeToString(signer.sign());
        } catch (GeneralSecurityException failure) {
            throw new IllegalStateException("Proxy local admission signing failed");
        }
    }

    /** Ed25519 is deterministic; compare against this captured private authority. */
    public boolean acceptsAdmissionProof(JsonEnvelope record, String proxy, String issuer, String proof) {
        if (proof == null || proof.length() != 86) return false;
        try {
            return java.security.MessageDigest.isEqual(Base64.getUrlDecoder().decode(proof),
                    Base64.getUrlDecoder().decode(admissionProof(record, proxy, issuer)));
        } catch (IllegalArgumentException invalid) {
            return false;
        }
    }

    public JsonEnvelope sign(JsonEnvelope envelope, Domain domain, String proxy, String target) {
        if (signingKey == null) throw new IllegalStateException("Proxy signing authority unavailable");
        validateEnvelope(envelope);
        if (envelope.getFields().keySet().stream().anyMatch(key -> key.startsWith(PREFIX))) {
            throw new IllegalArgumentException("Proxy origin metadata already present");
        }
        var fields = new TreeMap<>(envelope.getFields());
        fields.put(VERSION, "1");
        fields.put(TIME, Long.toString(clock.millis()));
        try {
            Signature signer = Signature.getInstance("Ed25519");
            signer.initSign(signingKey);
            signer.update(canonical(envelope, fields, domain, proxy, target));
            fields.put(PROOF, Base64.getEncoder().encodeToString(signer.sign()));
            return new JsonEnvelope(envelope.getSubChannel(), envelope.getSchema(), fields);
        } catch (GeneralSecurityException failure) {
            throw new IllegalStateException("Proxy origin signing failed");
        }
    }

    /**
     * Null means rejected. SQL retries may present the same signed row within the proof lifetime;
     * application vote IDs and durable receipts own deduplication, not an expiring
     * signature nonce. Proof expiry is shorter than durable released-receipt retention;
     * the proxy re-signs outgoing retries. Broker replay checks remain in the outer MAC.
     */
    public JsonEnvelope verify(JsonEnvelope envelope, Domain domain, String proxy, String target) {
        if (verificationKey == null) return null;
        try {
            validateEnvelope(envelope);
            var fields = new TreeMap<>(envelope.getFields());
            if (!"1".equals(fields.get(VERSION)) || !fields.keySet().containsAll(METADATA)
                    || fields.keySet().stream().anyMatch(key -> key.startsWith(PREFIX) && !METADATA.contains(key))) return null;
            String time = fields.get(TIME);
            if (!time.matches("[0-9]{1,19}")) return null;
            long timestamp = Long.parseLong(time);
            long now = clock.millis();
            if (timestamp < now - MAX_AGE_MILLIS || timestamp > now + MAX_FUTURE_SKEW_MILLIS) return null;
            String rawProof = fields.remove(PROOF);
            if (rawProof.length() != 88) return null;
            byte[] proof = Base64.getDecoder().decode(rawProof);
            if (proof.length != 64 || !Base64.getEncoder().encodeToString(proof).equals(rawProof)) return null;
            Signature verifier = Signature.getInstance("Ed25519");
            verifier.initVerify(verificationKey);
            verifier.update(canonical(envelope, fields, domain, proxy, target));
            if (!verifier.verify(proof)) return null;
            METADATA.forEach(fields::remove);
            return new JsonEnvelope(envelope.getSubChannel(), envelope.getSchema(), fields);
        } catch (GeneralSecurityException | RuntimeException invalid) {
            return null;
        }
    }

    private static void validateEnvelope(JsonEnvelope envelope) {
        if (envelope == null || envelope.getSubChannel() == null || envelope.getSubChannel().length() > 128
                || envelope.getFields().size() > 128) throw new IllegalArgumentException("Invalid proxy origin envelope");
        envelope.getFields().forEach((key, value) -> {
            if (key == null || value == null || key.length() > 128 || value.length() > 256 * 1024) {
                throw new IllegalArgumentException("Invalid proxy origin field");
            }
        });
    }

    private static byte[] canonical(JsonEnvelope envelope, Map<String, String> fields,
            Domain domain, String proxy, String target) {
        if (domain == null || proxy == null || !proxy.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")
                || target == null || !target.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
            throw new IllegalArgumentException("Invalid proxy origin binding");
        }
        try {
            var bytes = new ByteArrayOutputStream();
            var output = new DataOutputStream(bytes);
            text(output, "votingplugin-proxy-origin-v1");
            text(output, domain.name());
            text(output, proxy);
            text(output, target);
            output.writeInt(envelope.getSchema());
            text(output, envelope.getSubChannel());
            output.writeInt(fields.size());
            for (var entry : fields.entrySet()) {
                text(output, entry.getKey());
                text(output, entry.getValue());
            }
            return bytes.toByteArray();
        } catch (IOException impossible) {
            throw new IllegalStateException(impossible);
        }
    }

    private static void text(DataOutputStream output, String value) throws IOException {
        var encoded = StandardCharsets.UTF_8.newEncoder().encode(java.nio.CharBuffer.wrap(value));
        output.writeInt(encoded.remaining());
        byte[] bytes = new byte[encoded.remaining()];
        encoded.get(bytes);
        output.write(bytes);
    }
}
