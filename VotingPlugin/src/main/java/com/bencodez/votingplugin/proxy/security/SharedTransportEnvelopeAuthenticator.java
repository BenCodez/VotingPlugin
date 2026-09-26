package com.bencodez.votingplugin.proxy.security;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.UUID;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Authenticates messages carried by shared broker transports. */
public final class SharedTransportEnvelopeAuthenticator {
	public enum Domain {
		REDIS_PROXY_BACKEND("votingplugin-shared-redis-proxy-backend-envelope-v1"),
		MQTT_PROXY_BACKEND("votingplugin-shared-mqtt-proxy-backend-envelope-v1"),
		REDIS_MULTI_PROXY("votingplugin-shared-redis-multi-proxy-envelope-v1");

		private final String value;

		Domain(String value) {
			this.value = value;
		}
	}

	public enum Mode {
		REQUIRED,
		COMPATIBILITY;

		public static Mode parse(String configured) {
			if (configured == null || configured.isBlank()
					|| "COMPATIBILITY".equalsIgnoreCase(configured.trim())) return COMPATIBILITY;
			if ("REQUIRED".equalsIgnoreCase(configured.trim())) return REQUIRED;
			throw new IllegalArgumentException(
					"SharedTransportAuthentication must be COMPATIBILITY or REQUIRED");
		}
	}

	public enum Rejection {
		NONE,
		MISSING,
		MALFORMED,
		INVALID,
		STALE,
		REPLAY,
		CAPACITY
	}

	public record Verification(boolean accepted, boolean unsignedCompatibility, Rejection rejection,
			JsonEnvelope envelope) {
		private static Verification accepted(JsonEnvelope envelope, boolean unsignedCompatibility) {
			return new Verification(true, unsignedCompatibility, Rejection.NONE, envelope);
		}

		private static Verification rejected(Rejection rejection) {
			return new Verification(false, false, rejection, null);
		}
	}

	public static final String K_VERSION = "_vpAuthVersion";
	public static final String K_SENDER = "_vpAuthSender";
	public static final String K_TIMESTAMP = "_vpAuthTimestamp";
	public static final String K_MESSAGE_ID = "_vpAuthMessageId";
	public static final String K_MAC = "_vpAuthMac";
	// Four minutes is the longest possible retention for a message signed at the
	// positive skew boundary. This capacity sustains more than 270 messages/second
	// for that entire worst-case window without weakening replay rejection.
	static final int MAX_REPLAY_ENTRIES = 65_536;
	static final long MAX_CLOCK_SKEW_MILLIS = 2 * 60 * 1000L;
	private static final String ALGORITHM = "HmacSHA256";
	private static final String DERIVATION_DOMAIN = "votingplugin-shared-transport-key-v1";
	private static final String VERSION = "2";
	private static final String LEGACY_VERSION = "1";

	private final Map<Domain, byte[]> domainKeys;
	private final Mode mode;
	private final Clock clock;
	private final int maxReplayEntries;
	private final Map<UUID, Long> acceptedMessages = new HashMap<>();
	private final PriorityQueue<ReplayEntry> expiryOrder = new PriorityQueue<>(
			Comparator.comparingLong(ReplayEntry::expiresAt));

	private record ReplayEntry(UUID messageId, long expiresAt) {
	}

	private SharedTransportEnvelopeAuthenticator(byte[] masterKey, Mode mode, Clock clock) {
		this(masterKey, mode, clock, MAX_REPLAY_ENTRIES);
	}

	private SharedTransportEnvelopeAuthenticator(byte[] masterKey, Mode mode, Clock clock, int maxReplayEntries) {
		domainKeys = new EnumMap<>(Domain.class);
		if (masterKey != null) {
			for (Domain domain : Domain.values()) domainKeys.put(domain, deriveKey(masterKey, domain));
		}
		this.mode = mode;
		this.clock = clock;
		this.maxReplayEntries = maxReplayEntries;
	}

	public static SharedTransportEnvelopeAuthenticator load(Path keyFile, Mode mode) throws IOException {
		if (!Files.isRegularFile(keyFile)) {
			if (mode == Mode.COMPATIBILITY)
				return new SharedTransportEnvelopeAuthenticator(null, mode, Clock.systemUTC());
			throw new IOException("Shared Redis/MQTT transport authentication requires a shared secretkey.key");
		}
		try {
			byte[] decoded = Base64.getDecoder().decode(Files.readString(keyFile, StandardCharsets.US_ASCII).trim());
			if (decoded.length < 16) throw new IOException("Shared transport authentication key is too short");
			return new SharedTransportEnvelopeAuthenticator(decoded, mode, Clock.systemUTC());
		} catch (IllegalArgumentException invalid) {
			throw new IOException("Shared transport authentication key is invalid", invalid);
		}
	}

	static SharedTransportEnvelopeAuthenticator forTesting(byte[] key, Mode mode, Clock clock) {
		return new SharedTransportEnvelopeAuthenticator(key, mode, clock);
	}

	static SharedTransportEnvelopeAuthenticator forTesting(byte[] key, Mode mode, Clock clock,
			int maxReplayEntries) {
		return new SharedTransportEnvelopeAuthenticator(key, mode, clock, maxReplayEntries);
	}

	public Mode mode() {
		return mode;
	}

	public JsonEnvelope sign(JsonEnvelope envelope, Domain domain, String sender, String destination) {
		if (envelope == null) throw new IllegalArgumentException("envelope is required");
		Objects.requireNonNull(domain, "domain");
		// Compatibility traffic remains indistinguishable from a legacy sender. Signing
		// with a node-local key during a rolling upgrade would make upgraded peers reject
		// one another as soon as both happened to have generated different keys.
		if (mode == Mode.COMPATIBILITY) return envelope;
		if (sender == null || sender.isBlank() || sender.length() > 128)
			throw new IllegalArgumentException("shared transport sender identity is invalid");
		if (destination == null || destination.isBlank())
			throw new IllegalArgumentException("shared transport destination is invalid");
		if (domainKeys.isEmpty()) {
			throw new IllegalStateException("Shared transport authentication key is unavailable");
		}
		long timestamp = clock.millis();
		String messageId = UUID.randomUUID().toString();
		JsonEnvelope unsigned = withAuthenticationFields(envelope, sender, timestamp, messageId, null);
		String mac = calculateMac(unsigned, domain, destination, true);
		return withAuthenticationFields(envelope, sender, timestamp, messageId, mac);
	}

	public synchronized Verification verify(JsonEnvelope envelope, Domain domain, String destination) {
		if (envelope == null) return Verification.rejected(Rejection.MALFORMED);
		Objects.requireNonNull(domain, "domain");
		Map<String, String> fields = envelope.getFields();
		boolean anyAuthenticationField = fields.containsKey(K_VERSION) || fields.containsKey(K_SENDER)
				|| fields.containsKey(K_TIMESTAMP) || fields.containsKey(K_MESSAGE_ID) || fields.containsKey(K_MAC);
		if (!anyAuthenticationField) {
			if (mode == Mode.COMPATIBILITY) return Verification.accepted(envelope, true);
			return Verification.rejected(Rejection.MISSING);
		}
		String version = fields.get(K_VERSION);
		boolean destinationBound = VERSION.equals(version);
		if (domainKeys.isEmpty() || (!destinationBound
				&& !(mode == Mode.COMPATIBILITY && LEGACY_VERSION.equals(version)))
				|| (destinationBound && (destination == null || destination.isBlank())))
			return Verification.rejected(Rejection.MALFORMED);
		String sender = fields.get(K_SENDER);
		String messageId = fields.get(K_MESSAGE_ID);
		String suppliedMac = fields.get(K_MAC);
		long timestamp;
		UUID parsedMessageId;
		try {
			timestamp = Long.parseLong(fields.get(K_TIMESTAMP));
			parsedMessageId = UUID.fromString(messageId);
		} catch (RuntimeException malformed) {
			return Verification.rejected(Rejection.MALFORMED);
		}
		if (sender == null || sender.isBlank() || sender.length() > 128 || suppliedMac == null
				|| !suppliedMac.matches("[0-9a-f]{64}"))
			return Verification.rejected(Rejection.MALFORMED);

		String expected = calculateMac(withoutMac(envelope), domain, destination, destinationBound);
		if (!MessageDigest.isEqual(expected.getBytes(StandardCharsets.US_ASCII),
				suppliedMac.getBytes(StandardCharsets.US_ASCII))) return Verification.rejected(Rejection.INVALID);

		long now = clock.millis();
		if (timestamp < now - MAX_CLOCK_SKEW_MILLIS || timestamp > now + MAX_CLOCK_SKEW_MILLIS)
			return Verification.rejected(Rejection.STALE);
		pruneExpired(now);
		if (acceptedMessages.containsKey(parsedMessageId)) return Verification.rejected(Rejection.REPLAY);
		if (acceptedMessages.size() >= maxReplayEntries) return Verification.rejected(Rejection.CAPACITY);
		// Retain the nonce for the envelope's complete acceptance window. A sender may
		// legitimately be ahead by MAX_CLOCK_SKEW_MILLIS, so retention measured from
		// local acceptance time would otherwise leave a second replay window.
		long expiresAt = replayExpiry(timestamp);
		acceptedMessages.put(parsedMessageId, expiresAt);
		expiryOrder.add(new ReplayEntry(parsedMessageId, expiresAt));
		return Verification.accepted(stripAuthenticationFields(envelope), false);
	}

	public synchronized int replayEntryCount() {
		return acceptedMessages.size();
	}

	// Return the number of heap heads inspected so expiry cost can be asserted
	// without a timing-sensitive performance test.
	synchronized int pruneExpired(long now) {
		int inspected = 0;
		while (true) {
			inspected++;
			ReplayEntry next = expiryOrder.peek();
			if (next == null || next.expiresAt() >= now) return inspected;
			ReplayEntry expired = expiryOrder.remove();
			acceptedMessages.remove(expired.messageId(), expired.expiresAt());
		}
	}

	private static long replayExpiry(long timestamp) {
		if (timestamp > Long.MAX_VALUE - MAX_CLOCK_SKEW_MILLIS) return Long.MAX_VALUE;
		return timestamp + MAX_CLOCK_SKEW_MILLIS;
	}

	private String calculateMac(JsonEnvelope envelope, Domain domain, String destination, boolean destinationBound) {
		try {
			Mac mac = Mac.getInstance(ALGORITHM);
			mac.init(new SecretKeySpec(domainKeys.get(domain), ALGORITHM));
			update(mac, domain.value);
			if (destinationBound) update(mac, destination);
			update(mac, Integer.toString(envelope.getSchema()));
			update(mac, envelope.getFields().get(K_SENDER));
			update(mac, envelope.getSubChannel());
			update(mac, envelope.getFields().get(K_TIMESTAMP));
			update(mac, envelope.getFields().get(K_MESSAGE_ID));
			List<Map.Entry<String, String>> fields = new ArrayList<>(envelope.getFields().entrySet());
			fields.removeIf(entry -> isAuthenticationField(entry.getKey()));
			fields.sort(Comparator.comparing(Map.Entry::getKey));
			for (Map.Entry<String, String> entry : fields) {
				update(mac, entry.getKey());
				update(mac, entry.getValue());
			}
			return HexFormat.of().formatHex(mac.doFinal());
		} catch (GeneralSecurityException impossible) {
			throw new IllegalStateException("HMAC-SHA-256 is unavailable", impossible);
		}
	}

	private static byte[] deriveKey(byte[] masterKey, Domain domain) {
		try {
			Mac derivation = Mac.getInstance(ALGORITHM);
			derivation.init(new SecretKeySpec(masterKey, ALGORITHM));
			update(derivation, DERIVATION_DOMAIN);
			update(derivation, domain.value);
			return derivation.doFinal();
		} catch (GeneralSecurityException impossible) {
			throw new IllegalStateException("HMAC-SHA-256 is unavailable", impossible);
		}
	}

	private static JsonEnvelope withAuthenticationFields(JsonEnvelope envelope, String sender, long timestamp,
			String messageId, String mac) {
		JsonEnvelope.Builder builder = copyWithoutAuthenticationFields(envelope).toBuilder().put(K_VERSION, VERSION)
				.put(K_SENDER, sender == null ? "" : sender).put(K_TIMESTAMP, timestamp).put(K_MESSAGE_ID, messageId);
		if (mac != null) builder.put(K_MAC, mac);
		return builder.build();
	}

	private static JsonEnvelope withoutMac(JsonEnvelope envelope) {
		JsonEnvelope.Builder builder = JsonEnvelope.builder(envelope.getSubChannel()).schema(envelope.getSchema());
		for (Map.Entry<String, String> entry : envelope.getFields().entrySet()) {
			if (!K_MAC.equals(entry.getKey())) builder.put(entry.getKey(), entry.getValue());
		}
		return builder.build();
	}

	private static JsonEnvelope stripAuthenticationFields(JsonEnvelope envelope) {
		return copyWithoutAuthenticationFields(envelope);
	}

	private static JsonEnvelope copyWithoutAuthenticationFields(JsonEnvelope envelope) {
		JsonEnvelope.Builder builder = JsonEnvelope.builder(envelope.getSubChannel()).schema(envelope.getSchema());
		for (Map.Entry<String, String> entry : envelope.getFields().entrySet()) {
			if (!isAuthenticationField(entry.getKey())) builder.put(entry.getKey(), entry.getValue());
		}
		return builder.build();
	}

	private static boolean isAuthenticationField(String key) {
		return K_VERSION.equals(key) || K_SENDER.equals(key) || K_TIMESTAMP.equals(key)
				|| K_MESSAGE_ID.equals(key) || K_MAC.equals(key);
	}

	private static void update(Mac mac, String value) {
		byte[] bytes = value == null ? new byte[0] : value.getBytes(StandardCharsets.UTF_8);
		mac.update(ByteBuffer.allocate(Integer.BYTES).putInt(bytes.length).array());
		mac.update(bytes);
	}
}
