package com.bencodez.votingplugin.proxy.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Rejection;

class SharedTransportEnvelopeAuthenticatorTest {
	private static final byte[] KEY = "0123456789abcdef0123456789abcdef".getBytes(java.nio.charset.StandardCharsets.US_ASCII);
	private static final Clock CLOCK = Clock.fixed(Instant.ofEpochMilli(1_800_000_000_000L), ZoneOffset.UTC);

	@Test
	void missingSettingDefaultsToUpgradeSafeCompatibility() {
		assertEquals(Mode.COMPATIBILITY, Mode.parse(null));
		assertEquals(Mode.COMPATIBILITY, Mode.parse(" "));
		assertEquals(Mode.REQUIRED, Mode.parse("REQUIRED"));
		assertThrows(IllegalArgumentException.class, () -> Mode.parse("COMPATIBLE"));
		assertThrows(IllegalArgumentException.class, () -> Mode.parse("invalid"));
	}

	@Test
	void authenticatesAndStripsTransportMetadata() {
		SharedTransportEnvelopeAuthenticator authenticator = authenticator();
		JsonEnvelope original = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).put("player", "Alex")
				.put(VotingPluginWire.K_VOTE_ID, "00000000-0000-0000-0000-000000000001").build();

		JsonEnvelope signed = authenticator.sign(original, Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel");
		SharedTransportEnvelopeAuthenticator.Verification result = authenticator.verify(signed,
				Domain.REDIS_PROXY_BACKEND, "test-channel");

		assertTrue(result.accepted());
		assertEquals(original.getFields(), result.envelope().getFields());
		assertNull(result.envelope().getFields().get(SharedTransportEnvelopeAuthenticator.K_MAC));
	}

	@Test
	void rejectsPayloadAndSubchannelTampering() {
		SharedTransportEnvelopeAuthenticator authenticator = authenticator();
		JsonEnvelope signed = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
				.put("player", "Alex").build(), Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel");

		JsonEnvelope payloadChanged = signed.toBuilder().put("player", "Mallory").build();
		JsonEnvelope subchannelChanged = copyAs(signed, VotingPluginWire.SUB_VOTE_UPDATE);
		JsonEnvelope senderChanged = signed.toBuilder().put(SharedTransportEnvelopeAuthenticator.K_SENDER,
				"proxy-b").build();

		assertEquals(Rejection.INVALID, authenticator.verify(payloadChanged, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		assertEquals(Rejection.INVALID, authenticator.verify(subchannelChanged, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		assertEquals(Rejection.INVALID, authenticator.verify(senderChanged, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
	}

	@Test
	void domainSeparatedMacCannotAuthenticateMultiProxyTraffic() {
		SharedTransportEnvelopeAuthenticator authenticator = authenticator();
		JsonEnvelope signed = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build(),
				Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel");

		assertEquals(Rejection.INVALID, authenticator.verify(signed, Domain.REDIS_MULTI_PROXY, "test-channel").rejection());
		assertEquals(Rejection.INVALID, authenticator.verify(signed, Domain.MQTT_PROXY_BACKEND, "test-channel").rejection());
	}

	@Test
	void signedMessagesAreBoundToEachBrokerDestination() {
		for (Domain domain : Domain.values()) {
			SharedTransportEnvelopeAuthenticator authenticator = authenticator();
			JsonEnvelope original = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
					.put(VotingPluginWire.K_VOTE_ID, "00000000-0000-0000-0000-000000000001").build();
			JsonEnvelope signed = authenticator.sign(original, domain, "proxy-a", "channel-a");

			assertEquals(Rejection.INVALID, authenticator.verify(signed, domain, "channel-b").rejection());
			assertEquals(0, authenticator.replayEntryCount());
			assertEquals(original.getFields(), authenticator.verify(signed, domain, "channel-a").envelope().getFields());
			assertEquals("2", signed.getFields().get(SharedTransportEnvelopeAuthenticator.K_VERSION));
		}
	}

	@Test
	void rejectsAuthenticatedEnvelopeOutsideTheFreshnessWindow() {
		SharedTransportEnvelopeAuthenticator signer = authenticator();
		SharedTransportEnvelopeAuthenticator laterVerifier = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.REQUIRED, Clock.offset(CLOCK,
						java.time.Duration.ofMillis(SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS + 1)));
		JsonEnvelope signed = signer.sign(JsonEnvelope.builder(VotingPluginWire.SUB_STATUS).build(),
				Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");

		assertEquals(Rejection.STALE, laterVerifier.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
	}

	@Test
	void rejectsExactReplayWithoutGrowingUnbounded() {
		int testCapacity = 64;
		SharedTransportEnvelopeAuthenticator authenticator = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.REQUIRED, CLOCK, testCapacity);
		JsonEnvelope signed = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_LOGIN).build(),
				Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");

		assertTrue(authenticator.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
		assertEquals(Rejection.REPLAY, authenticator.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		assertEquals(1, authenticator.replayEntryCount());

		for (int index = 1; index < testCapacity; index++) {
			JsonEnvelope next = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_LOGIN)
					.put("sequence", index).build(), Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");
			assertTrue(authenticator.verify(next, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
		}
		JsonEnvelope overflow = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_LOGIN)
				.put("sequence", "overflow").build(), Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");
		assertEquals(Rejection.CAPACITY, authenticator.verify(overflow, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		assertEquals(testCapacity, authenticator.replayEntryCount());
	}

	@Test
	void retainsFutureDatedNonceForItsEntireAcceptanceWindow() {
		long base = CLOCK.millis();
		MutableClock clock = new MutableClock(base + SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS);
		SharedTransportEnvelopeAuthenticator authenticator = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.REQUIRED, clock);
		JsonEnvelope signed = authenticator.sign(JsonEnvelope.builder(VotingPluginWire.SUB_LOGIN).build(),
				Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");

		clock.set(base);
		assertTrue(authenticator.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
		clock.set(base + SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS + 1L);
		assertEquals(Rejection.REPLAY, authenticator.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		clock.set(base + 2L * SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS + 1L);
		assertEquals(Rejection.STALE, authenticator.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
	}

	@Test
	void expiryPruningInspectsOnlyExpiredEntriesAndTheNextLiveEntry() {
		long base = CLOCK.millis();
		MutableClock signerClock = new MutableClock(base + SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS);
		MutableClock verifierClock = new MutableClock(base);
		SharedTransportEnvelopeAuthenticator signer = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.REQUIRED, signerClock);
		SharedTransportEnvelopeAuthenticator verifier = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.REQUIRED, verifierClock, 4_098);
		JsonEnvelope vote = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE).build();
		JsonEnvelope firstLive = null;
		for (int index = 0; index < 4_096; index++) {
			JsonEnvelope signed = signer.sign(vote, Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel");
			if (firstLive == null) firstLive = signed;
			assertTrue(verifier.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
		}
		signerClock.set(base - SharedTransportEnvelopeAuthenticator.MAX_CLOCK_SKEW_MILLIS);
		for (int index = 0; index < 2; index++) {
			JsonEnvelope signed = signer.sign(vote, Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel");
			assertTrue(verifier.verify(signed, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
		}
		assertEquals(4_098, verifier.replayEntryCount());

		verifierClock.set(base + 1);
		assertEquals(3, verifier.pruneExpired(verifierClock.millis()));
		assertEquals(4_096, verifier.replayEntryCount());
		assertEquals(1, verifier.pruneExpired(verifierClock.millis()));
		assertEquals(Rejection.REPLAY,
				verifier.verify(firstLive, Domain.REDIS_PROXY_BACKEND, "test-channel").rejection());
		signerClock.set(base + 1);
		assertTrue(verifier.verify(signer.sign(vote, Domain.REDIS_PROXY_BACKEND, "proxy-a", "test-channel"),
				Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
	}

	@Test
	void compatibilityModeAcceptsOnlyWhollyUnsignedLegacyEnvelope() {
		SharedTransportEnvelopeAuthenticator compatibility = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.COMPATIBILITY, CLOCK);
		JsonEnvelope unsigned = JsonEnvelope.builder(VotingPluginWire.SUB_STATUS).build();
		JsonEnvelope partiallySigned = unsigned.toBuilder()
				.put(SharedTransportEnvelopeAuthenticator.K_VERSION, "1").build();

		assertTrue(compatibility.verify(unsigned, Domain.REDIS_PROXY_BACKEND, "test-channel").unsignedCompatibility());
		assertFalse(compatibility.verify(partiallySigned, Domain.REDIS_PROXY_BACKEND, "test-channel").accepted());
	}

	@Test
	void compatibilityModeKeepsOutboundTrafficUnsignedUntilRequired() {
		SharedTransportEnvelopeAuthenticator compatibility = SharedTransportEnvelopeAuthenticator.forTesting(KEY,
				Mode.COMPATIBILITY, CLOCK);
		JsonEnvelope original = JsonEnvelope.builder(VotingPluginWire.SUB_STATUS).put("server", "backend-a").build();

		JsonEnvelope outbound = compatibility.sign(original, Domain.REDIS_PROXY_BACKEND, "backend-a", "test-channel");

		assertEquals(original.getFields(), outbound.getFields());
		assertFalse(outbound.getFields().containsKey(SharedTransportEnvelopeAuthenticator.K_MAC));
		assertEquals(original, compatibility.sign(original, Domain.REDIS_PROXY_BACKEND, null, "test-channel"));
		assertEquals(original, compatibility.sign(original, Domain.REDIS_PROXY_BACKEND, "", "test-channel"));
		assertEquals(original, compatibility.sign(original, Domain.REDIS_PROXY_BACKEND, "x".repeat(129), "test-channel"));
		assertThrows(IllegalArgumentException.class,
				() -> authenticator().sign(original, Domain.REDIS_PROXY_BACKEND, "", "test-channel"));
	}

	@Test
	void onlyRequiredModeNeedsAProvisionedKey(@TempDir Path dataDirectory) throws Exception {
		assertThrows(java.io.IOException.class, () -> SharedTransportEnvelopeAuthenticator.load(
				dataDirectory.resolve("missing-secretkey.key"), Mode.REQUIRED));
		SharedTransportEnvelopeAuthenticator compatibility = SharedTransportEnvelopeAuthenticator.load(
				dataDirectory.resolve("missing-secretkey.key"), Mode.COMPATIBILITY);
		JsonEnvelope original = JsonEnvelope.builder(VotingPluginWire.SUB_STATUS).build();
		assertEquals(original.getFields(), compatibility.sign(original, Domain.REDIS_PROXY_BACKEND,
				"backend-a", "test-channel").getFields());
	}

	private static SharedTransportEnvelopeAuthenticator authenticator() {
		return SharedTransportEnvelopeAuthenticator.forTesting(KEY, Mode.REQUIRED, CLOCK);
	}

	private static JsonEnvelope copyAs(JsonEnvelope source, String subchannel) {
		return JsonEnvelope.builder(subchannel).schema(source.getSchema()).putAll(source.getFields()).build();
	}

	private static final class MutableClock extends Clock {
		private final AtomicLong millis;

		private MutableClock(long millis) {
			this.millis = new AtomicLong(millis);
		}

		private void set(long value) {
			millis.set(value);
		}

		@Override
		public ZoneId getZone() {
			return ZoneOffset.UTC;
		}

		@Override
		public Clock withZone(ZoneId zone) {
			return this;
		}

		@Override
		public Instant instant() {
			return Instant.ofEpochMilli(millis());
		}

		@Override
		public long millis() {
			return millis.get();
		}
	}
}
