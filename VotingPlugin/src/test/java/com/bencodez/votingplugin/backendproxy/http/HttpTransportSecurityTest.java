package com.bencodez.votingplugin.backendproxy.http;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Arrays;
import javax.net.ssl.X509TrustManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HttpTransportSecurityTest {
	@Test
	void connectionCodeRejectsExplicitZeroPort() {
		assertThrows(IllegalArgumentException.class,
				() -> new HttpConnectionCode("lobby", URI.create("https://proxy.example.test:0/"), pin('a'),
						pin('b'), Instant.now().plusSeconds(60), "token"));
	}

	@TempDir Path directory;

	@Test
	void connectionCodeRoundTripsAndRejectsAccidentalCorruption() {
		HttpConnectionCode original = new HttpConnectionCode("lobby", URI.create("https://Proxy.Example.test:8443/http"), pin('a'), pin('b'),
				Instant.parse("2030-01-01T00:00:00Z"), HttpTransportSecrets.randomToken());
		String encoded = original.encode();
		HttpConnectionCode parsed = HttpConnectionCode.parse(encoded);
		assertEquals(URI.create("https://proxy.example.test:8443/http/"), parsed.endpoint());
		assertEquals(original.serverCertificatePin(), parsed.serverCertificatePin());
		char last = encoded.charAt(encoded.length() - 1);
		assertThrows(IllegalArgumentException.class, () -> HttpConnectionCode.parse(encoded.substring(0, encoded.length() - 1)
				+ (last == 'A' ? 'B' : 'A')));
		assertThrows(IllegalArgumentException.class, () -> HttpConnectionCode.parse("http://not-a-code"));
	}

	@Test
	void expiredCodesAreNotActive() {
		HttpConnectionCode code = new HttpConnectionCode("lobby", URI.create("https://proxy.example.test/"), pin('a'), pin('b'),
				Instant.parse("2029-12-31T23:59:59Z"), HttpTransportSecrets.randomToken());
		assertFalse(code.isActive(Clock.fixed(Instant.parse("2030-01-01T00:00:00Z"), ZoneOffset.UTC)));
		assertThrows(IllegalArgumentException.class, () -> code.requireActive(Clock.fixed(Instant.parse("2030-01-01T00:00:00Z"), ZoneOffset.UTC)));
	}

	@Test
	void identityIsDurableAndPinsRejectTheWrongServer() throws Exception {
		HttpTlsIdentity created = HttpTlsIdentity.loadOrCreate(directory, "localhost");
		HttpTlsIdentity loaded = HttpTlsIdentity.loadOrCreate(directory, "localhost");
		assertEquals(created.serverCertificatePin(), loaded.serverCertificatePin());
		assertEquals(created.caCertificatePin(), loaded.caCertificatePin());
		HttpConnectionCode correct = new HttpConnectionCode("lobby", URI.create("https://localhost:8443/"), created.serverCertificatePin(),
				created.caCertificatePin(), Instant.now().plusSeconds(60), HttpTransportSecrets.randomToken());
		HttpConnectionCode incorrect = new HttpConnectionCode("lobby", URI.create("https://localhost:8443/"), pin('0'), created.caCertificatePin(),
				Instant.now().plusSeconds(60), HttpTransportSecrets.randomToken());
		assertTrue(HttpPinnedTls.matchesServerPin(correct, created.serverCertificate()));
		assertFalse(HttpPinnedTls.matchesServerPin(incorrect, created.serverCertificate()));
		assertTrue(Files.exists(directory.resolve("http-transport-ca.p12")));
		HttpTlsIdentity rotated = HttpTlsIdentity.loadOrCreate(directory, "127.0.0.1");
		assertEquals(created.caCertificatePin(), rotated.caCertificatePin());
		assertNotEquals(created.serverCertificatePin(), rotated.serverCertificatePin());
	}

	@Test
	void enrollmentIsSingleUseBoundToServerAndRevocable() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory, "localhost");
		Clock clock = Clock.fixed(Instant.parse("2030-01-01T00:00:00Z"), ZoneOffset.UTC);
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, clock);
		HttpConnectionCode wrongTargetCode = authority.createConnectionCode("lobby-1", URI.create("https://localhost:8443/"), Duration.ofMinutes(5));
		assertThrows(IllegalArgumentException.class, () -> authority.enroll("attacker", wrongTargetCode.enrollmentToken()));
		assertTrue(authority.authenticate("lobby-1", authority.enroll("lobby-1", wrongTargetCode.enrollmentToken()).certificate()),
				"a wrong backend must not consume another backend's connection code");
		authority.revoke("lobby-1");
		HttpConnectionCode code = authority.createConnectionCode("lobby-1", URI.create("https://localhost:8443/"), Duration.ofMinutes(5));
		HttpTlsIdentity.IssuedClientCertificate issued = authority.enroll("lobby-1", code.enrollmentToken());
		assertTrue(authority.authenticate("lobby-1", issued.certificate()));
		assertTrue(identity.validClientCertificate("LOBBY-1", issued.certificate()));
		assertFalse(authority.authenticate("lobby-2", issued.certificate()));
		assertThrows(IllegalArgumentException.class, () -> authority.enroll("lobby-2", code.enrollmentToken()));
		authority.revoke("lobby-1");
		assertFalse(authority.authenticate("lobby-1", issued.certificate()));
		HttpConnectionCode replacementCode = authority.createConnectionCode("lobby-1", URI.create("https://localhost:8443/"), Duration.ofMinutes(5));
		HttpTlsIdentity.IssuedClientCertificate replacement = authority.enroll("lobby-1", replacementCode.enrollmentToken());
		assertTrue(authority.authenticate("lobby-1", replacement.certificate()));
		assertFalse(authority.authenticate("lobby-1", issued.certificate()));
		HttpClientCredentialStore.saveEnrolled(directory.resolve("client"), code, issued);
		HttpClientCredentialStore.ClientCredential restored = HttpClientCredentialStore.load(directory.resolve("client"));
		assertEquals(HttpTransportSecrets.certificatePin(issued.certificate()), HttpTransportSecrets.certificatePin(restored.certificate()));
		HttpClientCredentialStore.HttpClientProfile profile = HttpClientCredentialStore.loadProfile(directory.resolve("client"));
		assertEquals("lobby-1", profile.serverId());
		assertEquals(code.endpoint(), profile.endpoint());
		assertEquals("lobby-1", HttpClientCredentialStore.loadEnrolled(directory.resolve("client")).profile().serverId());
		assertNotEquals(null, HttpPinnedTls.mutualTlsContext(code, restored));
		Path clientDirectory = directory.resolve("client");
		String generation = Files.readString(clientDirectory.resolve("http-transport-client-current"));
		Files.writeString(clientDirectory.resolve("http-transport-client-generations").resolve(generation)
				.resolve("http-transport-profile.properties"), "version=1\nserverId=lobby-1\n");
		assertThrows(java.io.IOException.class, () -> HttpClientCredentialStore.loadProfile(directory.resolve("client")));
		HttpEnrollmentAuthority durable = new HttpEnrollmentAuthority(identity, directory.resolve("state"));
		HttpConnectionCode durableCode = durable.createConnectionCode("survival", URI.create("https://localhost:8443/"), Duration.ofMinutes(5));
		HttpTlsIdentity.IssuedClientCertificate durableIssued = durable.enroll("survival", durableCode.enrollmentToken());
		assertTrue(new HttpEnrollmentAuthority(identity, directory.resolve("state")).authenticate("survival", durableIssued.certificate()));
		durable.revoke("survival");
		assertFalse(new HttpEnrollmentAuthority(identity, directory.resolve("state")).authenticate("survival", durableIssued.certificate()));
	}

	@Test
	void renewalKeepsOldCredentialUntilReplacementAuthenticates() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpEnrollmentAuthority authority = new HttpEnrollmentAuthority(identity, directory.resolve("state"));
		HttpConnectionCode code = authority.createConnectionCode("lobby-1", URI.create("https://localhost:8443/"), Duration.ofMinutes(5));
		HttpTlsIdentity.IssuedClientCertificate original = authority.enroll("lobby-1", code.enrollmentToken());
		HttpTlsIdentity.IssuedClientCertificate replacement = authority.renew("lobby-1", original.certificate());

		assertTrue(authority.authenticate("lobby-1", original.certificate()), "lost renewal responses must leave the old credential usable");
		assertTrue(authority.authenticate("lobby-1", replacement.certificate()), "first replacement request promotes the pending binding");
		assertFalse(authority.authenticate("lobby-1", original.certificate()), "promotion revokes the superseded credential");
		assertTrue(new HttpEnrollmentAuthority(identity, directory.resolve("state"))
				.authenticate("lobby-1", replacement.certificate()), "promoted renewal must survive restart");
	}

	@Test
	void serverLeafRotatesInsideRenewalWindowAndPreservesAuthority() throws Exception {
		Instant now = Instant.now();
		HttpTlsIdentity original = HttpTlsIdentity.loadOrCreate(directory, "localhost",
				Clock.fixed(now.minus(Duration.ofDays(340)), ZoneOffset.UTC));
		String originalPin = HttpTransportSecrets.certificatePin(original.serverCertificate());
		HttpTlsIdentity renewed = HttpTlsIdentity.loadOrCreate(directory, "localhost", Clock.fixed(now, ZoneOffset.UTC));
		assertNotEquals(originalPin, renewed.serverCertificatePin());
		assertEquals(original.caCertificatePin(), renewed.caCertificatePin());
		assertFalse(HttpTlsIdentity.needsRenewal(renewed.serverCertificate(), Clock.fixed(now, ZoneOffset.UTC)));
	}

	@Test
	void activeTlsContextRotatesServerLeafInsideRenewalWindow() throws Exception {
		Instant now = Instant.now();
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory, "localhost",
				Clock.fixed(now.minus(Duration.ofDays(340)), ZoneOffset.UTC));
		String expiringPin = HttpTransportSecrets.certificatePin(identity.serverCertificate());
		identity.serverContext();
		assertNotEquals(expiringPin, identity.serverCertificatePin());
		assertFalse(HttpTlsIdentity.needsRenewal(identity.serverCertificate(), Clock.systemUTC()));
	}

	@Test
	void serverTlsUsesPrivateCaTrustAndRejectsForeignClients() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		HttpTlsIdentity foreign = HttpTlsIdentity.loadOrCreate(directory.resolve("foreign"), "localhost");
		X509TrustManager trust = Arrays.stream(HttpTlsIdentity.trustManagers(identity.caCertificate()))
				.filter(X509TrustManager.class::isInstance).map(X509TrustManager.class::cast).findFirst().orElseThrow();
		HttpTlsIdentity.IssuedClientCertificate accepted = identity.issueClientCertificate("lobby-1");
		HttpTlsIdentity.IssuedClientCertificate rejected = foreign.issueClientCertificate("lobby-1");
		assertDoesNotThrow(() -> trust.checkClientTrusted(
				new java.security.cert.X509Certificate[] { accepted.certificate(), identity.caCertificate() }, "EC"));
		assertThrows(java.security.cert.CertificateException.class, () -> trust.checkClientTrusted(
				new java.security.cert.X509Certificate[] { rejected.certificate(), foreign.caCertificate() }, "EC"));
	}

	@Test
	void stagedCredentialDoesNotReplaceActiveGenerationUntilAtomicActivation() throws Exception {
		HttpTlsIdentity identity = HttpTlsIdentity.loadOrCreate(directory.resolve("proxy"), "localhost");
		Path client = directory.resolve("client");
		HttpTlsIdentity.IssuedClientCertificate original = identity.issueClientCertificate("lobby-1");
		HttpConnectionCode code = new HttpConnectionCode("lobby-1", URI.create("https://localhost:8443/"),
				identity.serverCertificatePin(), identity.caCertificatePin(), Instant.now().plusSeconds(60), "A".repeat(43));
		HttpClientCredentialStore.saveEnrolled(client, code, original);
		String originalPin = HttpTransportSecrets.certificatePin(HttpClientCredentialStore.load(client).certificate());
		HttpTlsIdentity.IssuedClientCertificate replacement = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.StagedCredential staged = HttpClientCredentialStore.stageReplacement(client, replacement);
		assertEquals(originalPin, HttpTransportSecrets.certificatePin(HttpClientCredentialStore.load(client).certificate()));
		HttpClientCredentialStore.activateReplacement(client, staged);
		assertNotEquals(originalPin, HttpTransportSecrets.certificatePin(HttpClientCredentialStore.load(client).certificate()));
		assertTrue(HttpClientCredentialStore.matchesEnrollmentCode(client, code),
				"automatic certificate renewal must retain the consumed-code marker");
		HttpTlsIdentity.IssuedClientCertificate manuallyReenrolled = identity.issueClientCertificate("lobby-1");
		HttpClientCredentialStore.saveEnrolled(client, code, manuallyReenrolled);
		assertEquals(HttpTransportSecrets.certificatePin(manuallyReenrolled.certificate()),
				HttpTransportSecrets.certificatePin(HttpClientCredentialStore.loadEnrolled(client).credential().certificate()));
	}

	private static String pin(char character) { return String.valueOf(character).repeat(64); }
}
