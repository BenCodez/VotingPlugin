package com.bencodez.votingplugin.backendproxy.http;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Properties;
import java.util.Base64;
import java.util.EnumSet;
import com.bencodez.votingplugin.util.DurableFiles;

/**
 * Single-use enrollment tokens and client-certificate binding.  Token material is never retained;
 * only SHA-256 hashes are kept until expiry.  This type is thread-safe.
 */
public final class HttpEnrollmentAuthority {
	private static final Duration MAX_ENROLLMENT_LIFETIME = Duration.ofMinutes(15);
	private final HttpTlsIdentity identity;
	private final Clock clock;
	private final Path stateFile;
	private final Map<String, Enrollment> enrollments = new HashMap<>();
	private final Map<String, ClientBinding> bindings = new HashMap<>();
	private final Set<String> revokedCertificatePins = new HashSet<>();
	private boolean persistenceFailure;

	/** Creates a restart-safe authority. State contains only public certificate pins and revocations. */
	public HttpEnrollmentAuthority(HttpTlsIdentity identity, Path stateDirectory) throws java.io.IOException {
		this(identity, Clock.systemUTC(), stateFile(stateDirectory));
		loadState();
	}

	HttpEnrollmentAuthority(HttpTlsIdentity identity, Clock clock) {
		this(identity, clock, null);
	}

	HttpEnrollmentAuthority(HttpTlsIdentity identity, Clock clock, Path stateFile) {
		if (identity == null || clock == null) throw new IllegalArgumentException("Identity and clock are required");
		this.identity = identity;
		this.clock = clock;
		this.stateFile = stateFile;
	}

	public synchronized HttpConnectionCode createConnectionCode(String serverId, URI endpoint, Duration lifetime) {
		serverId = HttpTlsIdentity.canonicalServerId(serverId);
		if (lifetime == null || lifetime.isNegative() || lifetime.isZero() || lifetime.compareTo(MAX_ENROLLMENT_LIFETIME) > 0)
			throw new IllegalArgumentException("Enrollment lifetime must be between one second and fifteen minutes");
		expireEnrollments();
		Instant expiresAt = clock.instant().plus(lifetime);
		String token = HttpTransportSecrets.randomToken();
		byte[] tokenHash = HttpTransportSecrets.sha256(token.getBytes(StandardCharsets.US_ASCII));
		String lookup = java.util.Base64.getUrlEncoder().withoutPadding().encodeToString(tokenHash);
		enrollments.put(lookup, new Enrollment(tokenHash, expiresAt, serverId));
		return new HttpConnectionCode(serverId, endpoint, identity.serverCertificatePin(), identity.caCertificatePin(), expiresAt, token);
	}

	public synchronized HttpTlsIdentity.IssuedClientCertificate enroll(String serverId, String enrollmentToken) throws Exception {
		serverId = HttpTlsIdentity.canonicalServerId(serverId);
		if (enrollmentToken == null || enrollmentToken.length() > 128) throw new IllegalArgumentException("Enrollment was rejected");
		expireEnrollments();
		byte[] suppliedHash = HttpTransportSecrets.sha256(enrollmentToken.getBytes(StandardCharsets.US_ASCII));
		String lookup = java.util.Base64.getUrlEncoder().withoutPadding().encodeToString(suppliedHash);
		Enrollment enrollment = enrollments.get(lookup);
		if (enrollment == null || !HttpTransportSecrets.constantTimeEquals(enrollment.tokenHash(), suppliedHash))
			throw new IllegalArgumentException("Enrollment was rejected");
		if (!serverId.equals(enrollment.serverId())) throw new IllegalArgumentException("Enrollment was rejected");
		enrollments.remove(lookup); // consume only after the token and its intended backend both match.
		ClientBinding existing = bindings.get(serverId);
		if (existing != null && !existing.revoked()) throw new IllegalStateException("Server id is already enrolled");
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate(serverId);
		bindings.put(serverId, new ClientBinding(HttpTransportSecrets.certificatePin(issued.certificate()), null, false));
		try { persistState(); }
		catch (java.io.IOException failure) { persistenceFailure = true; throw failure; }
		return issued;
	}

	public synchronized boolean authenticate(String serverId, java.security.cert.X509Certificate certificate) {
		if (persistenceFailure || serverId == null || certificate == null) return false;
		try { serverId = HttpTlsIdentity.canonicalServerId(serverId); }
		catch (IllegalArgumentException invalid) { return false; }
		if (!identity.validClientCertificate(serverId, certificate)) return false;
		ClientBinding binding = bindings.get(serverId);
		String pin = HttpTransportSecrets.certificatePin(certificate);
		if (binding == null || binding.revoked() || revokedCertificatePins.contains(pin)) return false;
		if (samePin(binding.certificatePin(), pin)) return true;
		if (!samePin(binding.pendingCertificatePin(), pin)) return false;
		bindings.put(serverId, new ClientBinding(pin, null, false));
		revokedCertificatePins.add(binding.certificatePin());
		try { persistState(); return true; }
		catch (java.io.IOException failure) { persistenceFailure = true; return false; }
	}

	/** Issues a replacement while the currently bound certificate is still valid. The old binding remains active
	 * until the replacement successfully authenticates, making a lost renewal response safe to retry. */
	public synchronized HttpTlsIdentity.IssuedClientCertificate renew(String serverId,
			java.security.cert.X509Certificate currentCertificate) throws Exception {
		if (!authenticate(serverId, currentCertificate)) throw new IllegalArgumentException("Certificate renewal was rejected");
		serverId = HttpTlsIdentity.canonicalServerId(serverId);
		ClientBinding binding = bindings.get(serverId);
		HttpTlsIdentity.IssuedClientCertificate issued = identity.issueClientCertificate(serverId);
		bindings.put(serverId, new ClientBinding(binding.certificatePin(),
				HttpTransportSecrets.certificatePin(issued.certificate()), false));
		try { persistState(); }
		catch (java.io.IOException failure) { persistenceFailure = true; throw failure; }
		return issued;
	}

	public synchronized void revoke(String serverId) {
		try { serverId = HttpTlsIdentity.canonicalServerId(serverId); }
		catch (IllegalArgumentException invalid) { return; }
		ClientBinding binding = bindings.get(serverId);
		if (binding != null) {
			bindings.put(serverId, new ClientBinding(binding.certificatePin(), binding.pendingCertificatePin(), true));
			revokedCertificatePins.add(binding.certificatePin());
			if (binding.pendingCertificatePin() != null) revokedCertificatePins.add(binding.pendingCertificatePin());
			try { persistState(); }
			catch (java.io.IOException failure) { persistenceFailure = true; throw new IllegalStateException("Could not persist HTTP certificate revocation", failure); }
		}
	}

	private synchronized void loadState() throws java.io.IOException {
		if (stateFile == null || !Files.exists(stateFile, LinkOption.NOFOLLOW_LINKS)) return;
		if (!Files.isRegularFile(stateFile, LinkOption.NOFOLLOW_LINKS) || Files.size(stateFile) > 65536)
			throw new java.io.IOException("HTTP enrollment state is invalid");
		Properties properties = new Properties();
		try (var input = Files.newInputStream(stateFile, LinkOption.NOFOLLOW_LINKS)) { properties.load(input); }
		for (String key : properties.stringPropertyNames()) {
			if (key.startsWith("binding.")) {
				String serverId = new String(Base64.getUrlDecoder().decode(key.substring("binding.".length())), StandardCharsets.UTF_8);
				serverId = HttpTlsIdentity.canonicalServerId(serverId);
				String[] value = properties.getProperty(key, "").split(":", -1);
				if (!((value.length == 2 && "1".equals(properties.getProperty("version")))
						|| (value.length == 3 && "2".equals(properties.getProperty("version"))))
						|| !value[0].matches("[0-9a-f]{64}"))
					throw new java.io.IOException("HTTP enrollment state is invalid");
				String pending = value.length == 3 && !"-".equals(value[1]) ? value[1] : null;
				String revoked = value[value.length - 1];
				if ((pending != null && !pending.matches("[0-9a-f]{64}")) || !("0".equals(revoked) || "1".equals(revoked)))
					throw new java.io.IOException("HTTP enrollment state is invalid");
				bindings.put(serverId, new ClientBinding(value[0], pending, "1".equals(revoked)));
				if ("1".equals(revoked)) { revokedCertificatePins.add(value[0]); if (pending != null) revokedCertificatePins.add(pending); }
			} else if (!"version".equals(key)) throw new java.io.IOException("HTTP enrollment state is invalid");
		}
		if (!("1".equals(properties.getProperty("version")) || "2".equals(properties.getProperty("version"))))
			throw new java.io.IOException("HTTP enrollment state is invalid");
	}

	private synchronized void persistState() throws java.io.IOException {
		if (stateFile == null) return;
		Properties properties = new Properties();
		properties.setProperty("version", "2");
		for (Map.Entry<String, ClientBinding> entry : bindings.entrySet()) {
			String key = Base64.getUrlEncoder().withoutPadding().encodeToString(entry.getKey().getBytes(StandardCharsets.UTF_8));
			properties.setProperty("binding." + key, entry.getValue().certificatePin() + ":"
					+ (entry.getValue().pendingCertificatePin() == null ? "-" : entry.getValue().pendingCertificatePin())
					+ ":" + (entry.getValue().revoked() ? "1" : "0"));
		}
		java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
		properties.store(bytes, "VotingPlugin HTTP certificate bindings");
		Path temporary = Files.createTempFile(stateFile.getParent(), stateFile.getFileName().toString(), ".tmp");
		try {
			setOwnerOnly(temporary);
			Files.write(temporary, bytes.toByteArray(), StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
			DurableFiles.forceFile(temporary);
			try { Files.move(temporary, stateFile, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING); }
			catch (java.nio.file.AtomicMoveNotSupportedException unsupported) { Files.move(temporary, stateFile, StandardCopyOption.REPLACE_EXISTING); }
			setOwnerOnly(stateFile);
			DurableFiles.forceDirectory(stateFile.getParent());
		} finally { Files.deleteIfExists(temporary); }
	}

	private static Path stateFile(Path directory) throws java.io.IOException {
		if (directory == null) throw new IllegalArgumentException("State directory is required");
		Files.createDirectories(directory);
		Path file = directory.toAbsolutePath().normalize().resolve("http-transport-clients.properties");
		if (Files.isSymbolicLink(file)) throw new java.io.IOException("Refusing unsafe HTTP enrollment state path");
		return file;
	}

	private static void setOwnerOnly(Path path) throws java.io.IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)); }
		catch (UnsupportedOperationException ignored) { }
	}

	private void expireEnrollments() {
		Instant now = clock.instant();
		enrollments.entrySet().removeIf(entry -> !entry.getValue().expiresAt().isAfter(now));
	}

	private record Enrollment(byte[] tokenHash, Instant expiresAt, String serverId) {
		private Enrollment { tokenHash = tokenHash.clone(); }
		@Override public byte[] tokenHash() { return tokenHash.clone(); }
	}
	private static boolean samePin(String expected, String actual) {
		return expected != null && actual != null && HttpTransportSecrets.constantTimeEquals(
				expected.getBytes(StandardCharsets.US_ASCII), actual.getBytes(StandardCharsets.US_ASCII));
	}

	private record ClientBinding(String certificatePin, String pendingCertificatePin, boolean revoked) { }
}
