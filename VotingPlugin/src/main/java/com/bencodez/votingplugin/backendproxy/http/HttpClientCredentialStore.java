package com.bencodez.votingplugin.backendproxy.http;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.net.URI;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.Base64;
import java.util.EnumSet;
import java.util.Properties;
import com.bencodez.votingplugin.util.DurableFiles;

/** Owner-only persistence for the client certificate bundle returned by enrollment. */
public final class HttpClientCredentialStore {
	private static final String BUNDLE_FILE = "http-transport-client.p12";
	private static final String PASSWORD_FILE = "http-transport-client-password";
	private static final String PROFILE_FILE = "http-transport-profile.properties";
	private static final String CONNECTION_CODE_DIGEST_FILE = "http-transport-connection-code.sha256";
	private static final String GENERATIONS_DIRECTORY = "http-transport-client-generations";
	private static final String CURRENT_FILE = "http-transport-client-current";
	private HttpClientCredentialStore() { }

	public static void save(Path directory, HttpTlsIdentity.IssuedClientCertificate issued) throws IOException {
		if (issued == null) throw new IllegalArgumentException("Issued credential is required");
		Files.createDirectories(directory);
		byte[] bundle = issued.pkcs12();
		try { writePrivate(safe(directory.resolve(BUNDLE_FILE)), bundle); }
		finally { java.util.Arrays.fill(bundle, (byte) 0); }
		char[] password = issued.password();
		try { writePrivate(safe(directory.resolve(PASSWORD_FILE)), asciiBytes(password)); }
		finally { java.util.Arrays.fill(password, '\0'); }
	}

	/** Persists the certificate plus the non-secret normal-transport profile after enrollment. */
	public static void saveEnrolled(Path directory, HttpConnectionCode code, HttpTlsIdentity.IssuedClientCertificate issued)
			throws IOException {
		if (code == null || issued == null) throw new IllegalArgumentException("Connection code and credential are required");
		HttpClientProfile profile = new HttpClientProfile(HttpTlsIdentity.canonicalServerId(issued.serverId()), code.endpoint(),
				code.serverCertificatePin(), code.caCertificatePin());
		try {
			StagedCredential staged = stage(directory, issued, profile, connectionCodeDigest(code));
			activateReplacement(directory, staged);
		} catch (IOException failure) { throw failure;
		} catch (Exception failure) { throw new IOException("Could not persist HTTP client credential", failure); }
	}

	private static void writeProfile(Path directory, HttpClientProfile profile) throws IOException {
		Properties properties = new Properties();
		properties.setProperty("version", "1");
		properties.setProperty("serverId", profile.serverId());
		properties.setProperty("endpoint", profile.endpoint().toASCIIString());
		properties.setProperty("serverPin", profile.serverCertificatePin());
		properties.setProperty("caPin", profile.caCertificatePin());
		java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
		properties.store(bytes, "VotingPlugin HTTP transport profile");
		writePrivate(safe(directory.resolve(PROFILE_FILE)), bytes.toByteArray());
	}

	public static ClientCredential load(Path directory) throws Exception {
		return loadCredential(activeDirectory(directory));
	}

	private static ClientCredential loadCredential(Path directory) throws Exception {
		Path bundle = safe(directory.resolve(BUNDLE_FILE));
		Path passwordFile = safe(directory.resolve(PASSWORD_FILE));
		if (!Files.isRegularFile(bundle, LinkOption.NOFOLLOW_LINKS) || !Files.isRegularFile(passwordFile, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP client certificate has not been enrolled");
		byte[] passwordBytes = Files.readAllBytes(passwordFile);
		if (passwordBytes.length < 40 || passwordBytes.length > 128) throw new IOException("HTTP client password is invalid");
		char[] password = new String(passwordBytes, StandardCharsets.US_ASCII).toCharArray();
		java.util.Arrays.fill(passwordBytes, (byte) 0);
		try {
			KeyStore store = KeyStore.getInstance("PKCS12");
			try (var input = Files.newInputStream(bundle, LinkOption.NOFOLLOW_LINKS)) { store.load(input, password); }
			PrivateKey privateKey = (PrivateKey) store.getKey("client", password);
			java.security.cert.Certificate[] chain = store.getCertificateChain("client");
			if (privateKey == null || chain == null || chain.length != 2
					|| !(chain[0] instanceof X509Certificate client) || !(chain[1] instanceof X509Certificate authority))
				throw new IOException("HTTP client certificate bundle is invalid");
			return new ClientCredential(privateKey, client, authority, password);
		} finally { java.util.Arrays.fill(password, '\0'); }
	}

	/** Writes and validates a replacement generation without touching the active credential. */
	static StagedCredential stageReplacement(Path directory, HttpTlsIdentity.IssuedClientCertificate issued) throws Exception {
		Path active = activeDirectory(directory);
		return stage(directory, issued, loadProfileFile(active), readConnectionCodeDigest(active));
	}

	private static StagedCredential stage(Path directory, HttpTlsIdentity.IssuedClientCertificate issued,
			HttpClientProfile profile, String connectionCodeDigest) throws Exception {
		if (directory == null || issued == null) throw new IllegalArgumentException("Credential replacement is required");
		Path credentialDirectory = directory.toAbsolutePath().normalize();
		boolean created = !Files.exists(credentialDirectory, LinkOption.NOFOLLOW_LINKS);
		Path generations = credentialDirectory.resolve(GENERATIONS_DIRECTORY);
		Files.createDirectories(generations);
		// Credential files cannot make the newly created credential-root entry durable.
		// Persist its parent before an enrolled transport can activate this root.
		if (created) DurableFiles.forceDirectory(credentialDirectory.getParent());
		if (Files.isSymbolicLink(generations) || !Files.isDirectory(generations, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP credential generation directory is unsafe");
		String name = java.util.UUID.randomUUID().toString();
		Path generation = generations.resolve(name);
		Files.createDirectory(generation);
		setOwnerOnlyDirectory(generation);
		try {
			save(generation, issued);
			ClientCredential replacement = loadCredential(generation);
			profile = new HttpClientProfile(profile.serverId(), profile.endpoint(), profile.serverCertificatePin(),
					HttpTransportSecrets.certificatePin(replacement.caCertificate()));
			writeProfile(generation, profile);
			if (connectionCodeDigest != null) writePrivate(safe(generation.resolve(CONNECTION_CODE_DIGEST_FILE)),
					connectionCodeDigest.getBytes(StandardCharsets.US_ASCII));
			EnrolledClient enrolled = loadEnrolled(generation);
			// Each file is durable within the generation, but the generation name is
			// published by its parent. Persist it before CURRENT can activate it.
			DurableFiles.forceDirectory(generations);
			return new StagedCredential(name, enrolled.credential(), enrolled.profile());
		} catch (Exception failure) {
			try { Files.deleteIfExists(generation.resolve(BUNDLE_FILE)); Files.deleteIfExists(generation.resolve(PASSWORD_FILE));
				Files.deleteIfExists(generation.resolve(PROFILE_FILE)); Files.deleteIfExists(generation.resolve(CONNECTION_CODE_DIGEST_FILE));
				Files.deleteIfExists(generation); }
			catch (IOException cleanup) { failure.addSuppressed(cleanup); }
			throw failure;
		}
	}

	/** Atomically makes a fully validated generation durable and active. */
	static void activateReplacement(Path directory, StagedCredential staged) throws IOException {
		if (directory == null || staged == null || !staged.name().matches("[0-9a-f-]{36}"))
			throw new IllegalArgumentException("Staged credential is invalid");
		Path generations = directory.toAbsolutePath().normalize().resolve(GENERATIONS_DIRECTORY);
		Path generation = generations.resolve(staged.name()).normalize();
		if (!generation.getParent().equals(generations) || Files.isSymbolicLink(generation)
				|| !Files.isRegularFile(generation.resolve(BUNDLE_FILE), LinkOption.NOFOLLOW_LINKS)
				|| !Files.isRegularFile(generation.resolve(PASSWORD_FILE), LinkOption.NOFOLLOW_LINKS)
				|| !Files.isRegularFile(generation.resolve(PROFILE_FILE), LinkOption.NOFOLLOW_LINKS))
			throw new IOException("Staged HTTP credential is incomplete");
		writePrivate(safe(directory.resolve(CURRENT_FILE)), staged.name().getBytes(StandardCharsets.US_ASCII));
	}

	static record StagedCredential(String name, ClientCredential credential, HttpClientProfile profile) { }

	public static HttpClientProfile loadProfile(Path directory) throws IOException {
		return loadProfileFile(activeDirectory(directory));
	}

	private static HttpClientProfile loadProfileFile(Path directory) throws IOException {
		Path profile = safe(directory.resolve(PROFILE_FILE));
		if (!Files.isRegularFile(profile, LinkOption.NOFOLLOW_LINKS) || Files.size(profile) > 8192)
			throw new IOException("HTTP transport profile has not been enrolled");
		Properties properties = new Properties();
		try (var input = Files.newInputStream(profile, LinkOption.NOFOLLOW_LINKS)) { properties.load(input); }
		if (properties.size() != 5 || !"1".equals(properties.getProperty("version")))
			throw new IOException("HTTP transport profile is invalid");
		try {
			return new HttpClientProfile(properties.getProperty("serverId"), URI.create(properties.getProperty("endpoint")),
					properties.getProperty("serverPin"), properties.getProperty("caPin"));
		} catch (IllegalArgumentException failure) { throw new IOException("HTTP transport profile is invalid", failure); }
	}

	public static boolean hasEnrolledProfile(Path directory) {
		try { loadEnrolled(directory); return true; }
		catch (Exception unavailable) { return false; }
	}

	/** Returns whether this exact one-time code created the active credential, without persisting the code itself. */
	public static boolean matchesEnrollmentCode(Path directory, HttpConnectionCode code) throws IOException {
		if (code == null) throw new IllegalArgumentException("Connection code is required");
		String stored = readConnectionCodeDigest(activeDirectory(directory));
		if (stored == null) return false;
		return HttpTransportSecrets.constantTimeEquals(stored.getBytes(StandardCharsets.US_ASCII),
				connectionCodeDigest(code).getBytes(StandardCharsets.US_ASCII));
	}

	/** Loads and cross-checks the persisted client key material and bound normal-transport profile. */
	public static EnrolledClient loadEnrolled(Path directory) throws Exception {
		Path active = activeDirectory(directory);
		return loadEnrolledDirectory(active);
	}

	private static EnrolledClient loadEnrolledDirectory(Path active) throws Exception {
		ClientCredential credential = loadCredential(active);
		HttpClientProfile profile = loadProfileFile(active);
		if (!matchesProfile(credential, profile)) throw new IOException("HTTP client certificate does not match its profile");
		return new EnrolledClient(profile, credential);
	}

	/** Captures the exact credential generation used by a transport before a staged replacement starts. */
	public static ActiveCredentialGeneration snapshotActiveGeneration(Path directory) throws Exception {
		Path root = directory.toAbsolutePath().normalize();
		Path active = activeDirectory(root);
		EnrolledClient enrolled = loadEnrolledDirectory(active);
		return new ActiveCredentialGeneration(active.equals(root) ? "" : active.getFileName().toString(),
				enrolled.profile(), readConnectionCodeDigest(active));
	}

	/**
	 * Restores a pre-replacement generation unless the replacement already activated a
	 * newer credential for the same backend endpoint. A successful renewal or same-endpoint
	 * re-enrollment may revoke the snapshotted certificate at the proxy, so that newer
	 * generation is the only safe rollback identity.
	 */
	public static void restoreActiveGenerationAfterReplacement(Path directory,
			ActiveCredentialGeneration snapshot) throws Exception {
		if (directory == null || snapshot == null) throw new IllegalArgumentException("Credential rollback is required");
		HttpClientProfile previous = snapshot.profile();
		Path active = null;
		HttpClientProfile current = null;
		if (previous != null) try {
			active = activeDirectory(directory);
			current = loadEnrolledDirectory(active).profile();
		} catch (Exception unavailable) { active = null; current = null; }
		if (active != null && previous.serverId().equals(current.serverId())
				&& previous.endpoint().equals(current.endpoint())) {
			restoreConnectionCodeDigest(active, snapshot.connectionCodeDigest());
			return;
		}
		restoreActiveGeneration(directory, snapshot);
	}

	/** Atomically restores a previously validated credential generation after replacement rollback. */
	public static void restoreActiveGeneration(Path directory, ActiveCredentialGeneration snapshot) throws Exception {
		if (directory == null || snapshot == null) throw new IllegalArgumentException("Credential rollback is required");
		Path root = directory.toAbsolutePath().normalize();
		Path generations = root.resolve(GENERATIONS_DIRECTORY);
		Path target = snapshot.name().isEmpty() ? root : generations.resolve(snapshot.name()).normalize();
		if (!snapshot.name().isEmpty() && (!target.getParent().equals(generations)
				|| Files.isSymbolicLink(target) || !Files.isDirectory(target, LinkOption.NOFOLLOW_LINKS)))
			throw new IOException("HTTP client credential generation is invalid");
		loadEnrolledDirectory(target);
		Path current = safe(root.resolve(CURRENT_FILE));
		if (snapshot.name().isEmpty()) {
			Files.deleteIfExists(current);
			DurableFiles.forceDirectory(root);
		} else {
			writePrivate(current, snapshot.name().getBytes(StandardCharsets.US_ASCII));
		}
	}

	public record ClientCredential(PrivateKey privateKey, X509Certificate certificate, X509Certificate caCertificate, char[] password) {
		public ClientCredential { password = password.clone(); }
		@Override public char[] password() { return password.clone(); }
	}

	public record HttpClientProfile(String serverId, URI endpoint, String serverCertificatePin, String caCertificatePin) {
		public HttpClientProfile {
			serverId = HttpTlsIdentity.canonicalServerId(serverId);
			HttpConnectionCode validation = new HttpConnectionCode(serverId, endpoint, serverCertificatePin, caCertificatePin,
					java.time.Instant.now().plusSeconds(1), HttpTransportSecrets.randomToken());
			endpoint = validation.endpoint();
			serverCertificatePin = validation.serverCertificatePin();
			caCertificatePin = validation.caCertificatePin();
		}
	}

	public record EnrolledClient(HttpClientProfile profile, ClientCredential credential) { }

	public record ActiveCredentialGeneration(String name, HttpClientProfile profile, String connectionCodeDigest) {
		public ActiveCredentialGeneration(String name) { this(name, null, null); }
		public ActiveCredentialGeneration {
			if (name == null || (!name.isEmpty() && !name.matches("[0-9a-f-]{36}")))
				throw new IllegalArgumentException("HTTP client credential generation is invalid");
			if (connectionCodeDigest != null && !connectionCodeDigest.matches("[0-9a-f]{64}"))
				throw new IllegalArgumentException("HTTP connection-code marker is invalid");
		}
	}

	private static boolean matchesProfile(ClientCredential credential, HttpClientProfile profile) {
		try {
			String authorityPin = HttpTransportSecrets.certificatePin(credential.caCertificate());
			if (!HttpTransportSecrets.constantTimeEquals(profile.caCertificatePin().getBytes(StandardCharsets.US_ASCII),
					authorityPin.getBytes(StandardCharsets.US_ASCII))) return false;
			credential.certificate().checkValidity();
			credential.certificate().verify(credential.caCertificate().getPublicKey());
			java.util.List<String> usage = credential.certificate().getExtendedKeyUsage();
			boolean[] keyUsage = credential.certificate().getKeyUsage();
			if (usage == null || !usage.contains(org.bouncycastle.asn1.x509.KeyPurposeId.id_kp_clientAuth.getId())
					|| keyUsage == null || !keyUsage[0]) return false;
			String expected = "urn:votingplugin:http-backend:" + profile.serverId();
			var names = credential.certificate().getSubjectAlternativeNames();
			if (names == null) return false;
			for (java.util.List<?> name : names) if (name.size() == 2
					&& Integer.valueOf(6).equals(name.get(0)) && expected.equals(name.get(1))) return true;
			return false;
		} catch (Exception invalid) { return false; }
	}

	private static Path safe(Path file) throws IOException {
		if (Files.isSymbolicLink(file)) throw new IOException("Refusing unsafe HTTP credential path");
		return file.toAbsolutePath().normalize();
	}

	private static Path activeDirectory(Path directory) throws IOException {
		Path root = directory.toAbsolutePath().normalize();
		Path current = safe(root.resolve(CURRENT_FILE));
		if (!Files.exists(current, LinkOption.NOFOLLOW_LINKS)) return root;
		if (!Files.isRegularFile(current, LinkOption.NOFOLLOW_LINKS) || Files.size(current) > 64)
			throw new IOException("HTTP client credential pointer is invalid");
		String name = Files.readString(current, StandardCharsets.US_ASCII);
		if (!name.matches("[0-9a-f-]{36}")) throw new IOException("HTTP client credential pointer is invalid");
		Path generations = root.resolve(GENERATIONS_DIRECTORY);
		Path generation = generations.resolve(name).normalize();
		if (!generation.getParent().equals(generations) || Files.isSymbolicLink(generation) || !Files.isDirectory(generation, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP client credential generation is invalid");
		return generation;
	}

	private static String readConnectionCodeDigest(Path directory) throws IOException {
		Path digest = safe(directory.resolve(CONNECTION_CODE_DIGEST_FILE));
		if (!Files.exists(digest, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(digest, LinkOption.NOFOLLOW_LINKS) || Files.size(digest) != 64)
			throw new IOException("HTTP connection-code marker is invalid");
		String value = Files.readString(digest, StandardCharsets.US_ASCII);
		if (!value.matches("[0-9a-f]{64}")) throw new IOException("HTTP connection-code marker is invalid");
		return value;
	}

	private static String connectionCodeDigest(HttpConnectionCode code) {
		return HttpTransportSecrets.sha256Hex(code.encode().getBytes(StandardCharsets.US_ASCII));
	}

	private static void restoreConnectionCodeDigest(Path directory, String digest) throws IOException {
		Path marker = safe(directory.resolve(CONNECTION_CODE_DIGEST_FILE));
		if (digest == null) {
			Files.deleteIfExists(marker);
			DurableFiles.forceDirectory(directory);
		} else {
			writePrivate(marker, digest.getBytes(StandardCharsets.US_ASCII));
		}
	}

	private static void writePrivate(Path file, byte[] contents) throws IOException {
		Path temporary = Files.createTempFile(file.getParent(), file.getFileName().toString(), ".tmp");
		try {
			setOwnerOnly(temporary);
			Files.write(temporary, contents, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
			DurableFiles.forceFile(temporary);
			try { Files.move(temporary, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING); }
			catch (java.nio.file.AtomicMoveNotSupportedException unsupported) { Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING); }
			setOwnerOnly(file);
			DurableFiles.forceDirectory(file.getParent());
		} finally { Files.deleteIfExists(temporary); }
	}

	private static void setOwnerOnly(Path path) throws IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)); }
		catch (UnsupportedOperationException ignored) { }
	}

	private static void setOwnerOnlyDirectory(Path path) throws IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ,
				PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_EXECUTE)); }
		catch (UnsupportedOperationException ignored) { }
	}

	private static byte[] asciiBytes(char[] characters) {
		byte[] output = new byte[characters.length];
		for (int index = 0; index < characters.length; index++) output[index] = (byte) characters[index];
		return output;
	}
}
