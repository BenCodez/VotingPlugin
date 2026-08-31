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
		save(directory, issued);
		HttpClientProfile profile = new HttpClientProfile(HttpTlsIdentity.canonicalServerId(issued.serverId()), code.endpoint(),
				code.serverCertificatePin(), code.caCertificatePin());
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

	public static HttpClientProfile loadProfile(Path directory) throws IOException {
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

	/** Loads and cross-checks the persisted client key material and bound normal-transport profile. */
	public static EnrolledClient loadEnrolled(Path directory) throws Exception {
		ClientCredential credential = load(directory);
		HttpClientProfile profile = loadProfile(directory);
		if (!matchesProfile(credential, profile)) throw new IOException("HTTP client certificate does not match its profile");
		return new EnrolledClient(profile, credential);
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

	private static boolean matchesProfile(ClientCredential credential, HttpClientProfile profile) {
		try {
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

	private static byte[] asciiBytes(char[] characters) {
		byte[] output = new byte[characters.length];
		for (int index = 0; index < characters.length; index++) output[index] = (byte) characters[index];
		return output;
	}
}
