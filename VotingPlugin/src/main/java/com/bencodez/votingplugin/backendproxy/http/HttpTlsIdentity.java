package com.bencodez.votingplugin.backendproxy.http;

import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.time.Instant;
import java.util.Date;
import java.util.EnumSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.BasicConstraints;
import org.bouncycastle.asn1.x509.Extension;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.KeyUsage;
import org.bouncycastle.asn1.x509.ExtendedKeyUsage;
import org.bouncycastle.asn1.x509.KeyPurposeId;
import org.bouncycastle.cert.X509CertificateHolder;
import org.bouncycastle.cert.X509v3CertificateBuilder;
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter;
import org.bouncycastle.cert.jcajce.JcaX509v3CertificateBuilder;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.operator.ContentSigner;
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder;
import com.bencodez.votingplugin.util.DurableFiles;

/** Durable private CA plus server identity used by the proxy HTTP listener. */
public final class HttpTlsIdentity {
	private static final String CA_FILE = "http-transport-ca.p12";
	private static final String SERVER_FILE = "http-transport-server.p12";
	private static final String PASSWORD_FILE = "http-transport-password";
	private static final char[] EMPTY_PASSWORD = new char[0];
	private final PrivateKey caKey;
	private final X509Certificate caCertificate;
	private final PrivateKey serverKey;
	private final X509Certificate serverCertificate;
	private final char[] password;

	private HttpTlsIdentity(PrivateKey caKey, X509Certificate caCertificate, PrivateKey serverKey,
			X509Certificate serverCertificate, char[] password) {
		this.caKey = caKey;
		this.caCertificate = caCertificate;
		this.serverKey = serverKey;
		this.serverCertificate = serverCertificate;
		this.password = password.clone();
	}

	public static HttpTlsIdentity loadOrCreate(Path directory, String advertisedHost) throws Exception {
		if (advertisedHost == null || advertisedHost.isBlank() || advertisedHost.length() > 253)
			throw new IllegalArgumentException("Advertised HTTPS host is invalid");
		Files.createDirectories(directory);
		Path caFile = safe(directory.resolve(CA_FILE));
		Path serverFile = safe(directory.resolve(SERVER_FILE));
		Path passwordFile = safe(directory.resolve(PASSWORD_FILE));
		boolean caExists = Files.exists(caFile, LinkOption.NOFOLLOW_LINKS);
		boolean serverExists = Files.exists(serverFile, LinkOption.NOFOLLOW_LINKS);
		boolean passwordExists = Files.exists(passwordFile, LinkOption.NOFOLLOW_LINKS);
		if (caExists || serverExists || passwordExists) {
			if (!(caExists && serverExists && passwordExists)) throw new IOException("HTTP TLS identity files are incomplete");
			char[] password = readPassword(passwordFile);
			try {
				KeyStore ca = load(caFile, password);
				KeyStore server = load(serverFile, password);
				PrivateKey caKey = (PrivateKey) ca.getKey("ca", password);
				X509Certificate caCertificate = (X509Certificate) ca.getCertificate("ca");
				PrivateKey serverKey = (PrivateKey) server.getKey("server", password);
				X509Certificate serverCertificate = (X509Certificate) server.getCertificate("server");
				if (caKey == null || caCertificate == null || serverKey == null || serverCertificate == null)
					throw new IOException("HTTP TLS identity files are invalid");
				if (!hasServerName(serverCertificate, advertisedHost)) {
					ensureBouncyCastle();
					KeyPair serverPair = keyPair();
					serverCertificate = certificate("CN=" + certificateName(advertisedHost), serverPair, caCertificate, caKey,
							CertificateRole.SERVER, advertisedHost);
					serverKey = serverPair.getPrivate();
					server = KeyStore.getInstance("PKCS12");
					server.load(null, EMPTY_PASSWORD);
					server.setKeyEntry("server", serverKey, password, new Certificate[] { serverCertificate, caCertificate });
					writeStore(serverFile, server, password);
				}
				return new HttpTlsIdentity(caKey, caCertificate, serverKey, serverCertificate, password);
			} finally { Arrays.fill(password, '\0'); }
		}
		ensureBouncyCastle();
		char[] password = HttpTransportSecrets.randomToken().toCharArray();
		try {
			KeyPair caPair = keyPair();
			X509Certificate caCertificate = certificate("CN=VotingPlugin HTTP private CA", caPair, null, null, CertificateRole.CA, null);
			KeyPair serverPair = keyPair();
			X509Certificate serverCertificate = certificate("CN=" + certificateName(advertisedHost), serverPair, caCertificate,
					caPair.getPrivate(), CertificateRole.SERVER, advertisedHost);
			KeyStore ca = KeyStore.getInstance("PKCS12");
			ca.load(null, EMPTY_PASSWORD);
			ca.setKeyEntry("ca", caPair.getPrivate(), password, new Certificate[] { caCertificate });
			KeyStore server = KeyStore.getInstance("PKCS12");
			server.load(null, EMPTY_PASSWORD);
			server.setKeyEntry("server", serverPair.getPrivate(), password, new Certificate[] { serverCertificate, caCertificate });
			writeStore(caFile, ca, password);
			writeStore(serverFile, server, password);
		byte[] passwordBytes = asciiBytes(password);
		try { writePrivate(passwordFile, passwordBytes); }
		finally { Arrays.fill(passwordBytes, (byte) 0); }
			return new HttpTlsIdentity(caPair.getPrivate(), caCertificate, serverPair.getPrivate(), serverCertificate, password);
		} finally { Arrays.fill(password, '\0'); }
	}

	public String serverCertificatePin() { return HttpTransportSecrets.certificatePin(serverCertificate); }
	public String caCertificatePin() { return HttpTransportSecrets.certificatePin(caCertificate); }
	public X509Certificate caCertificate() { return caCertificate; }
	public X509Certificate serverCertificate() { return serverCertificate; }

	/**
	 * The listener requests a client certificate but validates its issuance/binding in the HTTP handler.
	 * This is necessary to share the enrollment and normal endpoints on one JDK HttpsServer listener.
	 */
	public SSLContext serverContext() throws Exception {
		KeyStore store = KeyStore.getInstance("PKCS12");
		store.load(null, EMPTY_PASSWORD);
		store.setKeyEntry("server", serverKey, password, new Certificate[] { serverCertificate, caCertificate });
		KeyManagerFactory keyManagers = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
		keyManagers.init(store, password);
		SSLContext context = SSLContext.getInstance("TLS");
		context.init(keyManagers.getKeyManagers(), new TrustManager[] { new EnrollmentAwareTrustManager(caCertificate) }, null);
		return context;
	}

	public IssuedClientCertificate issueClientCertificate(String serverId) throws Exception {
		serverId = canonicalServerId(serverId);
		ensureBouncyCastle();
		KeyPair pair = keyPair();
		X509Certificate certificate = certificate("CN=" + serverId, pair, caCertificate, caKey, CertificateRole.CLIENT,
				"urn:votingplugin:http-backend:" + serverId);
		char[] clientPassword = HttpTransportSecrets.randomToken().toCharArray();
		try {
			KeyStore store = KeyStore.getInstance("PKCS12");
			store.load(null, EMPTY_PASSWORD);
			store.setKeyEntry("client", pair.getPrivate(), clientPassword, new Certificate[] { certificate, caCertificate });
			java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
			store.store(bytes, clientPassword);
			return new IssuedClientCertificate(serverId, certificate, bytes.toByteArray(), clientPassword);
		} finally { Arrays.fill(clientPassword, '\0'); }
	}

	public static String canonicalServerId(String serverId) {
		if (serverId == null || !serverId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}"))
			throw new IllegalArgumentException("Server id is invalid");
		return serverId.toLowerCase(java.util.Locale.ROOT);
	}

	public boolean issuedByThisCa(X509Certificate certificate) {
		if (certificate == null) return false;
		try {
			certificate.checkValidity();
			certificate.verify(caCertificate.getPublicKey());
			return true;
		} catch (Exception failure) {
			return false;
		}
	}

	public boolean validClientCertificate(String expectedServerId, X509Certificate certificate) {
		if (!issuedByThisCa(certificate)) return false;
		try {
			List<String> usage = certificate.getExtendedKeyUsage();
			boolean[] keyUsage = certificate.getKeyUsage();
			if (usage == null || !usage.contains(KeyPurposeId.id_kp_clientAuth.getId()) || keyUsage == null || !keyUsage[0]) return false;
			String expectedUri = "urn:votingplugin:http-backend:" + canonicalServerId(expectedServerId);
			Collection<List<?>> names = certificate.getSubjectAlternativeNames();
			if (names == null) return false;
			for (List<?> name : names) {
				if (name.size() == 2 && Integer.valueOf(GeneralName.uniformResourceIdentifier).equals(name.get(0))
						&& expectedUri.equals(name.get(1))) return true;
			}
			return false;
		} catch (Exception failure) { return false; }
	}

	public record IssuedClientCertificate(String serverId, X509Certificate certificate, byte[] pkcs12, char[] password) {
		public IssuedClientCertificate {
			pkcs12 = pkcs12.clone();
			password = password.clone();
		}
		@Override public byte[] pkcs12() { return pkcs12.clone(); }
		@Override public char[] password() { return password.clone(); }
	}

	private static KeyPair keyPair() throws Exception {
		KeyPairGenerator generator = KeyPairGenerator.getInstance("EC");
		generator.initialize(new java.security.spec.ECGenParameterSpec("secp256r1"));
		return generator.generateKeyPair();
	}

	private static X509Certificate certificate(String subject, KeyPair subjectKey, X509Certificate issuer, PrivateKey issuerKey,
			CertificateRole role, String subjectAlternativeName) throws Exception {
		Instant now = Instant.now();
		X500Name issuerName = issuer == null ? new X500Name(subject) : new X500Name(issuer.getSubjectX500Principal().getName());
		X509v3CertificateBuilder builder = new JcaX509v3CertificateBuilder(issuerName,
				new BigInteger(160, new java.security.SecureRandom()).setBit(159), Date.from(now.minusSeconds(300)),
				Date.from(now.plusSeconds(role == CertificateRole.CA ? 315360000L : 31536000L)), new X500Name(subject), subjectKey.getPublic());
		builder.addExtension(Extension.basicConstraints, true, new BasicConstraints(role == CertificateRole.CA));
		builder.addExtension(Extension.keyUsage, true, new KeyUsage(role == CertificateRole.CA ? KeyUsage.keyCertSign | KeyUsage.cRLSign
				: KeyUsage.digitalSignature));
		if (role == CertificateRole.SERVER) builder.addExtension(Extension.extendedKeyUsage, false,
				new ExtendedKeyUsage(KeyPurposeId.id_kp_serverAuth));
		if (role == CertificateRole.CLIENT) builder.addExtension(Extension.extendedKeyUsage, false,
				new ExtendedKeyUsage(KeyPurposeId.id_kp_clientAuth));
		if (role == CertificateRole.SERVER && subjectAlternativeName != null) {
			GeneralName name;
			if (subjectAlternativeName.matches("(?:\\d{1,3}\\.){3}\\d{1,3}") || subjectAlternativeName.indexOf(':') >= 0)
				name = new GeneralName(GeneralName.iPAddress, subjectAlternativeName);
			else name = new GeneralName(GeneralName.dNSName, subjectAlternativeName);
			builder.addExtension(Extension.subjectAlternativeName, false, new GeneralNames(name));
		}
		if (role == CertificateRole.CLIENT) builder.addExtension(Extension.subjectAlternativeName, false,
				new GeneralNames(new GeneralName(GeneralName.uniformResourceIdentifier, subjectAlternativeName)));
		ContentSigner signer = new JcaContentSignerBuilder("SHA256withECDSA").setProvider("BC")
				.build(issuerKey == null ? subjectKey.getPrivate() : issuerKey);
		X509CertificateHolder holder = builder.build(signer);
		return new JcaX509CertificateConverter().setProvider("BC").getCertificate(holder);
	}

	private static void ensureBouncyCastle() {
		if (Security.getProvider("BC") == null) Security.addProvider(new BouncyCastleProvider());
	}

	private static String certificateName(String host) {
		return host.replaceAll("[^A-Za-z0-9 ._-]", "_");
	}

	private static boolean hasServerName(X509Certificate certificate, String advertisedHost) {
		try {
			Collection<List<?>> names = certificate.getSubjectAlternativeNames();
			if (names == null) return false;
			for (List<?> name : names) {
				if (name.size() != 2 || !(name.get(1) instanceof String value)) continue;
				if ((Integer.valueOf(GeneralName.dNSName).equals(name.get(0)) || Integer.valueOf(GeneralName.iPAddress).equals(name.get(0)))
						&& advertisedHost.equalsIgnoreCase(value)) return true;
			}
			return false;
		} catch (Exception failure) { return false; }
	}

	private static Path safe(Path file) throws IOException {
		Path parent = file.toAbsolutePath().normalize().getParent();
		if (parent == null || Files.isSymbolicLink(file)) throw new IOException("Refusing unsafe HTTP TLS identity path");
		return file.toAbsolutePath().normalize();
	}

	private static KeyStore load(Path path, char[] password) throws Exception {
		KeyStore store = KeyStore.getInstance("PKCS12");
		try (var input = Files.newInputStream(path, LinkOption.NOFOLLOW_LINKS)) { store.load(input, password); }
		return store;
	}

	private static char[] readPassword(Path path) throws IOException {
		byte[] bytes = Files.readAllBytes(path);
		if (bytes.length < 40 || bytes.length > 128) throw new IOException("HTTP TLS password file is invalid");
		try { return new String(bytes, java.nio.charset.StandardCharsets.US_ASCII).toCharArray(); }
		finally { Arrays.fill(bytes, (byte) 0); }
	}

	private static void writeStore(Path file, KeyStore store, char[] password) throws Exception {
		java.io.ByteArrayOutputStream bytes = new java.io.ByteArrayOutputStream();
		store.store(bytes, password);
		byte[] contents = bytes.toByteArray();
		try { writePrivate(file, contents); }
		finally { Arrays.fill(contents, (byte) 0); }
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

	private static byte[] asciiBytes(char[] characters) {
		byte[] output = new byte[characters.length];
		for (int index = 0; index < characters.length; index++) output[index] = (byte) characters[index];
		return output;
	}

	private static void setOwnerOnly(Path path) throws IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)); }
		catch (UnsupportedOperationException ignored) { /* Windows ACLs are inherited; never make the file world-readable. */ }
	}

	private static final class EnrollmentAwareTrustManager implements X509TrustManager {
		private final X509Certificate[] acceptedIssuers;
		private EnrollmentAwareTrustManager(X509Certificate caCertificate) { this.acceptedIssuers = new X509Certificate[] { caCertificate }; }
		@Override public void checkClientTrusted(X509Certificate[] chain, String authType) { }
		@Override public void checkServerTrusted(X509Certificate[] chain, String authType) { throw new UnsupportedOperationException(); }
		@Override public X509Certificate[] getAcceptedIssuers() { return acceptedIssuers.clone(); }
	}
	private enum CertificateRole { CA, SERVER, CLIENT }
}
