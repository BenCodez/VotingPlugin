package com.bencodez.votingplugin.backendproxy.http;

import java.security.cert.X509Certificate;
import java.security.KeyStore;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

/** Builds the backend TLS context.  A public CA store is intentionally not consulted. */
public final class HttpPinnedTls {
	private HttpPinnedTls() { }

	public static SSLContext clientContext(HttpConnectionCode code) throws Exception {
		if (code == null) throw new IllegalArgumentException("Connection code is required");
		SSLContext context = SSLContext.getInstance("TLS");
		context.init(null, new TrustManager[] { new PinnedServerTrustManager(code.serverCertificatePin(), code.caCertificatePin()) }, null);
		return context;
	}

	/**
	 * Normal transport context: presents the enrolled client certificate and accepts only the
	 * proxy leaf/CA pins in the connection code. Callers must not override the HttpClient default
	 * endpoint-identification settings; hostname verification remains enabled.
	 */
	public static SSLContext mutualTlsContext(HttpConnectionCode code, HttpClientCredentialStore.ClientCredential credential)
			throws Exception {
		if (code == null || credential == null || credential.privateKey() == null || credential.certificate() == null || credential.caCertificate() == null)
			throw new IllegalArgumentException("Enrolled client credential is required");
		char[] password = credential.password();
		try {
			KeyStore store = KeyStore.getInstance("PKCS12");
			store.load(null, new char[0]);
			store.setKeyEntry("client", credential.privateKey(), password,
					new java.security.cert.Certificate[] { credential.certificate(), credential.caCertificate() });
			KeyManagerFactory managers = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
			managers.init(store, password);
			SSLContext context = SSLContext.getInstance("TLS");
			context.init(managers.getKeyManagers(), new TrustManager[] {
					new PinnedServerTrustManager(code.serverCertificatePin(), code.caCertificatePin()) }, null);
			return context;
		} finally { java.util.Arrays.fill(password, '\0'); }
	}

	public static boolean matchesServerPin(HttpConnectionCode code, X509Certificate certificate) {
		if (code == null || certificate == null) return false;
		String actual = HttpTransportSecrets.certificatePin(certificate);
		return HttpTransportSecrets.constantTimeEquals(code.serverCertificatePin()
				.getBytes(java.nio.charset.StandardCharsets.US_ASCII), actual.getBytes(java.nio.charset.StandardCharsets.US_ASCII));
	}

	/** TLS 1.3 is used where the runtime exposes it; hostname verification is deliberately left enabled. */
	public static SSLParameters secureParameters(SSLContext context) {
		SSLParameters parameters = context.getDefaultSSLParameters();
		for (String protocol : context.getSupportedSSLParameters().getProtocols()) {
			if ("TLSv1.3".equals(protocol)) {
				parameters.setProtocols(new String[] { "TLSv1.3" });
				break;
			}
		}
		return parameters;
	}

	private static final class PinnedServerTrustManager implements X509TrustManager {
		private final String expectedPin;
		private final String expectedCaPin;
		private PinnedServerTrustManager(String expectedPin, String expectedCaPin) {
			this.expectedPin = expectedPin;
			this.expectedCaPin = expectedCaPin;
		}
		@Override public void checkClientTrusted(X509Certificate[] chain, String authType) { throw new UnsupportedOperationException(); }
		@Override public void checkServerTrusted(X509Certificate[] chain, String authType) throws java.security.cert.CertificateException {
			if (chain == null || chain.length < 2) throw new java.security.cert.CertificateException("Server certificate chain is incomplete");
			chain[0].checkValidity();
			chain[chain.length - 1].checkValidity();
			try { chain[0].verify(chain[chain.length - 1].getPublicKey()); }
			catch (java.security.GeneralSecurityException invalid) {
				throw new java.security.cert.CertificateException("Server certificate signature is invalid", invalid);
			}
			if (chain[chain.length - 1].getBasicConstraints() < 0)
				throw new java.security.cert.CertificateException("Server certificate authority is invalid");
			String actual = HttpTransportSecrets.certificatePin(chain[0]);
			if (!HttpTransportSecrets.constantTimeEquals(expectedPin.getBytes(java.nio.charset.StandardCharsets.US_ASCII),
					actual.getBytes(java.nio.charset.StandardCharsets.US_ASCII)))
				throw new java.security.cert.CertificateException("Server certificate pin does not match");
			String issuer = HttpTransportSecrets.certificatePin(chain[chain.length - 1]);
			if (!HttpTransportSecrets.constantTimeEquals(expectedCaPin.getBytes(java.nio.charset.StandardCharsets.US_ASCII),
					issuer.getBytes(java.nio.charset.StandardCharsets.US_ASCII)))
				throw new java.security.cert.CertificateException("Server certificate authority pin does not match");
		}
		@Override public X509Certificate[] getAcceptedIssuers() { return new X509Certificate[0]; }
	}
}
