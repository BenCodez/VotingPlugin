package com.bencodez.votingplugin.proxy.velocity;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;

/** Loads the HTTP TLS libraries that Velocity does not resolve from plugin metadata. */
final class VelocityRuntimeLibraries {
	private static final String CENTRAL_MIRROR = "https://maven-central.storage-download.googleapis.com/maven2/";
	private static final int CONNECT_TIMEOUT_MILLIS = 10_000;
	private static final int READ_TIMEOUT_MILLIS = 30_000;
	private static final long MAX_ARTIFACT_BYTES = 16L * 1024L * 1024L;
	private static final List<Library> LIBRARIES = List.of(
			library("org/bouncycastle/bcprov-jdk18on/1.85/bcprov-jdk18on-1.85.jar",
					"20af26bf6060bb8005cc2389916812c1e0e998dc48d2ced7131b89461b54cff7"),
			library("org/bouncycastle/bcutil-jdk18on/1.85/bcutil-jdk18on-1.85.jar",
					"590f55ed5d68529239898a4a5c4f730b6e37f45d1cfa3fbe51f8485abe32c42d"),
			library("org/bouncycastle/bcpkix-jdk18on/1.85/bcpkix-jdk18on-1.85.jar",
					"c9f82b2d4e99c4bbdfccf684e52cc06ea06a0b567bfd0d08f9c5a3f417055996"));

	private VelocityRuntimeLibraries() {
	}

	static void ensureAvailable(Path directory, ClassLoader pluginLoader) throws IOException {
		ensureAvailable(directory, pluginLoader, VelocityRuntimeLibraries::download);
	}

	static void ensureAvailable(Path directory, ClassLoader pluginLoader, ArtifactFetcher fetcher) throws IOException {
		if (areLibrariesAvailable(pluginLoader)) {
			return;
		}
		Files.createDirectories(directory);
		for (Library library : LIBRARIES) {
			Path artifact = directory.resolve(library.fileName());
			if (!hasExpectedDigest(artifact, library.sha256())) {
				downloadVerified(library, artifact, fetcher);
			}
			addPath(pluginLoader, artifact);
		}
		if (!areLibrariesAvailable(pluginLoader)) {
			throw new IOException("Bouncy Castle runtime remained incomplete after loading its libraries");
		}
	}

	private static Library library(String path, String sha256) {
		return new Library(path.substring(path.lastIndexOf('/') + 1), URI.create(CENTRAL_MIRROR + path), sha256);
	}

	private static void downloadVerified(Library library, Path artifact, ArtifactFetcher fetcher) throws IOException {
		Path temporary = Files.createTempFile(artifact.getParent(), artifact.getFileName().toString() + ".", ".download");
		setPrivatePermissions(temporary);
		try {
			fetcher.fetch(library.uri(), temporary);
			if (!hasExpectedDigest(temporary, library.sha256())) {
				throw new IOException("Downloaded runtime library failed SHA-256 verification: " + library.fileName());
			}
			try {
				Files.move(temporary, artifact, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException ignored) {
				Files.move(temporary, artifact, StandardCopyOption.REPLACE_EXISTING);
			}
		} finally {
			Files.deleteIfExists(temporary);
		}
	}

	private static void download(URI source, Path target) throws IOException {
		if (!"https".equalsIgnoreCase(source.getScheme())) {
			throw new IOException("Runtime library source must use HTTPS");
		}
		HttpURLConnection connection = (HttpURLConnection) source.toURL().openConnection();
		connection.setConnectTimeout(CONNECT_TIMEOUT_MILLIS);
		connection.setReadTimeout(READ_TIMEOUT_MILLIS);
		connection.setInstanceFollowRedirects(false);
		connection.setRequestProperty("User-Agent", "VotingPlugin-runtime-library-loader");
		try {
			if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
				throw new IOException("Runtime library download returned HTTP " + connection.getResponseCode());
			}
			long declaredLength = connection.getContentLengthLong();
			if (declaredLength > MAX_ARTIFACT_BYTES) {
				throw new IOException("Runtime library exceeds download limit");
			}
			try (InputStream input = connection.getInputStream(); var output = Files.newOutputStream(target)) {
				byte[] buffer = new byte[8192];
				long total = 0;
				int read;
				while ((read = input.read(buffer)) >= 0) {
					total += read;
					if (total > MAX_ARTIFACT_BYTES) {
						throw new IOException("Runtime library exceeds download limit");
					}
					output.write(buffer, 0, read);
				}
			}
		} finally {
			connection.disconnect();
		}
	}

	private static void addPath(ClassLoader loader, Path artifact) throws IOException {
		Method addPath = null;
		for (Class<?> type = loader.getClass(); type != null && addPath == null; type = type.getSuperclass()) {
			try {
				addPath = type.getDeclaredMethod("addPath", Path.class);
			} catch (NoSuchMethodException ignored) {
				// Continue through the class-loader hierarchy.
			}
		}
		if (addPath == null) {
			throw new IOException("Velocity plugin class loader does not expose addPath(Path)");
		}
		try {
			addPath.setAccessible(true);
			addPath.invoke(loader, artifact);
		} catch (IllegalAccessException | InvocationTargetException | RuntimeException failure) {
			throw new IOException("Could not attach Velocity runtime library " + artifact.getFileName(), failure);
		}
	}

	private static boolean areLibrariesAvailable(ClassLoader loader) {
		for (String requiredClass : new String[] {
				"org.bouncycastle.jce.provider.BouncyCastleProvider",
				"org.bouncycastle.asn1.cms.ContentInfo",
				"org.bouncycastle.cert.X509CertificateHolder" }) {
			try {
				Class.forName(requiredClass, false, loader);
			} catch (ClassNotFoundException unavailable) {
				return false;
			}
		}
		return true;
	}

	private static boolean hasExpectedDigest(Path file, String expected) throws IOException {
		if (!Files.isRegularFile(file)) {
			return false;
		}
		try (InputStream input = Files.newInputStream(file)) {
			MessageDigest digest = MessageDigest.getInstance("SHA-256");
			byte[] buffer = new byte[8192];
			int read;
			while ((read = input.read(buffer)) >= 0) {
				digest.update(buffer, 0, read);
			}
			return expected.equals(HexFormat.of().formatHex(digest.digest()));
		} catch (NoSuchAlgorithmException impossible) {
			throw new IllegalStateException("SHA-256 is unavailable", impossible);
		}
	}

	private static void setPrivatePermissions(Path file) {
		try {
			Files.setPosixFilePermissions(file, Set.of(PosixFilePermission.OWNER_READ,
					PosixFilePermission.OWNER_WRITE));
		} catch (IOException | UnsupportedOperationException ignored) {
			// Non-POSIX systems retain their default file permissions.
		}
	}

	@FunctionalInterface
	interface ArtifactFetcher {
		void fetch(URI source, Path target) throws IOException;
	}

	private record Library(String fileName, URI uri, String sha256) {
	}
}
