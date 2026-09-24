package com.bencodez.votingplugin.util;

import java.io.IOException;
import java.io.InputStream;
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
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.sqlite.util.OSInfo;

/** Prepares the current Xerial SQLite native without embedding every target in the plugin JAR. */
public final class SqliteNativeLibrary {
	static final String DRIVER_FILE = "sqlite-jdbc-3.53.4.0.jar";
	static final String DRIVER_SHA256 = "bcb1f51e36f940867e83342f9efbf5968ac44a6bef4d397bb4af7b17b45cd2fb";
	private static final URI DRIVER_URI = URI.create("https://maven-central.storage-download.googleapis.com/maven2/"
			+ "org/xerial/sqlite-jdbc/3.53.4.0/" + DRIVER_FILE);
	private static final long MAX_DRIVER_BYTES = 16L * 1024L * 1024L;

	private SqliteNativeLibrary() {
	}

	/** Ensures Xerial can load the native for this operating system and architecture. */
	public static synchronized void ensureAvailable(Path directory) throws IOException {
		if (System.getProperty("org.sqlite.lib.path") != null) {
			return;
		}
		String folder = OSInfo.getNativeLibFolderPathForCurrentOS();
		String libraryName = nativeLibraryName(folder);
		String resource = "org/sqlite/native/" + folder + "/" + libraryName;
		if (SqliteNativeLibrary.class.getClassLoader().getResource(resource) != null) {
			return;
		}

		Files.createDirectories(directory);
		Path driver = directory.resolve(DRIVER_FILE);
		if (!hasExpectedDigest(driver, DRIVER_SHA256)) {
			downloadVerified(driver);
		}
		Path nativeDirectory = directory.resolve("sqlite-native").resolve(folder);
		Files.createDirectories(nativeDirectory);
		Path nativeLibrary = nativeDirectory.resolve(libraryName);
		extractVerifiedEntry(driver, resource, nativeLibrary);
		System.setProperty("org.sqlite.lib.path", nativeDirectory.toAbsolutePath().normalize().toString());
		System.setProperty("org.sqlite.lib.name", libraryName);
	}

	private static String nativeLibraryName(String folder) throws IOException {
		if (folder.startsWith("Windows/")) return "sqlitejdbc.dll";
		if (folder.startsWith("Mac/")) return "libsqlitejdbc.dylib";
		if (folder.startsWith("Linux/") || folder.startsWith("Linux-Musl/")
				|| folder.startsWith("FreeBSD/")) return "libsqlitejdbc.so";
		throw new IOException("SQLite does not publish a native library for " + folder);
	}

	private static void downloadVerified(Path target) throws IOException {
		Path temporary = Files.createTempFile(target.getParent(), DRIVER_FILE + ".", ".download");
		setPrivatePermissions(temporary);
		HttpURLConnection connection = (HttpURLConnection) DRIVER_URI.toURL().openConnection();
		connection.setConnectTimeout(10_000);
		connection.setReadTimeout(30_000);
		connection.setInstanceFollowRedirects(false);
		connection.setRequestProperty("User-Agent", "VotingPlugin-sqlite-native-loader");
		try {
			if (connection.getResponseCode() != HttpURLConnection.HTTP_OK) {
				throw new IOException("SQLite driver download returned HTTP " + connection.getResponseCode());
			}
			long declaredLength = connection.getContentLengthLong();
			if (declaredLength > MAX_DRIVER_BYTES) throw new IOException("SQLite driver exceeds download limit");
			try (InputStream input = connection.getInputStream(); var output = Files.newOutputStream(temporary)) {
				byte[] buffer = new byte[8192];
				long total = 0;
				int read;
				while ((read = input.read(buffer)) >= 0) {
					total += read;
					if (total > MAX_DRIVER_BYTES) throw new IOException("SQLite driver exceeds download limit");
					output.write(buffer, 0, read);
				}
			}
			if (!hasExpectedDigest(temporary, DRIVER_SHA256)) {
				throw new IOException("Downloaded SQLite driver failed SHA-256 verification");
			}
			moveReplacing(temporary, target);
		} finally {
			connection.disconnect();
			Files.deleteIfExists(temporary);
		}
	}

	private static void extractVerifiedEntry(Path driver, String resource, Path target) throws IOException {
		try (JarFile jar = new JarFile(driver.toFile())) {
			JarEntry entry = jar.getJarEntry(resource);
			if (entry == null || entry.isDirectory() || entry.getSize() <= 0 || entry.getSize() > 2L * 1024L * 1024L) {
				throw new IOException("SQLite driver does not contain the expected native: " + resource);
			}
			Path temporary = Files.createTempFile(target.getParent(), target.getFileName().toString() + ".", ".extract");
			setPrivatePermissions(temporary);
			try {
				try (InputStream input = jar.getInputStream(entry)) {
					Files.copy(input, temporary, StandardCopyOption.REPLACE_EXISTING);
				}
				moveReplacing(temporary, target);
			} finally {
				Files.deleteIfExists(temporary);
			}
		}
	}

	private static void moveReplacing(Path source, Path target) throws IOException {
		try {
			Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
		} catch (AtomicMoveNotSupportedException ignored) {
			Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
		}
	}

	private static boolean hasExpectedDigest(Path file, String expected) throws IOException {
		if (!Files.isRegularFile(file)) return false;
		try (InputStream input = Files.newInputStream(file)) {
			MessageDigest digest = MessageDigest.getInstance("SHA-256");
			byte[] buffer = new byte[8192];
			int read;
			while ((read = input.read(buffer)) >= 0) digest.update(buffer, 0, read);
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
}
