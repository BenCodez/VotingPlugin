package com.bencodez.votingplugin.util;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.sqlite.util.OSInfo;

/** Verifies the embedded Xerial SQLite native, with a pinned-driver fallback for custom packaging. */
public final class SqliteNativeLibrary {
	static final String DRIVER_FILE = "sqlite-jdbc-3.54.1.0.jar";
	static final String DRIVER_SHA256 = "f8dce420a6016278497f4f0560f65602b9db858de141b541cf91a4644ebe4639";
	private static final URI DRIVER_URI = URI.create("https://maven-central.storage-download.googleapis.com/maven2/"
			+ "org/xerial/sqlite-jdbc/3.54.1.0/" + DRIVER_FILE);
	private static final long MAX_DRIVER_BYTES = 16L * 1024L * 1024L;
	private static final int MAX_NATIVE_BYTES = 2 * 1024 * 1024;
	// Digests from the official artifact identified by DRIVER_SHA256. Never trust a resource
	// supplied by a different driver or classloader merely because its path matches.
	private static final Map<String, String> NATIVE_SHA256 = Map.ofEntries(
			Map.entry("org/sqlite/native/FreeBSD/aarch64/libsqlitejdbc.so",
					"d450f90bfaf858c2e52505b4370c99ede31c07430c66c3231132a6920b26ed5c"),
			Map.entry("org/sqlite/native/FreeBSD/x86/libsqlitejdbc.so",
					"61f86fe5144d8965a3e64bfa48af874f6901167a8b3e4e6a370500640ac8ac1b"),
			Map.entry("org/sqlite/native/FreeBSD/x86_64/libsqlitejdbc.so",
					"04584ec9a87bc2d59e707949f2c9db3e206108dd8fce1cf1797f13d7e0760f30"),
			Map.entry("org/sqlite/native/Linux/aarch64/libsqlitejdbc.so",
					"2deb508458a39049204b0e92db5d82e53598eb4e2d34dff3ee29cc47a38f83ee"),
			Map.entry("org/sqlite/native/Linux/arm/libsqlitejdbc.so",
					"9b50eb59ae3a2d2a956287c53b3c6cf87cd5ffff58c00889fbd5a7a9ef220482"),
			Map.entry("org/sqlite/native/Linux/armv6/libsqlitejdbc.so",
					"3601ac7a21215b9dc928db447a63a28383dad419e579d77168ab6c91acbbbf3b"),
			Map.entry("org/sqlite/native/Linux/armv7/libsqlitejdbc.so",
					"43f0c78a6de21ce39539a0df93edf19bcef79c95358467128459d551666f098c"),
			Map.entry("org/sqlite/native/Linux/ppc64/libsqlitejdbc.so",
					"1a2f43080088c6230aa1554e1fc198e1961f8ef8f5df94656a612fee97ada41a"),
			Map.entry("org/sqlite/native/Linux/riscv64/libsqlitejdbc.so",
					"cbfb0bed9e5feca3dcec0fcd95d9e9c21f90382adc62be64fcd3300772d7c74d"),
			Map.entry("org/sqlite/native/Linux/x86/libsqlitejdbc.so",
					"7440830f45d7b2e32f16c27cc6d61e65070f9a7ea6ceff731a141943faf97512"),
			Map.entry("org/sqlite/native/Linux/x86_64/libsqlitejdbc.so",
					"9149f234a03da04caba72ce534dfa0f0e5b999ae8d03d630034be65f24c7b44c"),
			Map.entry("org/sqlite/native/Linux-Musl/aarch64/libsqlitejdbc.so",
					"126521645e72d05b57442583ab33b2c8494fd04d60d1be442a15536723e26861"),
			Map.entry("org/sqlite/native/Linux-Musl/x86/libsqlitejdbc.so",
					"cf9e993ea2a052f8ec3931a202f4fea0049e7f0276b3726c1ac87d52064c45c0"),
			Map.entry("org/sqlite/native/Linux-Musl/x86_64/libsqlitejdbc.so",
					"b21aac0da3a80a0c4cee035d5919021c991ee17c8284a76b38ab7b62e68bab52"),
			Map.entry("org/sqlite/native/Mac/aarch64/libsqlitejdbc.dylib",
					"47fa68b43f34552fbfedeed25e82875a42cfd53e81c33c7cee20ac3f729c060e"),
			Map.entry("org/sqlite/native/Mac/x86_64/libsqlitejdbc.dylib",
					"51d30fe476f0170cd42102baabb82c4b39d43996c9209cdc60876a4b928d85f7"),
			Map.entry("org/sqlite/native/Windows/aarch64/sqlitejdbc.dll",
					"59ec28822968199a7a94feb62a44357ab201c7bc3c55ebdf0e3e5ece840a45b6"),
			Map.entry("org/sqlite/native/Windows/armv7/sqlitejdbc.dll",
					"175095c594d0fa73e819d7b1066a4d8a19265d89bbe4bf0c629a29dc233b6e04"),
			Map.entry("org/sqlite/native/Windows/x86/sqlitejdbc.dll",
					"d03f852a3350e65f6ed8244e6f348d154aca10c89820e1387885dd95b4fa0445"),
			Map.entry("org/sqlite/native/Windows/x86_64/sqlitejdbc.dll",
					"4896f8ba8824a85f10f31383e38b9542b6043886a0c834e0d6c8606b02f82308"));
	private static final long STALE_NATIVE_MILLIS = 24L * 60L * 60L * 1000L;

	private SqliteNativeLibrary() {
	}

	/** Ensures Xerial can load the native for this operating system and architecture. */
	public static synchronized void ensureAvailable(Path directory) throws IOException {
		String configuredName = System.getProperty("org.sqlite.lib.name");
		// A custom name alone selects a library through Xerial/java.library.path.
		// The standard name still uses our verified extraction, including on noexec temp mounts.
		if (System.getProperty("org.sqlite.lib.path") != null
				|| (configuredName != null && !configuredName.equals(System.mapLibraryName("sqlitejdbc")))) {
			try {
				if (!org.sqlite.core.NativeDB.load()) throw new IOException("Configured SQLite native library did not load");
				return;
			} catch (Exception failure) {
				throw failure instanceof IOException io ? io
						: new IOException("Could not load the configured SQLite native library", failure);
			}
		}
		String folder = OSInfo.getNativeLibFolderPathForCurrentOS();
		Path nativeLibrary = prepareNative(directory, folder, SqliteNativeLibrary.class.getClassLoader(),
				SqliteNativeLibrary::download);
		if (nativeLibrary != null) loadPreparedNative(nativeLibrary.getParent(), nativeLibrary.getFileName().toString());
	}

	static Path prepareNative(Path directory, String folder, ClassLoader resourceLoader, ArtifactFetcher fetcher)
			throws IOException {
		String libraryName = nativeLibraryName(folder);
		String resource = "org/sqlite/native/" + folder + "/" + libraryName;
		byte[] embeddedBytes = null;
		try (InputStream embedded = resourceLoader.getResourceAsStream(resource)) {
			if (embedded != null) {
				String expected = NATIVE_SHA256.get(resource);
				byte[] bytes = embedded.readNBytes(MAX_NATIVE_BYTES + 1);
				if (expected == null || bytes.length == 0 || bytes.length > MAX_NATIVE_BYTES
						|| !expected.equals(sha256(bytes))) {
					throw new IOException("Embedded SQLite native failed SHA-256 verification: " + resource);
				}
				embeddedBytes = bytes;
			}
		}
		Files.createDirectories(directory);
		Path driver = directory.resolve(DRIVER_FILE);
		if (embeddedBytes == null && !hasExpectedDigest(driver, DRIVER_SHA256)) {
			try {
				downloadVerified(driver, fetcher);
			} catch (IOException failure) {
				throw new IOException("Unable to obtain the verified SQLite driver; pre-provision " + DRIVER_FILE
						+ " in the VotingPlugin libraries directory or configure org.sqlite.lib.path", failure);
			}
		}
		Path platformDirectory = directory.resolve("sqlite-native").resolve(folder);
		Files.createDirectories(platformDirectory);
		cleanupStaleNativeCopies(platformDirectory);
		Path nativeDirectory = platformDirectory.resolve("load-" + UUID.randomUUID());
		Files.createDirectories(nativeDirectory);
		Path nativeLibrary = nativeDirectory.resolve(libraryName);
		if (embeddedBytes != null) {
			// Keep loading in the plugin directory, including when the JVM temp directory
			// is read-only or mounted noexec on a previously fallback-backed platform.
			Path temporary = Files.createTempFile(nativeDirectory, libraryName + ".", ".extract");
			setPrivatePermissions(temporary);
			try {
				Files.write(temporary, embeddedBytes);
				moveReplacing(temporary, nativeLibrary);
			} finally { Files.deleteIfExists(temporary); }
		} else {
			extractVerifiedEntry(driver, resource, nativeLibrary);
		}
		nativeLibrary.toFile().deleteOnExit();
		nativeDirectory.toFile().deleteOnExit();
		return nativeLibrary;
	}

	private static void cleanupStaleNativeCopies(Path platformDirectory) {
		try (var loads = Files.newDirectoryStream(platformDirectory, "load-*")) {
			for (Path load : loads) {
				if (!Files.isDirectory(load, LinkOption.NOFOLLOW_LINKS)) continue;
				if (Files.getLastModifiedTime(load, LinkOption.NOFOLLOW_LINKS).toMillis()
						> System.currentTimeMillis() - STALE_NATIVE_MILLIS) continue;
				try (var files = Files.newDirectoryStream(load)) {
					for (Path file : files) {
						if (Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) Files.deleteIfExists(file);
					}
				}
				Files.deleteIfExists(load);
			}
		} catch (IOException | SecurityException ignored) {
			// A prior classloader may still own the native, especially on Windows.
		}
	}

	private static void loadPreparedNative(Path nativeDirectory, String libraryName) throws IOException {
		synchronized (System.getProperties()) {
			String oldPath = System.getProperty("org.sqlite.lib.path");
			String oldName = System.getProperty("org.sqlite.lib.name");
			try {
				System.setProperty("org.sqlite.lib.path", nativeDirectory.toAbsolutePath().normalize().toString());
				System.setProperty("org.sqlite.lib.name", libraryName);
				if (!org.sqlite.core.NativeDB.load()) throw new IOException("SQLite native library did not load");
			} catch (Exception failure) {
				throw failure instanceof IOException io ? io : new IOException("Could not load SQLite native library", failure);
			} finally {
				restoreProperty("org.sqlite.lib.path", oldPath);
				restoreProperty("org.sqlite.lib.name", oldName);
			}
		}
	}

	private static void restoreProperty(String name, String value) {
		if (value == null) System.clearProperty(name);
		else System.setProperty(name, value);
	}

	private static String nativeLibraryName(String folder) throws IOException {
		if (folder.startsWith("Windows/")) return "sqlitejdbc.dll";
		if (folder.startsWith("Mac/")) return "libsqlitejdbc.dylib";
		if (folder.startsWith("Linux/") || folder.startsWith("Linux-Musl/")
				|| folder.startsWith("FreeBSD/")) return "libsqlitejdbc.so";
		throw new IOException("SQLite does not publish a native library for " + folder);
	}

	private static void downloadVerified(Path target, ArtifactFetcher fetcher) throws IOException {
		Path temporary = Files.createTempFile(target.getParent(), DRIVER_FILE + ".", ".download");
		setPrivatePermissions(temporary);
		try {
			fetcher.fetch(DRIVER_URI, temporary);
			if (!hasExpectedDigest(temporary, DRIVER_SHA256))
				throw new IOException("Downloaded SQLite driver failed SHA-256 verification");
			moveReplacing(temporary, target);
		} finally { Files.deleteIfExists(temporary); }
	}

	private static void download(URI source, Path target) throws IOException {
		if (!"https".equalsIgnoreCase(source.getScheme())) throw new IOException("SQLite driver source must use HTTPS");
		HttpURLConnection connection = (HttpURLConnection) source.toURL().openConnection();
		connection.setConnectTimeout(10_000);
		connection.setReadTimeout(30_000);
		connection.setInstanceFollowRedirects(false);
		connection.setRequestProperty("User-Agent", "VotingPlugin-sqlite-native-loader");
		try {
			if (connection.getResponseCode() != HttpURLConnection.HTTP_OK)
				throw new IOException("SQLite driver download returned HTTP " + connection.getResponseCode());
			long declaredLength = connection.getContentLengthLong();
			if (declaredLength > MAX_DRIVER_BYTES) throw new IOException("SQLite driver exceeds download limit");
			try (InputStream input = connection.getInputStream(); var output = Files.newOutputStream(target)) {
				byte[] buffer = new byte[8192];
				long total = 0;
				int read;
				while ((read = input.read(buffer)) >= 0) {
					total += read;
					if (total > MAX_DRIVER_BYTES) throw new IOException("SQLite driver exceeds download limit");
					output.write(buffer, 0, read);
				}
			}
		} finally { connection.disconnect(); }
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

	private static String sha256(byte[] bytes) {
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
		} catch (NoSuchAlgorithmException impossible) {
			throw new IllegalStateException("SHA-256 is unavailable", impossible);
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
	@FunctionalInterface
	interface ArtifactFetcher {
		void fetch(URI source, Path target) throws IOException;
	}

}
