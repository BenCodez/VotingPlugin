package com.bencodez.votingplugin.proxy.velocity;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class VelocityRuntimeLibrariesTest {
	@Test
	void rejectsAnUnverifiedDownload(@TempDir Path directory) throws Exception {
		try (TestPluginClassLoader loader = new TestPluginClassLoader()) {
			assertThrows(IOException.class, () -> VelocityRuntimeLibraries.ensureAvailable(directory, loader,
					(source, target) -> Files.writeString(target, "unverified")));
		}
		assertFalse(Files.exists(directory.resolve("bcprov-jdk18on-1.85.jar")));
	}

	@Test
	void installsVerifiedLibrariesAndReplacesCorruptCache(@TempDir Path directory) throws Exception {
		Map<String, Path> dependencies = Map.of(
				"bcprov-jdk18on-1.85.jar", dependency("bcprov-jdk18on-1.85.jar"),
				"bcutil-jdk18on-1.85.jar", dependency("bcutil-jdk18on-1.85.jar"),
				"bcpkix-jdk18on-1.85.jar", dependency("bcpkix-jdk18on-1.85.jar"));
		Path libraries = directory.resolve("libraries");
		Files.createDirectories(libraries);
		Files.writeString(libraries.resolve("bcprov-jdk18on-1.85.jar"), "corrupt");

		// A parent or another plugin may expose only bcprov. That partial state must
		// not suppress loading bcutil and bcpkix.
		try (TestPluginClassLoader loader = new TestPluginClassLoader(
				dependencies.get("bcprov-jdk18on-1.85.jar").toUri().toURL())) {
			VelocityRuntimeLibraries.ensureAvailable(libraries, loader, (source, target) -> {
				Path dependency = dependencies.get(Path.of(source.getPath()).getFileName().toString());
				if (dependency == null) {
					throw new IOException("Unexpected dependency " + source);
				}
				Files.copy(dependency, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
			});

			assertTrue(Files.size(libraries.resolve("bcprov-jdk18on-1.85.jar")) > 1_000_000L);
			assertTrue(Class.forName("org.bouncycastle.jce.provider.BouncyCastleProvider", true, loader)
					.getConstructor().newInstance() instanceof java.security.Provider);
			assertTrue(Class.forName("org.bouncycastle.asn1.cms.ContentInfo", false, loader) != null);
			assertTrue(Class.forName("org.bouncycastle.cert.X509CertificateHolder", false, loader) != null);
		}
		try (var files = Files.list(libraries)) {
			assertFalse(files.anyMatch(path -> path.getFileName().toString().endsWith(".download")));
		}
	}

	private static Path dependency(String fileName) throws IOException {
		for (String entry : System.getProperty("java.class.path").split(java.io.File.pathSeparator)) {
			Path candidate = Path.of(entry);
			if (candidate.getFileName() != null && fileName.equals(candidate.getFileName().toString())) {
				return candidate;
			}
		}
		throw new IOException("Dependency was not present on the test classpath: " + fileName);
	}

	private static final class TestPluginClassLoader extends URLClassLoader {
		private TestPluginClassLoader(URL... initial) {
			super(initial, ClassLoader.getPlatformClassLoader());
		}

		@SuppressWarnings("unused")
		void addPath(Path path) throws IOException {
			addURL(path.toUri().toURL());
		}
	}
}
