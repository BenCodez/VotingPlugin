package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.UUID;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class PluginDeploymentServiceTest {
	@TempDir Path directory;

	@Test void backendStagesOnlyAnExactVerifiedVotingPluginJarAndIsIdempotent() throws Exception {
		byte[] artifact = jar("name: VotingPlugin\nmain: example.Main\n");
		PluginDeploymentService service = PluginDeploymentService.backend(directory.resolve("update"));
		PluginDeploymentService.Task task = task(artifact);

		PluginDeploymentService.Result staged = service.stage(task, new ByteArrayInputStream(artifact), () -> true);

		assertTrue(staged.success());
		assertEquals("RESTART_REQUIRED", staged.code());
		assertEquals(artifact.length, Files.size(directory.resolve("update/VotingPlugin.jar")));
		assertTrue(service.stage(task, new ByteArrayInputStream(new byte[0]), () -> true).success());
	}

	@Test void backendIgnoresMatchingMarkerOnlyWhenTargetIsValidThenRestagesWhenCorrupted() throws Exception {
		byte[] artifact = jar("name: VotingPlugin\n");
		PluginDeploymentService service = PluginDeploymentService.backend(directory.resolve("update"));
		PluginDeploymentService.Task task = task(artifact);

		service.stage(task, new ByteArrayInputStream(artifact), () -> true);
		assertArrayEquals(artifact, Files.readAllBytes(directory.resolve("update/VotingPlugin.jar")));

		byte[] corrupted = new byte[artifact.length];
		Arrays.fill(corrupted, (byte) 0x42);
		Files.write(directory.resolve("update/VotingPlugin.jar"), corrupted);
		assertArrayEquals(corrupted, Files.readAllBytes(directory.resolve("update/VotingPlugin.jar")));

		assertTrue(service.stage(task, new ByteArrayInputStream(artifact), () -> true).success());
		assertArrayEquals(artifact, Files.readAllBytes(directory.resolve("update/VotingPlugin.jar")));
	}

	@Test void invalidPluginIdentityAndDigestNeverReachTheUpdateFolder() throws Exception {
		byte[] wrongPlugin = jar("name: NotVotingPlugin\n");
		PluginDeploymentService service = PluginDeploymentService.backend(directory.resolve("update"));
		PluginDeploymentService.Result invalid = service.stage(task(wrongPlugin), new ByteArrayInputStream(wrongPlugin), () -> true);
		assertFalse(invalid.success());
		assertEquals("INVALID_ARTIFACT", invalid.code());
		assertFalse(Files.exists(directory.resolve("update/VotingPlugin.jar")));

		byte[] good = jar("name: VotingPlugin\n");
		PluginDeploymentService.Task mismatch = new PluginDeploymentService.Task(UUID.randomUUID(), "vp.jar",
				"0".repeat(64), good.length, UUID.randomUUID());
		assertEquals("HASH_MISMATCH", service.stage(mismatch, new ByteArrayInputStream(good), () -> true).code());
	}

	@Test void proxyKeepsTheCurrentJarAsABackupBeforeReplacement() throws Exception {
		Path current = directory.resolve("VotingPlugin.jar");
		byte[] original = jar("name: VotingPlugin\nversion: 0\n");
		byte[] first = jar("name: VotingPlugin\nversion: 1\n");
		byte[] second = jar("name: VotingPlugin\nversion: 2\n");
		Files.write(current, original);
		PluginDeploymentService service = PluginDeploymentService.proxy(current);

		assertEquals("RESTART_REQUIRED", service.stage(task(first), new ByteArrayInputStream(first), () -> true).code());
		assertEquals("RESTART_REQUIRED", service.stage(task(second), new ByteArrayInputStream(second), () -> true).code());
		assertArrayEquals(first, Files.readAllBytes(directory.resolve("VotingPlugin.jar.control-backup")));
		assertArrayEquals(second, Files.readAllBytes(current));
	}

	@Test void cancellationDuringCopyDoesNotPublish() throws Exception {
		byte[] artifact = jar("name: VotingPlugin\n");
		PluginDeploymentService service = PluginDeploymentService.backend(directory.resolve("update"));
		PluginDeploymentService.Task task = task(artifact);

		assertEquals("RESTART_REQUIRED", service.stage(task, new ByteArrayInputStream(artifact), () -> true).code());
		String markerBefore = Files.readString(directory.resolve("update/VotingPlugin.jar.control-deployment"));
		byte[] targetBefore = Files.readAllBytes(directory.resolve("update/VotingPlugin.jar"));

		AtomicBoolean active = new AtomicBoolean(true);
		InputStream body = new CancellingInputStream(new ByteArrayInputStream(artifact), active);
		PluginDeploymentService.Task replacement = task(artifact);
		assertEquals("CANCELLED", service.stage(replacement, body, active::get).code());

		assertEquals(markerBefore, Files.readString(directory.resolve("update/VotingPlugin.jar.control-deployment")));
		assertArrayEquals(targetBefore, Files.readAllBytes(directory.resolve("update/VotingPlugin.jar")));
		assertFalse(active.get());
	}

	private static PluginDeploymentService.Task task(byte[] artifact) throws Exception {
		String digest = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(artifact));
		return new PluginDeploymentService.Task(UUID.randomUUID(), "votingplugin.jar", digest, artifact.length,
				UUID.randomUUID());
	}

	private static byte[] jar(String pluginYml) throws Exception {
		ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		try (JarOutputStream jar = new JarOutputStream(bytes)) {
			jar.putNextEntry(new JarEntry("plugin.yml"));
			jar.write(pluginYml.getBytes(StandardCharsets.UTF_8));
			jar.closeEntry();
			jar.putNextEntry(new JarEntry("example/Main.class"));
			jar.write(new byte[] { 0, 1, 2 });
			jar.closeEntry();
		}
		return bytes.toByteArray();
	}

	private static class CancellingInputStream extends InputStream {
		private final InputStream delegate;
		private final AtomicBoolean active;
		private boolean cancelled;

		private CancellingInputStream(InputStream delegate, AtomicBoolean active) {
			this.delegate = delegate;
			this.active = active;
		}

		@Override
		public int read(byte[] bytes, int start, int length) throws IOException {
			int read = delegate.read(bytes, start, length);
			if (!cancelled && read > 0) {
				cancelled = true;
				active.set(false);
			}
			return read;
		}

		@Override
		public int read() throws IOException { return delegate.read(); }
	}
}
