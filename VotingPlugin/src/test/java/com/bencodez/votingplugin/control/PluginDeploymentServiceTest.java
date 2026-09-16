package com.bencodez.votingplugin.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
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

	@Test void proxyRecoversActivationInterruptedBeforeMarkerWithoutReplacingBackup() throws Exception {
		Path current = directory.resolve("VotingPlugin.jar");
		Path backup = directory.resolve("VotingPlugin.jar.control-backup");
		Path marker = directory.resolve("VotingPlugin.jar.control-deployment");
		byte[] original = jar("name: VotingPlugin\nversion: original\n");
		byte[] candidate = jar("name: VotingPlugin\nversion: candidate\n");
		PluginDeploymentService.Task task = task(candidate);
		Files.write(current, candidate);
		Files.write(backup, original);
		PluginDeploymentService service = PluginDeploymentService.proxy(current);

		assertEquals("RESTART_REQUIRED",
				service.stage(task, new ByteArrayInputStream(new byte[0]), () -> true).code());

		assertArrayEquals(candidate, Files.readAllBytes(current));
		assertArrayEquals(original, Files.readAllBytes(backup));
		String state = Files.readString(marker, StandardCharsets.US_ASCII);
		assertTrue(state.contains(task.deploymentId().toString()));
		assertTrue(state.contains(task.sha256()));
		assertTrue(state.contains(Long.toString(task.size())));
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

	@Test void markerPublicationFailureRestoresThePreviousArtifact() throws Exception {
		Path update = directory.resolve("update");
		Files.createDirectories(update);
		byte[] original = jar("name: VotingPlugin\nversion: original\n");
		byte[] candidate = jar("name: VotingPlugin\nversion: candidate\n");
		Path target = update.resolve("VotingPlugin.jar");
		Files.write(target, original);
		Path marker = update.resolve("VotingPlugin.jar.control-deployment");
		Files.createDirectory(marker);
		Files.writeString(marker.resolve("keep"), "marker publication must fail");
		PluginDeploymentService service = PluginDeploymentService.backend(update);

		assertThrows(IOException.class, () -> service.stage(task(candidate), new ByteArrayInputStream(candidate), () -> true));

		assertArrayEquals(original, Files.readAllBytes(target));
	}

	@Test void proxyMarkerPublicationFailureRestoresTheCurrentJarAndKeepsItsBackup() throws Exception {
		byte[] original = jar("name: VotingPlugin\nversion: original\n");
		byte[] candidate = jar("name: VotingPlugin\nversion: candidate\n");
		Path target = directory.resolve("VotingPlugin.jar");
		Files.write(target, original);
		Path marker = directory.resolve("VotingPlugin.jar.control-deployment");
		Files.createDirectory(marker);
		Files.writeString(marker.resolve("keep"), "marker publication must fail");
		PluginDeploymentService service = PluginDeploymentService.proxy(target);

		assertThrows(IOException.class, () -> service.stage(task(candidate), new ByteArrayInputStream(candidate), () -> true));

		assertArrayEquals(original, Files.readAllBytes(target));
		assertArrayEquals(original, Files.readAllBytes(directory.resolve("VotingPlugin.jar.control-backup")));
	}

	@Test void cancellationClosesAStalledArtifactStream() throws Exception {
		PluginDeploymentService service = PluginDeploymentService.backend(directory.resolve("update"));
		AtomicBoolean active = new AtomicBoolean(true);
		BlockingInputStream stalled = new BlockingInputStream(active);
		ExecutorService executor = Executors.newSingleThreadExecutor();
		try {
			Future<PluginDeploymentService.Result> result = executor.submit(() -> service.stage(
					task(new byte[] { 1 }), stalled, active::get));
			assertTrue(stalled.awaitRead());
			service.cancel();
			assertEquals("CANCELLED", result.get(5, TimeUnit.SECONDS).code());
			assertFalse(Files.exists(directory.resolve("update/VotingPlugin.jar")));
		} finally {
			executor.shutdownNow();
		}
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

	private static class BlockingInputStream extends InputStream {
		private final AtomicBoolean active;
		private final CountDownLatch reading = new CountDownLatch(1);
		private boolean closed;

		private BlockingInputStream(AtomicBoolean active) { this.active = active; }

		private boolean awaitRead() throws InterruptedException { return reading.await(5, TimeUnit.SECONDS); }

		@Override
		public synchronized int read(byte[] bytes, int start, int length) throws IOException {
			reading.countDown();
			while (!closed) {
				try { wait(); }
				catch (InterruptedException failure) { Thread.currentThread().interrupt(); throw new IOException(failure); }
			}
			throw new IOException("stream closed");
		}

		@Override
		public int read() throws IOException { return read(new byte[1], 0, 1); }

		@Override
		public synchronized void close() {
			closed = true;
			active.set(false);
			notifyAll();
		}
	}
}
