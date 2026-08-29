package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.sun.net.httpserver.HttpServer;

class HostedControlManagerTest {
	@TempDir
	Path directory;

	@Test
	void manualPinsAndContainedPathsAreValidatedWhileBlankSelectsLatest() throws Exception {
		Path root = directory.toAbsolutePath().normalize();
		assertThrows(IllegalArgumentException.class,
				() -> HostedControlManager.resolveInside(root, "../outside.jar", "jar"));
		assertThrows(IllegalArgumentException.class,
				() -> HostedControlManager.parseDownloadUri(
						"https://github.com/BenCodez/VotingPlugin-Control/releases/latest/download/control.jar"));
		assertThrows(IllegalArgumentException.class, () -> settings(root.resolve("control.jar"), root.resolve("data"),
				true, false, "not-a-digest", 30));
		HostedControlManager.Settings latest = latestSettings(root.resolve("control.jar"), root.resolve("data"), true);
		assertTrue(latest.usesLatestRelease());
		assertThrows(IllegalArgumentException.class, () -> new HostedControlManager.Settings(root,
				root.resolve("control.jar"), root.resolve("data"), true, true,
				URI.create("https://github.com/BenCodez/VotingPlugin-Control/releases/download/v0.1.3/votingplugin-control.jar"),
				"", "127.0.0.1", 8080, 30, 60));
	}

	@Test
	void githubLatestMetadataRequiresStableOfficialAssetAndDigest() throws Exception {
		String sha256 = "a".repeat(64);
		String valid = """
				{"tag_name":"v0.1.3","draft":false,"prerelease":false,"assets":[
				{"name":"votingplugin-control.jar","digest":"sha256:%s","browser_download_url":
				"https://github.com/BenCodez/VotingPlugin-Control/releases/download/v0.1.3/votingplugin-control.jar"}]}
				""".formatted(sha256);
		HostedControlManager.ArtifactSpec artifact = HostedControlManager.JdkGithubReleaseResolver
				.parseLatestRelease(valid.getBytes(StandardCharsets.UTF_8));
		assertEquals("v0.1.3", artifact.version());
		assertEquals(sha256, artifact.sha256());

		String wrongRepository = valid.replace("BenCodez/VotingPlugin-Control", "someone/other");
		assertThrows(IOException.class, () -> HostedControlManager.JdkGithubReleaseResolver
				.parseLatestRelease(wrongRepository.getBytes(StandardCharsets.UTF_8)));
		String prerelease = valid.replace("\"prerelease\":false", "\"prerelease\":true");
		assertThrows(IOException.class, () -> HostedControlManager.JdkGithubReleaseResolver
				.parseLatestRelease(prerelease.getBytes(StandardCharsets.UTF_8)));
	}

	@Test
	void platformNeutralFactoryKeepsHostingOptIn() {
		HostedControlManager.HostConfiguration disabled = new HostedControlManager.HostConfiguration(false, true,
				false, "", "", "control/control.jar", "control/data", "127.0.0.1", 8080, 30, 60);
		assertNull(HostedControlManager.create(directory, disabled, message -> { }));
		HostedControlManager.HostConfiguration escaping = new HostedControlManager.HostConfiguration(true, false,
				false, "", "0".repeat(64), "../control.jar", "control/data", "127.0.0.1", 8080, 30, 60);
		assertThrows(IllegalArgumentException.class,
				() -> HostedControlManager.create(directory, escaping, message -> { }));
		HostedControlManager.HostConfiguration invalidHost = new HostedControlManager.HostConfiguration(true, false,
				false, "", "0".repeat(64), "control/control.jar", "control/data", "[", 8080, 30, 60);
		assertThrows(IllegalArgumentException.class,
				() -> HostedControlManager.create(directory, invalidHost, message -> { }));
	}

	@Test
	void localEndpointMatchesLoopbackOrTheConfiguredListenerAddress() {
		HostedControlManager.HostConfiguration hosted = new HostedControlManager.HostConfiguration(true, false,
				false, "", "0".repeat(64), "control/control.jar", "control/data", "10.0.0.5", 2150, 30, 60);

		assertFalse(HostedControlManager.isDirectLocalEndpoint("http://127.0.0.1:2150", hosted));
		assertTrue(HostedControlManager.isDirectLocalEndpoint("http://10.0.0.5:2150", hosted));
		assertFalse(HostedControlManager.isDirectLocalEndpoint("http://10.0.0.6:2150", hosted));
		assertFalse(HostedControlManager.isDirectLocalEndpoint("https://10.0.0.5:2150", hosted));
		HostedControlManager.HostConfiguration ipv6 = new HostedControlManager.HostConfiguration(true, false,
				false, "", "0".repeat(64), "control/control.jar", "control/data", "::1", 2150, 30, 60);
		assertTrue(HostedControlManager.isDirectLocalEndpoint("http://[::1]:2150", ipv6));
		HostedControlManager.HostConfiguration wildcard = new HostedControlManager.HostConfiguration(true, false,
				false, "", "0".repeat(64), "control/control.jar", "control/data", "0.0.0.0", 2150, 30, 60);
		assertTrue(HostedControlManager.isDirectLocalEndpoint("http://127.0.0.1:2150", wildcard));
	}

	@Test
	void hostedArtifactBodyReadCannotOutliveDownloadTimeout() throws Exception {
		CountDownLatch bodyStarted = new CountDownLatch(1);
		CountDownLatch releaseBody = new CountDownLatch(1);
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/control.jar", exchange -> {
			exchange.sendResponseHeaders(200, 0);
			try (var body = exchange.getResponseBody()) {
				body.write(1);
				body.flush();
				bodyStarted.countDown();
				try {
					releaseBody.await();
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		});
		server.start();
		try {
			Path target = directory.resolve("stalled.jar");
			URI source = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/control.jar");
			long started = System.nanoTime();
			IOException failure = assertThrows(IOException.class, () -> new HostedControlManager.JdkArtifactDownloader()
					.download(source, target, 1024, Duration.ofMillis(250)));
			assertTrue(bodyStarted.await(2, TimeUnit.SECONDS));
			assertTrue(failure.getMessage().contains("timed out"));
			assertTrue(Duration.ofNanos(System.nanoTime() - started).compareTo(Duration.ofSeconds(2)) < 0);
		} finally {
			releaseBody.countDown();
			server.stop(0);
		}
	}

	@Test
	void hostedHealthBodyReadCannotOutliveProbeTimeout() throws Exception {
		CountDownLatch bodyStarted = new CountDownLatch(1);
		CountDownLatch releaseBody = new CountDownLatch(1);
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/api/v1/health", exchange -> {
			exchange.sendResponseHeaders(200, 0);
			try (var body = exchange.getResponseBody()) {
				body.write("{\"status\":\"ok\"".getBytes(StandardCharsets.UTF_8));
				body.flush();
				bodyStarted.countDown();
				try {
					releaseBody.await();
				} catch (InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		});
		server.start();
		try {
			URI endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/");
			long started = System.nanoTime();
			assertFalse(new HostedControlManager.JdkHealthProbe().isHealthy(endpoint, Duration.ofMillis(250),
					"00000000-0000-0000-0000-000000000001"));
			assertTrue(bodyStarted.await(2, TimeUnit.SECONDS));
			assertTrue(Duration.ofNanos(System.nanoTime() - started).compareTo(Duration.ofSeconds(2)) < 0);
		} finally {
			releaseBody.countDown();
			server.stop(0);
		}
	}

	@Test
	void hostedHealthRequiresTheLaunchedChildIdentity() throws Exception {
		String launchId = "00000000-0000-0000-0000-000000000001";
		HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/api/v1/health", exchange -> {
			byte[] body = ("{\"status\":\"ok\",\"launchId\":\"" + launchId
					+ "\",\"identity\":{\"protocolVersion\":1}}").getBytes(StandardCharsets.UTF_8);
			exchange.sendResponseHeaders(200, body.length);
			try (var output = exchange.getResponseBody()) {
				output.write(body);
			}
		});
		server.start();
		try {
			URI endpoint = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/");
			HostedControlManager.JdkHealthProbe probe = new HostedControlManager.JdkHealthProbe();
			assertTrue(probe.isHealthy(endpoint, Duration.ofSeconds(2), launchId));
			assertFalse(probe.isHealthy(endpoint, Duration.ofSeconds(2),
					"00000000-0000-0000-0000-000000000002"));
		} finally {
			server.stop(0);
		}
	}

	@Test
	void missingPinnedArtifactIsStagedVerifiedAndStarted() throws Exception {
		byte[] release = "signed-release-content".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, false,
				digest(release), 30);
		FakeLauncher launcher = new FakeLauncher();
		AtomicInteger downloads = new AtomicInteger();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> {
					downloads.incrementAndGet();
					Files.write(target, release);
				}, launcher, (endpoint, timeout, launchId) -> true,
				millis -> { throw new AssertionError("healthy startup must not sleep"); }, System::nanoTime,
				message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.RUNNING, manager.status());
		assertEquals(1, downloads.get());
		assertEquals(List.of("signed-release-content"), launcher.launchedContents);
		assertEquals("signed-release-content", Files.readString(jar));
		manager.close();
		assertTrue(launcher.processes.get(0).destroyed);
	}

	@Test
	void blankPinDownloadsOfficialLatestAndCachesVerifiedMetadata() throws Exception {
		byte[] release = "latest-release-content".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/latest-control.jar");
		HostedControlManager.Settings settings = latestSettings(jar, root.resolve("control/latest-data"), true);
		HostedControlManager.ArtifactSpec latest = officialArtifact("v0.1.3", digest(release));
		FakeLauncher launcher = new FakeLauncher();
		AtomicInteger resolutions = new AtomicInteger();
		HostedControlManager manager = new HostedControlManager(settings,
				Executors.newSingleThreadScheduledExecutor(),
				(source, target, maximum, timeout) -> Files.write(target, release), timeout -> {
					resolutions.incrementAndGet();
					return latest;
				}, launcher, (configured, artifact, nodeId, verifier, timeout) -> { },
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, null, message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.RUNNING, manager.status());
		assertEquals(1, resolutions.get());
		assertEquals("latest-release-content", Files.readString(jar));
		assertTrue(Files.isRegularFile(settings.releaseStateFile()));
		assertTrue(Files.readString(settings.releaseStateFile()).contains("v0.1.3"));
		manager.close();
	}

	@Test
	void latestModeStagesAndRestartsOnlyAfterNewReleaseIsVerified() throws Exception {
		byte[] oldRelease = "old-release".getBytes(StandardCharsets.UTF_8);
		byte[] newRelease = "new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/updating-control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, oldRelease);
		HostedControlManager.Settings settings = latestSettings(jar, root.resolve("control/updating-data"), true);
		HostedControlManager.ArtifactSpec oldArtifact = officialArtifact("v0.1.2", digest(oldRelease));
		HostedControlManager.ArtifactSpec newArtifact = officialArtifact("v0.1.3", digest(newRelease));
		AtomicInteger resolutions = new AtomicInteger();
		AtomicInteger downloads = new AtomicInteger();
		FakeLauncher launcher = new FakeLauncher();
		HostedControlManager manager = new HostedControlManager(settings,
				Executors.newSingleThreadScheduledExecutor(), (source, target, maximum, timeout) -> {
					downloads.incrementAndGet();
					assertEquals(newArtifact.downloadUri(), source);
					Files.write(target, newRelease);
				}, timeout -> resolutions.getAndIncrement() == 0 ? oldArtifact : newArtifact,
				launcher, (configured, artifact, nodeId, verifier, timeout) -> { },
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, null, message -> { });

		manager.runOnce();
		HostedControlManager.ManagedProcess first = launcher.processes.get(0);
		manager.checkForLatestUpdate(first);

		assertEquals(2, resolutions.get());
		assertEquals(1, downloads.get());
		assertEquals(List.of("old-release", "new-release"), launcher.launchedContents);
		assertFalse(first.isAlive());
		assertEquals("new-release", Files.readString(jar));
		assertTrue(Files.readString(settings.releaseStateFile()).contains("v0.1.3"));
		manager.close();
	}

	@Test
	void localEnrollmentInstallsVerifierBeforeTheHostedProcessStarts() throws Exception {
		byte[] release = "release-with-enrollment-command".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, release);
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), false, false,
				digest(release), 30);
		ControlCredentialFile.PendingAutoEnrollment pending = ControlCredentialFile.prepareAutoEnrollment(root,
				"control/control-credential.txt", "proxy-a");
		AtomicInteger installations = new AtomicInteger();
		FakeLauncher launcher = new FakeLauncher();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> { throw new AssertionError("download was not expected"); },
				launcher, (configured, artifact, nodeId, verifier, timeout) -> {
					assertTrue(launcher.launchedContents.isEmpty());
					assertEquals("proxy-a", nodeId);
					assertEquals(pending.verifier(), verifier);
					installations.incrementAndGet();
				}, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, pending, message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.RUNNING, manager.status());
		assertEquals(1, installations.get());
		assertFalse(Files.exists(root.resolve("control/control-credential.txt.auto-enroll")));
		manager.close();
	}

	@Test
	void runningHostAcceptsVerifierOnlyBackendEnrollment() throws Exception {
		byte[] release = "release-with-enrollment-command".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, release);
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), false, false,
				digest(release), 30);
		settings = new HostedControlManager.Settings(settings.rootDirectory(), settings.jarFile(),
				settings.dataDirectory(), settings.autoDownload(), settings.autoUpdate(), settings.downloadUri(),
				settings.sha256(), "10.0.0.5", 8080, settings.startupTimeoutSeconds(),
				settings.downloadTimeoutSeconds());
		AtomicInteger installations = new AtomicInteger();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> { throw new AssertionError("download was not expected"); },
				new FakeLauncher(), (configured, artifact, nodeId, verifier, timeout) -> installations.incrementAndGet(),
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, null, message -> { });
		manager.runOnce();

		assertTrue(manager.installNodeVerifier("survival", "", "http://10.0.0.5:8080")
				.get(5, TimeUnit.SECONDS));
		assertEquals(0, installations.get());
		assertTrue(manager.installNodeVerifier("survival", "a".repeat(64), "http://10.0.0.5:8080")
				.get(5, TimeUnit.SECONDS));
		assertEquals(1, installations.get());
		assertFalse(manager.installNodeVerifier("../invalid", "a".repeat(64), "http://10.0.0.5:8080")
				.get(5, TimeUnit.SECONDS));
		assertFalse(manager.installNodeVerifier("survival", "b".repeat(64), "http://external.example:8080")
				.get(5, TimeUnit.SECONDS));
		manager.close();
	}

	@Test
	void wildcardHostProvesRequestedRouteReachesTheRunningLaunch() throws Exception {
		byte[] release = "release-with-enrollment-command".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/wildcard-control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, release);
		HostedControlManager.Settings base = settings(jar, root.resolve("control/wildcard-data"), false, false,
				digest(release), 30);
		HostedControlManager.Settings settings = new HostedControlManager.Settings(base.rootDirectory(), base.jarFile(),
				base.dataDirectory(), base.autoDownload(), base.autoUpdate(), base.downloadUri(), base.sha256(),
				"0.0.0.0", 2150, base.startupTimeoutSeconds(), base.downloadTimeoutSeconds());
		AtomicInteger installations = new AtomicInteger();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> { throw new AssertionError("download was not expected"); },
				new FakeLauncher(), (configured, artifact, nodeId, verifier, timeout) -> installations.incrementAndGet(),
				(endpoint, timeout, launchId) -> !"external.example".equals(endpoint.getHost()),
				millis -> { }, System::nanoTime, null, message -> { });
		manager.runOnce();

		assertTrue(manager.installNodeVerifier("survival", "a".repeat(64), "http://10.0.0.5:2150")
				.get(5, TimeUnit.SECONDS));
		assertFalse(manager.installNodeVerifier("creative", "b".repeat(64), "http://external.example:2150")
				.get(5, TimeUnit.SECONDS));
		assertFalse(manager.installNodeVerifier("loopback", "c".repeat(64), "http://127.0.0.1:2150")
				.get(5, TimeUnit.SECONDS));
		assertEquals(1, installations.get());
		manager.close();
	}

	@Test
	void failedUpdateRollsBackAndStartsPreviousRelease() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "bad-new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, true,
				digest(update), 1);
		FakeLauncher launcher = new FakeLauncher();
		AtomicInteger healthChecks = new AtomicInteger();
		AtomicLong clock = new AtomicLong();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> Files.write(target, update), launcher,
				(endpoint, timeout, launchId) -> healthChecks.incrementAndGet() > 4,
				millis -> clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis)), clock::get, message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.ROLLED_BACK, manager.status());
		assertEquals(List.of("bad-new-release", "previous-release"), launcher.launchedContents);
		assertEquals("previous-release", Files.readString(jar));
		assertEquals("bad-new-release", Files.readString(settings.failedFile()));
		assertTrue(launcher.processes.get(0).destroyed);
		launcher.processes.get(1).destroy();
		Files.writeString(jar, "corrupted-after-rollback");
		manager.runOnce();
		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		assertEquals(2, launcher.launchedContents.size());
		manager.close();
	}

	@Test
	void replacementIsDownloadedAndVerifiedBeforeActiveArtifactChanges() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, true,
				digest(update), 30);
		FakeLauncher launcher = new FakeLauncher();
		HostedControlManager manager = new HostedControlManager(settings,
				Executors.newSingleThreadScheduledExecutor(),
				(source, target, maximum, timeout) -> Files.write(target, update), launcher,
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		manager.prepareForReplacement();

		assertEquals("previous-release", Files.readString(jar));
		assertTrue(launcher.launchedContents.isEmpty());
		assertTrue(manager.startAndWaitForInitialResult());
		assertEquals(List.of("new-release"), launcher.launchedContents);
		manager.close();
	}

	@Test
	void unusableReplacementDoesNotChangeActiveArtifact() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, false,
				digest(update), 30);
		FakeLauncher launcher = new FakeLauncher();
		HostedControlManager manager = new HostedControlManager(settings,
				Executors.newSingleThreadScheduledExecutor(),
				(source, target, maximum, timeout) -> { throw new AssertionError("replacement downloaded"); },
				launcher, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		assertThrows(IOException.class, manager::prepareForReplacement);

		assertEquals("previous-release", Files.readString(jar));
		assertTrue(launcher.launchedContents.isEmpty());
		manager.close();
	}

	@Test
	void initialHealthFailureIsReportedToLifecycleCaller() throws Exception {
		byte[] release = "new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, false,
				digest(release), 1);
		AtomicLong clock = new AtomicLong();
		HostedControlManager manager = new HostedControlManager(settings,
				Executors.newSingleThreadScheduledExecutor(),
				(source, target, maximum, timeout) -> Files.write(target, release), new FakeLauncher(),
				(endpoint, timeout, launchId) -> false,
				millis -> clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis)), clock::get, message -> { });

		assertFalse(manager.startAndWaitForInitialResult());
		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		manager.close();
	}

	@Test
	void manualPinnedInstallNeedsNoDownloaderAndCloseIsNonBlocking() throws Exception {
		byte[] release = "manual-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		Files.createDirectories(jar.getParent());
		Files.write(jar, release);
		HostedControlManager.Settings settings = new HostedControlManager.Settings(root, jar,
				root.resolve("control/data"), false, false, null, digest(release), "127.0.0.1", 8080, 30, 60);
		FakeLauncher launcher = new FakeLauncher();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> { throw new AssertionError("manual install downloaded"); },
				launcher, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		manager.runOnce();
		manager.close();

		assertEquals(HostedControlManager.Status.STOPPED, manager.status());
		assertFalse(launcher.processes.get(0).isAlive());
	}

	@Test
	void digestMismatchNeverActivatesOrLaunchesDownloadedCode() throws Exception {
		byte[] expected = "expected-release".getBytes(StandardCharsets.UTF_8);
		byte[] untrusted = "different-content".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, false,
				digest(expected), 30);
		FakeLauncher launcher = new FakeLauncher();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> Files.write(target, untrusted), launcher,
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		assertFalse(Files.exists(jar));
		assertTrue(launcher.processes.isEmpty());
		manager.close();
	}

	@Test
	void closeAndWaitDoesNotReturnUntilArtifactWorkerStops() throws Exception {
		byte[] release = "release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control/control.jar");
		HostedControlManager.Settings settings = settings(jar, root.resolve("control/data"), true, false,
				digest(release), 1);
		CountDownLatch entered = new CountDownLatch(1);
		CountDownLatch releaseWorker = new CountDownLatch(1);
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor, (source, target, maximum, timeout) -> {
			entered.countDown();
			while (releaseWorker.getCount() > 0) {
				try {
					releaseWorker.await();
				} catch (InterruptedException ignored) { }
			}
			Files.write(target, release);
		}, new FakeLauncher(), (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });
		manager.start();
		assertTrue(entered.await(2, TimeUnit.SECONDS));
		CountDownLatch closingStarted = new CountDownLatch(1);
		CompletableFuture<Void> closing = CompletableFuture.runAsync(() -> {
			closingStarted.countDown();
			manager.closeAndWait();
		});
		assertTrue(closingStarted.await(2, TimeUnit.SECONDS));
		assertFalse(closing.isDone());
		releaseWorker.countDown();
		closing.get(2, TimeUnit.SECONDS);
		assertTrue(executor.isTerminated());
	}

	@Test
	void forcedProcessTerminationIsAwaitedBeforeReturning() {
		StubbornProcess process = new StubbornProcess();

		HostedControlManager.stopProcess(process, true);

		assertTrue(process.forciblyDestroyed);
		assertEquals(2, process.waits.get());
	}

	@Test
	void failedForcedTerminationRetainsManagedProcessForRetry() throws Exception {
		Path root = directory.toAbsolutePath().normalize();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(
				settings(root.resolve("control.jar"), root.resolve("data"), false, false, digest("x".getBytes()), 1),
				executor, (source, target, maximum, timeout) -> { }, new FakeLauncher(),
				(endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });
		UnkillableProcess process = new UnkillableProcess();
		java.lang.reflect.Field managed = HostedControlManager.class.getDeclaredField("managedProcess");
		managed.setAccessible(true);
		managed.set(manager, process);

		assertThrows(IllegalStateException.class, manager::closeAndWait);

		assertSame(process, managed.get(manager));
		assertTrue(process.forciblyDestroyed);
		assertEquals(2, process.waits.get());
	}

	@Test
	void failedHealthTerminationDefersRetryUntilRetainedProcessExits() throws Exception {
		byte[] release = "manual-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, release);
		ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
		RetainedProcess process = new RetainedProcess();
		AtomicInteger launches = new AtomicInteger();
		AtomicLong clock = new AtomicLong();
		HostedControlManager manager = new HostedControlManager(
				settings(jar, root.resolve("data"), false, false, digest(release), 1), executor,
				(source, target, maximum, timeout) -> { }, (settings, artifact, launchId) -> {
					launches.incrementAndGet();
					return process;
				}, (endpoint, timeout, launchId) -> false,
				millis -> clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis)), clock::get, message -> { });

		manager.runOnce();
		manager.runOnce();

		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		assertEquals(1, launches.get());
		assertTrue(executor.getQueue().isEmpty());
		process.exit();
		assertEquals(1, executor.getQueue().size());
		manager.close();
	}

	@Test
	void delayedUpdateProcessExitPreservesRollbackEligibility() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "bad-new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("data"), true, true,
				digest(update), 1);
		ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
		RetainedProcess retained = new RetainedProcess();
		List<String> launches = new ArrayList<>();
		AtomicInteger launchCount = new AtomicInteger();
		AtomicLong clock = new AtomicLong();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> Files.write(target, update),
				(launchSettings, artifact, launchId) -> {
					launches.add(Files.readString(artifact));
					return launchCount.getAndIncrement() == 0 ? retained : new FakeProcess();
				}, (endpoint, timeout, launchId) -> "previous-release".equals(launches.get(launches.size() - 1)),
				millis -> clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis)), clock::get, message -> { });

		manager.runOnce();
		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		retained.exit();
		manager.runOnce();

		assertEquals(HostedControlManager.Status.ROLLED_BACK, manager.status());
		assertEquals(List.of("bad-new-release", "previous-release"), launches);
		assertEquals("previous-release", Files.readString(jar));
		assertEquals("bad-new-release", Files.readString(settings.failedFile()));
		manager.close();
	}

	@Test
	void pendingUpdateRollbackSurvivesManagerRestart() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] failedUpdate = "bad-new-release-a".getBytes(StandardCharsets.UTF_8);
		byte[] nextUpdate = "new-release-b".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("data"), true, true,
				digest(failedUpdate), 1);
		FakeLauncher failedLauncher = new FakeLauncher();
		ScheduledExecutorService failedExecutor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager failedManager = new HostedControlManager(settings, failedExecutor,
				(source, target, maximum, timeout) -> Files.write(target, failedUpdate), failedLauncher,
				(endpoint, timeout, launchId) -> { throw new IllegalStateException("simulated JVM exit"); },
				millis -> { }, System::nanoTime, message -> { });

		failedManager.runOnce();
		failedExecutor.shutdownNow();
		assertEquals("bad-new-release-a", Files.readString(jar));
		assertTrue(Files.isRegularFile(settings.rollbackPendingFile()));

		HostedControlManager.Settings recoveredSettings = settings(jar, root.resolve("data"), true, true,
				digest(nextUpdate), 1);
		FakeLauncher recoveredLauncher = new FakeLauncher();
		AtomicInteger recoveredDownloads = new AtomicInteger();
		ScheduledExecutorService recoveredExecutor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager recoveredManager = new HostedControlManager(recoveredSettings, recoveredExecutor,
				(source, target, maximum, timeout) -> {
					recoveredDownloads.incrementAndGet();
					Files.write(target, nextUpdate);
				},
				recoveredLauncher, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime,
				message -> { });
		recoveredManager.runOnce();

		assertEquals(HostedControlManager.Status.ROLLED_BACK, recoveredManager.status());
		assertEquals(List.of("previous-release"), recoveredLauncher.launchedContents);
		assertEquals("previous-release", Files.readString(jar));
		assertEquals(0, recoveredDownloads.get());
		assertFalse(Files.exists(recoveredSettings.rollbackPendingFile()));

		recoveredLauncher.processes.get(0).destroy();
		recoveredManager.runOnce();
		assertEquals(HostedControlManager.Status.RUNNING, recoveredManager.status());
		assertEquals(1, recoveredDownloads.get());
		assertEquals(List.of("previous-release", "new-release-b"), recoveredLauncher.launchedContents);
		assertEquals("new-release-b", Files.readString(jar));
		recoveredManager.close();
	}

	@Test
	void quarantinedUpdateDigestSurvivesManagerRestart() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "bad-new-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("data"), true, true,
				digest(update), 1);
		FakeLauncher failedLauncher = new FakeLauncher();
		AtomicInteger healthChecks = new AtomicInteger();
		AtomicLong clock = new AtomicLong();
		ScheduledExecutorService failedExecutor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager failedManager = new HostedControlManager(settings, failedExecutor,
				(source, target, maximum, timeout) -> Files.write(target, update), failedLauncher,
				(endpoint, timeout, launchId) -> healthChecks.incrementAndGet() > 4,
				millis -> clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(millis)), clock::get, message -> { });
		failedManager.runOnce();
		assertEquals(HostedControlManager.Status.ROLLED_BACK, failedManager.status());
		assertTrue(Files.isRegularFile(settings.quarantineFile()));
		failedManager.close();

		FakeLauncher restartedLauncher = new FakeLauncher();
		ScheduledExecutorService restartedExecutor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager restartedManager = new HostedControlManager(settings, restartedExecutor,
				(source, target, maximum, timeout) -> { throw new AssertionError("quarantined update downloaded"); },
				restartedLauncher, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime,
				message -> { });
		restartedManager.runOnce();

		assertEquals(HostedControlManager.Status.RUNNING, restartedManager.status());
		assertEquals(List.of("previous-release"), restartedLauncher.launchedContents);
		assertEquals("previous-release", Files.readString(jar));
		restartedManager.close();
	}

	@Test
	void incompleteActivationIsRetriedWithoutQuarantiningCandidate() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "candidate-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("data"), true, true,
				digest(update), 1);
		AtomicInteger downloads = new AtomicInteger();
		AtomicInteger launches = new AtomicInteger();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> {
					downloads.incrementAndGet();
					Files.write(target, update);
				}, (launchSettings, artifact, launchId) -> {
					if (launches.getAndIncrement() == 0) throw new IOException("simulated pre-launch exit");
					return new FakeProcess();
				}, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		manager.runOnce();
		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		assertFalse(Files.exists(settings.healthCheckingFile()));
		manager.runOnce();

		assertEquals(HostedControlManager.Status.RUNNING, manager.status());
		assertEquals(2, downloads.get());
		assertEquals(2, launches.get());
		assertEquals("candidate-release", Files.readString(jar));
		assertFalse(Files.exists(settings.quarantineFile()));
		manager.close();
	}

	@Test
	void launchStatePublicationFailureStopsTheLaunchedChild() throws Exception {
		byte[] previous = "previous-release".getBytes(StandardCharsets.UTF_8);
		byte[] update = "candidate-release".getBytes(StandardCharsets.UTF_8);
		Path root = directory.toAbsolutePath().normalize();
		Path jar = root.resolve("control.jar");
		Files.write(jar, previous);
		HostedControlManager.Settings settings = settings(jar, root.resolve("data"), true, true,
				digest(update), 1);
		FakeProcess process = new FakeProcess();
		ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
		HostedControlManager manager = new HostedControlManager(settings, executor,
				(source, target, maximum, timeout) -> Files.write(target, update),
				(launchSettings, artifact, launchId) -> {
					Files.createDirectory(settings.healthCheckingFile());
					Files.writeString(settings.healthCheckingFile().resolve("blocker"), "x");
					return process;
				}, (endpoint, timeout, launchId) -> true, millis -> { }, System::nanoTime, message -> { });

		manager.runOnce();

		assertEquals(HostedControlManager.Status.FAILED, manager.status());
		assertTrue(process.destroyed);
		assertFalse(process.isAlive());
		manager.close();
	}

	private HostedControlManager.Settings settings(Path jar, Path data, boolean autoDownload, boolean autoUpdate,
			String sha256, int startupTimeout) {
		Path root = directory.toAbsolutePath().normalize();
		return new HostedControlManager.Settings(root, jar, data, autoDownload, autoUpdate,
				URI.create("https://github.com/BenCodez/VotingPlugin-Control/releases/download/v0.1.0/control.jar"),
				sha256, "127.0.0.1", 8080, startupTimeout, 60);
	}

	private HostedControlManager.Settings latestSettings(Path jar, Path data, boolean autoUpdate) {
		Path root = directory.toAbsolutePath().normalize();
		return new HostedControlManager.Settings(root, jar, data, true, autoUpdate, null, "",
				"127.0.0.1", 8080, 30, 60);
	}

	private HostedControlManager.ArtifactSpec officialArtifact(String version, String sha256) {
		return new HostedControlManager.ArtifactSpec(URI.create(
				"https://github.com/BenCodez/VotingPlugin-Control/releases/download/" + version
						+ "/votingplugin-control.jar"), sha256, version);
	}

	private String digest(byte[] bytes) throws Exception {
		Path input = directory.resolve("digest-" + System.nanoTime());
		Files.write(input, bytes);
		return HostedControlManager.sha256(input);
	}

	private static final class FakeLauncher implements HostedControlManager.ProcessLauncher {
		private final List<String> launchedContents = new ArrayList<>();
		private final List<FakeProcess> processes = new ArrayList<>();

		@Override
		public HostedControlManager.ManagedProcess launch(HostedControlManager.Settings settings, Path artifact,
				String launchId)
				throws java.io.IOException {
			launchedContents.add(Files.readString(artifact));
			FakeProcess process = new FakeProcess();
			processes.add(process);
			return process;
		}
	}

	private static final class FakeProcess implements HostedControlManager.ManagedProcess {
		private final CompletableFuture<Void> exit = new CompletableFuture<>();
		private volatile boolean alive = true;
		private volatile boolean destroyed;

		@Override public boolean isAlive() { return alive; }
		@Override public void destroy() { destroyed = true; alive = false; exit.complete(null); }
		@Override public void destroyForcibly() { destroy(); }
		@Override public boolean waitFor(long timeout, TimeUnit unit) { return !alive; }
		@Override public CompletableFuture<Void> onExit() { return exit; }
	}

	private static final class StubbornProcess implements HostedControlManager.ManagedProcess {
		private final AtomicInteger waits = new AtomicInteger();
		private final CompletableFuture<Void> exit = new CompletableFuture<>();
		private volatile boolean alive = true;
		private volatile boolean forciblyDestroyed;

		@Override public boolean isAlive() { return alive; }
		@Override public void destroy() { }
		@Override public void destroyForcibly() {
			forciblyDestroyed = true;
			alive = false;
			exit.complete(null);
		}
		@Override public boolean waitFor(long timeout, TimeUnit unit) {
			waits.incrementAndGet();
			return !alive;
		}
		@Override public CompletableFuture<Void> onExit() { return exit; }
	}

	private static final class UnkillableProcess implements HostedControlManager.ManagedProcess {
		private final AtomicInteger waits = new AtomicInteger();
		private final CompletableFuture<Void> exit = new CompletableFuture<>();
		private volatile boolean forciblyDestroyed;

		@Override public boolean isAlive() { return true; }
		@Override public void destroy() { }
		@Override public void destroyForcibly() { forciblyDestroyed = true; }
		@Override public boolean waitFor(long timeout, TimeUnit unit) {
			waits.incrementAndGet();
			return false;
		}
		@Override public CompletableFuture<Void> onExit() { return exit; }
	}

	private static final class RetainedProcess implements HostedControlManager.ManagedProcess {
		private final CompletableFuture<Void> exit = new CompletableFuture<>();
		private volatile boolean alive = true;

		@Override public boolean isAlive() { return alive; }
		@Override public void destroy() { }
		@Override public void destroyForcibly() { }
		@Override public boolean waitFor(long timeout, TimeUnit unit) { return false; }
		@Override public CompletableFuture<Void> onExit() { return exit; }
		private void exit() {
			alive = false;
			exit.complete(null);
		}
	}
}
