package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.HexFormat;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.LongSupplier;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.util.BoundedHttpBodyHandler;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Opt-in provisioner for a separate VotingPlugin Control JVM. Downloads and
 * process supervision never run in the proxy event loop or vote-processing path.
 */
public final class HostedControlManager implements AutoCloseable {
	static final long MAX_DOWNLOAD_BYTES = 64L * 1024L * 1024L;
	private static final int HEALTH_ATTEMPTS_PER_SECOND = 4;
	private static final int EXPECTED_PROTOCOL_VERSION = 1;

	private final Settings settings;
	private final ScheduledExecutorService executor;
	private final ArtifactDownloader downloader;
	private final ProcessLauncher launcher;
	private final HealthProbe healthProbe;
	private final Sleeper sleeper;
	private final LongSupplier nanoTime;
	private final Consumer<String> logger;
	private final AtomicBoolean workInProgress = new AtomicBoolean();
	private final Object processLifecycle = new Object();
	private volatile boolean closed;
	private volatile Status status = Status.STARTING;
	private volatile ManagedProcess managedProcess;
	private volatile ManagedProcess exitAwaitedProcess;
	private volatile Future<?> activeTask;
	private volatile int failures;
	private volatile String quarantinedSha256;
	private volatile String trustedRollbackSha256;
	private volatile String rollbackCandidateSha256;
	private volatile boolean rollbackCandidateReachedHealth;
	private volatile boolean rollbackPending;

	private HostedControlManager(Settings settings, Consumer<String> logger) {
		this(settings, daemonExecutor(), new JdkArtifactDownloader(), new JdkProcessLauncher(),
				new JdkHealthProbe(), Thread::sleep, System::nanoTime, logger);
	}

	HostedControlManager(Settings settings, ScheduledExecutorService executor, ArtifactDownloader downloader,
			ProcessLauncher launcher, HealthProbe healthProbe, Sleeper sleeper, LongSupplier nanoTime,
			Consumer<String> logger) {
		this.settings = Objects.requireNonNull(settings, "settings");
		this.executor = Objects.requireNonNull(executor, "executor");
		this.downloader = Objects.requireNonNull(downloader, "downloader");
		this.launcher = Objects.requireNonNull(launcher, "launcher");
		this.healthProbe = Objects.requireNonNull(healthProbe, "healthProbe");
		this.sleeper = Objects.requireNonNull(sleeper, "sleeper");
		this.nanoTime = Objects.requireNonNull(nanoTime, "nanoTime");
		this.logger = Objects.requireNonNull(logger, "logger");
	}

	public static HostedControlManager create(VotingPluginProxy proxy) {
		VotingPluginProxyConfig config = proxy.getConfig();
		if (!config.getControlHostedEnabled()) {
			return null;
		}
		Path root = proxy.getDataFolderPlugin().toPath().toAbsolutePath().normalize();
		Settings settings = new Settings(root,
				resolveInside(root, config.getControlHostedJarFile(), "Control hosted JAR"),
				resolveInside(root, config.getControlHostedDataDirectory(), "Control hosted data directory"),
				config.getControlHostedAutoDownload(), config.getControlHostedAutoUpdate(),
				parseDownloadUri(config.getControlHostedDownloadUrl()), config.getControlHostedSha256(),
				config.getControlHostedHost(), config.getControlHostedPort(),
				config.getControlHostedStartupTimeoutSeconds(), config.getControlHostedDownloadTimeoutSeconds());
		return new HostedControlManager(settings, message -> proxy.log("[Control Host] " + message));
	}

	public void start() {
		if (closed || activeTask != null) {
			return;
		}
		activeTask = executor.submit(this::runOnce);
	}

	void runOnce() {
		if (closed || !workInProgress.compareAndSet(false, true)) {
			return;
		}
		try {
			secureDirectory(settings.rootDirectory(), settings.jarFile().getParent());
			secureDirectory(settings.rootDirectory(), settings.dataDirectory());
			recoverPersistedRollbackState();
			ManagedProcess retained = managedProcess;
			if (retained != null) {
				if (retained.isAlive()) return;
				clearManagedProcess(retained);
			}
			if (rollbackPending && Files.isRegularFile(settings.previousFile(), LinkOption.NOFOLLOW_LINKS)) {
				if (rollbackCandidateReachedHealth) {
					if (rollbackAndStartPrevious()) return;
					throw new IOException("The previous Control release did not become healthy");
				}
				restoreIncompleteActivation();
			}
			recoverPersistedQuarantineState();
			boolean updated = prepareArtifact();
			if (updated) {
				rollbackCandidateSha256 = settings.sha256();
				rollbackPending = true;
			}
			if (closed) {
				return;
			}
			LaunchedProcess launched = launch(settings.jarFile());
			ManagedProcess process = launched.process();
			if (updated) {
				try {
					persistHealthCheckingState(trustedRollbackSha256, rollbackCandidateSha256);
				} catch (IOException publicationFailure) {
					try {
						stopProcess(process, true);
					} catch (RuntimeException stopFailure) {
						publicationFailure.addSuppressed(stopFailure);
					}
					throw publicationFailure;
				}
				rollbackCandidateReachedHealth = true;
			}
			if (awaitHealthy(launched)) {
				failures = 0;
				clearPersistedRollbackState();
				rollbackCandidateSha256 = null;
				rollbackCandidateReachedHealth = false;
				rollbackPending = false;
				status = Status.RUNNING;
				logger.accept("WebUI is available at " + settings.endpoint());
				monitor(process);
				return;
			}
			stopProcess(process, true);
			if (rollbackPending && Files.isRegularFile(settings.previousFile(), LinkOption.NOFOLLOW_LINKS)) {
				if (rollbackAndStartPrevious()) return;
			}
			throw new IOException("Control did not become healthy");
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			if (!closed) {
				fail();
			}
		} catch (Exception e) {
			if (!closed) {
				fail();
			}
		} finally {
			workInProgress.set(false);
		}
	}

	private boolean rollbackAndStartPrevious() throws IOException, InterruptedException {
		String failedCandidateSha256 = rollbackCandidateSha256;
		persistQuarantineState(trustedRollbackSha256, failedCandidateSha256);
		rollback();
		rollbackPending = false;
		quarantinedSha256 = failedCandidateSha256;
		rollbackCandidateSha256 = null;
		rollbackCandidateReachedHealth = false;
		LaunchedProcess previousLaunch = launch(settings.jarFile());
		ManagedProcess previous = previousLaunch.process();
		if (awaitHealthy(previousLaunch)) {
			failures = 0;
			status = Status.ROLLED_BACK;
			logger.accept("The new Control release failed health checks; the previous release is running");
			monitor(previous);
			return true;
		}
		stopProcess(previous, true);
		return false;
	}

	private boolean prepareArtifact() throws IOException, InterruptedException {
		String installedSha256 = null;
		if (Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			installedSha256 = sha256(settings.jarFile());
			if (settings.sha256().equals(installedSha256)) {
				return false;
			}
			if (settings.sha256().equals(quarantinedSha256)) {
				if (installedSha256.equals(trustedRollbackSha256)) return false;
				throw new IOException("Rolled-back Control artifact no longer matches its trusted digest");
			}
			trustedRollbackSha256 = installedSha256;
		}
		boolean installed = Files.exists(settings.jarFile(), LinkOption.NOFOLLOW_LINKS);
		if (installed && !Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control artifact path is not a regular file");
		}
		if ((installed && !settings.autoUpdate()) || (!installed && !settings.autoDownload())) {
			throw new IOException("Control artifact is unavailable or does not match its pin");
		}
		if (settings.downloadUri() == null) {
			throw new IOException("Control download URL is not configured");
		}

		status = Status.DOWNLOADING;
		Path staged = settings.jarFile().resolveSibling(settings.jarFile().getFileName() + ".staged-" + UUID.randomUUID());
		try {
			downloader.download(settings.downloadUri(), staged, MAX_DOWNLOAD_BYTES,
					Duration.ofSeconds(settings.downloadTimeoutSeconds()));
			if (!settings.sha256().equals(sha256(staged))) {
				throw new IOException("Downloaded Control artifact failed SHA-256 verification");
			}
			activate(staged, installed, installedSha256);
			return installed;
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	private void activate(Path staged, boolean installed, String installedSha256) throws IOException {
		if (installed) {
			persistRollbackState(installedSha256);
			atomicMove(settings.jarFile(), settings.previousFile(), true);
		}
		try {
			atomicMove(staged, settings.jarFile(), false);
		} catch (IOException e) {
			if (installed && Files.exists(settings.previousFile(), LinkOption.NOFOLLOW_LINKS)) {
				atomicMove(settings.previousFile(), settings.jarFile(), true);
				clearPersistedRollbackState();
			}
			throw e;
		}
	}

	private void rollback() throws IOException {
		if (Files.exists(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			atomicMove(settings.jarFile(), settings.failedFile(), true);
		}
		atomicMove(settings.previousFile(), settings.jarFile(), false);
		clearPersistedRollbackState();
	}

	private void persistRollbackState(String previousSha256) throws IOException {
		Files.deleteIfExists(settings.healthCheckingFile());
		persistDigestState(settings.rollbackPendingFile(), previousSha256, settings.sha256(), "rollback");
	}

	private void persistHealthCheckingState(String previousSha256, String candidateSha256) throws IOException {
		persistDigestState(settings.healthCheckingFile(), previousSha256, candidateSha256, "health-checking");
	}

	private void persistQuarantineState(String previousSha256, String candidateSha256) throws IOException {
		persistDigestState(settings.quarantineFile(), previousSha256, candidateSha256, "quarantine");
	}

	private void persistDigestState(Path marker, String previousSha256, String candidateSha256, String name)
			throws IOException {
		if (previousSha256 == null || !previousSha256.matches("[0-9a-f]{64}")) {
			throw new IOException("Control " + name + " digest is unavailable");
		}
		if (candidateSha256 == null || !candidateSha256.matches("[0-9a-f]{64}")) {
			throw new IOException("Control " + name + " candidate digest is unavailable");
		}
		if (Files.exists(marker, LinkOption.NOFOLLOW_LINKS)
				&& !Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control " + name + " state path is not a regular file");
		}
		Path staged = marker.resolveSibling(marker.getFileName() + ".staged-" + UUID.randomUUID());
		byte[] state = (previousSha256 + "\n" + candidateSha256 + "\n")
				.getBytes(java.nio.charset.StandardCharsets.US_ASCII);
		try (FileChannel channel = FileChannel.open(staged, StandardOpenOption.CREATE_NEW,
				StandardOpenOption.WRITE)) {
			ByteBuffer buffer = ByteBuffer.wrap(state);
			while (buffer.hasRemaining()) channel.write(buffer);
			channel.force(true);
		}
		try {
			atomicMove(staged, marker, true);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	private void recoverPersistedRollbackState() throws IOException {
		Path marker = settings.rollbackPendingFile();
		String[] lines = readDigestState(marker, "rollback");
		if (lines == null) return;
		String previousSha256 = lines[0];
		String candidateSha256 = lines[1];
		boolean active = Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS);
		String activeSha256 = active ? sha256(settings.jarFile()) : null;
		if (previousSha256.equals(activeSha256)) {
			clearPersistedRollbackState();
			rollbackCandidateSha256 = null;
			rollbackCandidateReachedHealth = false;
			rollbackPending = false;
			return;
		}
		if (active && !candidateSha256.equals(activeSha256)) {
			throw new IOException("Active Control artifact does not match pending rollback state");
		}
		if (!Files.isRegularFile(settings.previousFile(), LinkOption.NOFOLLOW_LINKS)
				|| !previousSha256.equals(sha256(settings.previousFile()))) {
			throw new IOException("Previous Control artifact does not match pending rollback state");
		}
		trustedRollbackSha256 = previousSha256;
		rollbackCandidateSha256 = candidateSha256;
		String[] healthChecking = readDigestState(settings.healthCheckingFile(), "health-checking");
		if (healthChecking != null && (!previousSha256.equals(healthChecking[0])
				|| !candidateSha256.equals(healthChecking[1]))) {
			throw new IOException("Control health-checking state does not match pending rollback state");
		}
		rollbackCandidateReachedHealth = healthChecking != null;
		rollbackPending = true;
	}

	private void restoreIncompleteActivation() throws IOException {
		if (Files.exists(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			atomicMove(settings.jarFile(), settings.failedFile(), true);
		}
		atomicMove(settings.previousFile(), settings.jarFile(), false);
		clearPersistedRollbackState();
		trustedRollbackSha256 = null;
		rollbackCandidateSha256 = null;
		rollbackCandidateReachedHealth = false;
		rollbackPending = false;
	}

	private void recoverPersistedQuarantineState() throws IOException {
		String[] lines = readDigestState(settings.quarantineFile(), "quarantine");
		if (lines == null) return;
		String previousSha256 = lines[0];
		String candidateSha256 = lines[1];
		if (!settings.sha256().equals(candidateSha256)) {
			clearPersistedQuarantineState();
			quarantinedSha256 = null;
			trustedRollbackSha256 = null;
			return;
		}
		if (!Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)
				|| !previousSha256.equals(sha256(settings.jarFile()))) {
			throw new IOException("Active Control artifact does not match quarantined rollback state");
		}
		trustedRollbackSha256 = previousSha256;
		quarantinedSha256 = candidateSha256;
	}

	private String[] readDigestState(Path marker, String name) throws IOException {
		if (!Files.exists(marker, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control " + name + " state is invalid");
		}
		byte[] bytes;
		try (FileChannel channel = FileChannel.open(marker, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)) {
			long size = channel.size();
			if (size > 256L) throw new IOException("Control " + name + " state is invalid");
			bytes = new byte[(int) size];
			ByteBuffer buffer = ByteBuffer.wrap(bytes);
			while (buffer.hasRemaining() && channel.read(buffer) >= 0) { }
		}
		String[] lines = new String(bytes, java.nio.charset.StandardCharsets.US_ASCII).split("\\R");
		if (lines.length != 2 || !lines[0].matches("[0-9a-f]{64}") || !lines[1].matches("[0-9a-f]{64}")) {
			throw new IOException("Control " + name + " state is invalid");
		}
		return lines;
	}

	private void clearPersistedRollbackState() throws IOException {
		Files.deleteIfExists(settings.rollbackPendingFile());
		Files.deleteIfExists(settings.healthCheckingFile());
	}

	private void clearPersistedQuarantineState() throws IOException {
		Files.deleteIfExists(settings.quarantineFile());
	}

	private LaunchedProcess launch(Path artifact) throws IOException {
		status = Status.STARTING_PROCESS;
		synchronized (processLifecycle) {
			if (closed) throw new IOException("Hosted Control manager is closed");
			if (managedProcess != null) {
				if (managedProcess.isAlive()) {
					throw new IOException("The previous hosted Control process is still running");
				}
				managedProcess = null;
				exitAwaitedProcess = null;
			}
			String launchId = UUID.randomUUID().toString();
			ManagedProcess process = launcher.launch(settings, artifact, launchId);
			managedProcess = process;
			return new LaunchedProcess(process, launchId);
		}
	}

	private boolean awaitHealthy(LaunchedProcess launched) throws InterruptedException {
		ManagedProcess process = launched.process();
		long timeoutNanos = TimeUnit.SECONDS.toNanos(settings.startupTimeoutSeconds());
		long started = nanoTime.getAsLong();
		long deadline = started + timeoutNanos;
		if (deadline < started) {
			deadline = Long.MAX_VALUE;
		}
		boolean firstAttempt = true;
		while (!closed) {
			if (!process.isAlive()) {
				return false;
			}
			long now = nanoTime.getAsLong();
			if (!firstAttempt && now >= deadline) {
				return false;
			}
			firstAttempt = false;
			long remaining = Math.max(1L, deadline - now);
			if (healthProbe.isHealthy(settings.endpoint(),
					Duration.ofNanos(Math.min(TimeUnit.SECONDS.toNanos(2), remaining)), launched.launchId())) {
				return process.isAlive();
			}
			if (nanoTime.getAsLong() >= deadline) {
				return false;
			}
			sleeper.sleep(1000L / HEALTH_ATTEMPTS_PER_SECOND);
		}
		return false;
	}

	private void monitor(ManagedProcess process) {
		process.onExit().thenRun(() -> {
			if (closed || managedProcess != process) {
				return;
			}
			managedProcess = null;
			status = Status.FAILED;
			logger.accept("Control exited unexpectedly; a bounded restart will be attempted");
			scheduleRetry();
		});
	}

	private void fail() {
		status = Status.FAILED;
		ManagedProcess process = managedProcess;
		if (process != null && process.isAlive()) {
			failWhileProcessIsAlive(process);
			return;
		}
		if (process != null) clearManagedProcess(process);
		logger.accept("Control could not be provisioned or started; VotingPlugin remains unaffected and will retry");
		scheduleRetry();
	}

	private void failWhileProcessIsAlive(ManagedProcess process) {
		status = Status.FAILED;
		synchronized (processLifecycle) {
			if (closed || managedProcess != process || exitAwaitedProcess == process) return;
			exitAwaitedProcess = process;
		}
		logger.accept("Control could not be stopped; provisioning will retry only after that process exits");
		process.onExit().whenComplete((ignored, failure) -> {
			boolean retry;
			synchronized (processLifecycle) {
				if (exitAwaitedProcess == process) exitAwaitedProcess = null;
				retry = !closed && managedProcess == process;
				if (retry) managedProcess = null;
			}
			if (retry) scheduleRetry();
		});
	}

	private void scheduleRetry() {
		if (closed) {
			return;
		}
		int attempt = Math.min(failures++, 6);
		long delay = Math.min(300L, 5L << attempt);
		try {
			executor.schedule(this::runOnce, delay, TimeUnit.SECONDS);
		} catch (RuntimeException ignored) {
			// Executor shutdown raced with the failure callback.
		}
	}

	public Status status() {
		return status;
	}

	@Override
	public void close() {
		close(false);
	}

	/** Used only by an explicit plugin reload to avoid racing the replacement bind. */
	public void closeAndWait() {
		close(true);
	}

	private void close(boolean waitForProcess) {
		ManagedProcess process;
		synchronized (processLifecycle) {
			closed = true;
			status = Status.STOPPED;
			process = managedProcess;
		}
		Future<?> task = activeTask;
		if (task != null) {
			task.cancel(true);
		}
		RuntimeException shutdownFailure = null;
		try {
			if (process != null) {
				stopProcess(process, waitForProcess);
				if (waitForProcess) {
					clearManagedProcess(process);
				} else {
					process.onExit().whenComplete((ignored, failure) -> clearManagedProcess(process));
				}
			}
		} catch (RuntimeException failure) {
			shutdownFailure = failure;
		} finally {
			executor.shutdownNow();
		}
		if (waitForProcess) {
			long waitSeconds = (long) settings.downloadTimeoutSeconds() + settings.startupTimeoutSeconds() + 5L;
			try {
				if (!executor.awaitTermination(waitSeconds, TimeUnit.SECONDS)) {
					IllegalStateException workerFailure = new IllegalStateException(
							"Hosted Control worker did not stop before reload");
					if (shutdownFailure != null) workerFailure.addSuppressed(shutdownFailure);
					throw workerFailure;
				}
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				IllegalStateException interrupted = new IllegalStateException(
						"Interrupted while waiting for the hosted Control worker", e);
				if (shutdownFailure != null) interrupted.addSuppressed(shutdownFailure);
				throw interrupted;
			}
		}
		if (shutdownFailure != null) throw shutdownFailure;
	}

	private void clearManagedProcess(ManagedProcess process) {
		synchronized (processLifecycle) {
			if (managedProcess == process) {
				managedProcess = null;
				if (exitAwaitedProcess == process) exitAwaitedProcess = null;
			}
		}
	}

	static void stopProcess(ManagedProcess process, boolean wait) {
		process.destroy();
		if (wait) {
			try {
				if (!process.waitFor(3, TimeUnit.SECONDS)) {
					process.destroyForcibly();
					if (!process.waitFor(3, TimeUnit.SECONDS)) {
						throw new IllegalStateException("Hosted Control process did not stop after forced termination");
					}
				}
			} catch (InterruptedException e) {
				process.destroyForcibly();
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Interrupted while stopping the hosted Control process", e);
			}
		} else {
			process.onExit().orTimeout(3, TimeUnit.SECONDS).exceptionally(failure -> {
				process.destroyForcibly();
				return null;
			});
		}
	}

	static String sha256(Path file) throws IOException {
		try (InputStream input = Files.newInputStream(file);
				DigestInputStream digest = new DigestInputStream(input, MessageDigest.getInstance("SHA-256"))) {
			digest.transferTo(OutputStream.nullOutputStream());
			return HexFormat.of().formatHex(digest.getMessageDigest().digest());
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable", e);
		}
	}

	private static void secureDirectory(Path root, Path directory) throws IOException {
		Files.createDirectories(root);
		Files.createDirectories(directory);
		Path realRoot = root.toRealPath();
		Path realDirectory = directory.toRealPath();
		if (!realDirectory.startsWith(realRoot)) {
			throw new IOException("Control path escapes the plugin data directory");
		}
	}

	private static void atomicMove(Path source, Path target, boolean replace) throws IOException {
		try {
			if (replace) {
				Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			} else {
				Files.move(source, target, StandardCopyOption.ATOMIC_MOVE);
			}
		} catch (AtomicMoveNotSupportedException e) {
			throw new IOException("Atomic Control artifact activation is unsupported", e);
		}
	}

	static Path resolveInside(Path root, String configured, String name) {
		if (configured == null || configured.isBlank()) {
			throw new IllegalArgumentException(name + " is not configured");
		}
		Path relative = Path.of(configured.trim());
		if (relative.isAbsolute()) {
			throw new IllegalArgumentException(name + " must be relative to the plugin data directory");
		}
		Path resolved = root.resolve(relative).normalize();
		if (resolved.equals(root) || !resolved.startsWith(root)) {
			throw new IllegalArgumentException(name + " escapes the plugin data directory");
		}
		return resolved;
	}

	static URI parseDownloadUri(String configured) {
		if (configured == null || configured.isBlank()) {
			return null;
		}
		URI uri;
		try {
			uri = URI.create(configured.trim());
		} catch (IllegalArgumentException e) {
			throw new IllegalArgumentException("Control download URL is invalid");
		}
		if (!"https".equalsIgnoreCase(uri.getScheme()) || uri.getHost() == null || uri.getUserInfo() != null
				|| uri.getQuery() != null || uri.getFragment() != null || uri.getPath() == null
				|| uri.getPath().toLowerCase(Locale.ROOT).contains("/latest/")) {
			throw new IllegalArgumentException("Control download URL must be a versioned HTTPS release asset");
		}
		return uri;
	}

	private static ScheduledExecutorService daemonExecutor() {
		ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, runnable -> {
			Thread thread = new Thread(runnable, "votingplugin-control-host");
			thread.setDaemon(true);
			return thread;
		});
		executor.setRemoveOnCancelPolicy(true);
		return executor;
	}

	public enum Status {
		STARTING, DOWNLOADING, STARTING_PROCESS, RUNNING, ROLLED_BACK, FAILED, STOPPED
	}

	record Settings(Path rootDirectory, Path jarFile, Path dataDirectory, boolean autoDownload, boolean autoUpdate,
			URI downloadUri, String sha256, String host, int port, int startupTimeoutSeconds,
			int downloadTimeoutSeconds) {
		Settings {
			rootDirectory = Objects.requireNonNull(rootDirectory, "rootDirectory").toAbsolutePath().normalize();
			jarFile = Objects.requireNonNull(jarFile, "jarFile").toAbsolutePath().normalize();
			dataDirectory = Objects.requireNonNull(dataDirectory, "dataDirectory").toAbsolutePath().normalize();
			if (!jarFile.startsWith(rootDirectory) || !dataDirectory.startsWith(rootDirectory)
					|| jarFile.equals(rootDirectory) || dataDirectory.equals(rootDirectory)
					|| dataDirectory.startsWith(jarFile)) {
				throw new IllegalArgumentException("Control hosted paths are invalid");
			}
			sha256 = sha256 == null ? "" : sha256.trim().toLowerCase(Locale.ROOT);
			if (!sha256.matches("[0-9a-f]{64}")) {
				throw new IllegalArgumentException("Control hosted SHA-256 pin must contain 64 hexadecimal characters");
			}
			if ((autoDownload || autoUpdate) && downloadUri == null) {
				throw new IllegalArgumentException("Control hosted download URL is required");
			}
			host = host == null ? "" : host.trim();
			if (host.isEmpty() || host.indexOf('\0') >= 0) {
				throw new IllegalArgumentException("Control hosted host is invalid");
			}
			if (port < 1 || port > 65535 || startupTimeoutSeconds < 1 || startupTimeoutSeconds > 120
					|| downloadTimeoutSeconds < 5 || downloadTimeoutSeconds > 300) {
				throw new IllegalArgumentException("Control hosted bounds are invalid");
			}
		}

		Path previousFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".previous");
		}

		Path failedFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".failed");
		}

		Path rollbackPendingFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".rollback-pending");
		}

		Path quarantineFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".quarantined");
		}

		Path healthCheckingFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".health-checking");
		}

		URI endpoint() {
			try {
				return new URI("http", null, host, port, "/", null, null);
			} catch (Exception e) {
				throw new IllegalArgumentException("Control hosted endpoint is invalid");
			}
		}
	}

	interface ArtifactDownloader {
		void download(URI source, Path target, long maximumBytes, Duration timeout)
				throws IOException, InterruptedException;
	}

	interface ProcessLauncher {
		ManagedProcess launch(Settings settings, Path artifact, String launchId) throws IOException;
	}

	interface HealthProbe {
		boolean isHealthy(URI endpoint, Duration timeout, String launchId);
	}

	interface Sleeper {
		void sleep(long millis) throws InterruptedException;
	}

	interface ManagedProcess {
		boolean isAlive();
		void destroy();
		void destroyForcibly();
		boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException;
		CompletableFuture<Void> onExit();
	}

	static final class JdkArtifactDownloader implements ArtifactDownloader {
		private final HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10))
				.followRedirects(HttpClient.Redirect.NEVER).build();

		@Override
		public void download(URI source, Path target, long maximumBytes, Duration timeout)
				throws IOException, InterruptedException {
			long deadline = System.nanoTime() + timeout.toNanos();
			URI current = source;
			for (int redirects = 0; redirects <= 5; redirects++) {
				long requestNanos = remainingDownloadNanos(deadline);
				HttpRequest request = HttpRequest.newBuilder(current).timeout(Duration.ofNanos(requestNanos))
						.header("Accept", "application/java-archive, application/octet-stream")
						.header("User-Agent", "VotingPlugin-Control-Host").GET().build();
				HttpResponse<InputStream> response = client.send(request, HttpResponse.BodyHandlers.ofInputStream());
				int status = response.statusCode();
				if (status >= 300 && status < 400) {
					try (InputStream ignored = response.body()) {
						String location = response.headers().firstValue("Location")
								.orElseThrow(() -> new IOException("Control download redirect is invalid"));
						current = current.resolve(location);
						if (!"https".equalsIgnoreCase(current.getScheme()) || current.getHost() == null
								|| current.getUserInfo() != null) {
							throw new IOException("Control download redirected outside HTTPS");
						}
						continue;
					}
				}
				if (status != 200) {
					response.body().close();
					throw new IOException("Control download did not return HTTP 200");
				}
				long declared = response.headers().firstValueAsLong("Content-Length").orElse(-1L);
				if (declared > maximumBytes) {
					response.body().close();
					throw new IOException("Control artifact exceeds the download limit");
				}
				try (InputStream input = response.body(); OutputStream output = Files.newOutputStream(target,
						StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)) {
					AtomicBoolean bodyTimedOut = new AtomicBoolean();
					Thread timeoutThread = bodyTimeoutThread(input, deadline, bodyTimedOut);
					byte[] buffer = new byte[8192];
					long total = 0;
					int count;
					try {
						while ((count = input.read(buffer)) != -1) {
							if (bodyTimedOut.get() || System.nanoTime() >= deadline) {
								throw new IOException("Control artifact download timed out");
							}
							total += count;
							if (total > maximumBytes) {
								throw new IOException("Control artifact exceeds the download limit");
							}
							output.write(buffer, 0, count);
						}
					} catch (IOException failure) {
						if (bodyTimedOut.get()) throw new IOException("Control artifact download timed out", failure);
						throw failure;
					} finally {
						timeoutThread.interrupt();
					}
					if (bodyTimedOut.get()) throw new IOException("Control artifact download timed out");
				}
				return;
			}
			throw new IOException("Control download redirected too many times");
		}

		private static long remainingDownloadNanos(long deadline) throws IOException {
			long remaining = deadline - System.nanoTime();
			if (remaining <= 0) throw new IOException("Control artifact download timed out");
			return remaining;
		}

		private static Thread bodyTimeoutThread(InputStream input, long deadline, AtomicBoolean timedOut)
				throws IOException {
			long remaining = remainingDownloadNanos(deadline);
			Thread thread = new Thread(() -> {
				try {
					TimeUnit.NANOSECONDS.sleep(remaining);
					timedOut.set(true);
					input.close();
				} catch (InterruptedException ignored) {
					Thread.currentThread().interrupt();
				} catch (IOException ignored) { }
			}, "VotingPlugin-Control-Download-Timeout");
			thread.setDaemon(true);
			thread.setContextClassLoader(ClassLoader.getPlatformClassLoader());
			thread.start();
			return thread;
		}
	}

	private static final class JdkProcessLauncher implements ProcessLauncher {
		@Override
		public ManagedProcess launch(Settings settings, Path artifact, String launchId) throws IOException {
			Path log = artifact.resolveSibling("control-output.log");
			if (Files.exists(log, LinkOption.NOFOLLOW_LINKS)
					&& !Files.isRegularFile(log, LinkOption.NOFOLLOW_LINKS)) {
				throw new IOException("Control output path is not a regular file");
			}
			if (Files.isRegularFile(log, LinkOption.NOFOLLOW_LINKS) && Files.size(log) > 1024L * 1024L) {
				Files.move(log, log.resolveSibling("control-output.previous.log"), StandardCopyOption.REPLACE_EXISTING);
			}
			Path java = Path.of(System.getProperty("java.home"), "bin", isWindows() ? "java.exe" : "java");
			ProcessBuilder builder = new ProcessBuilder(java.toString(), "-jar", artifact.toString());
			builder.directory(artifact.getParent().toFile());
			builder.redirectErrorStream(true);
			builder.redirectOutput(ProcessBuilder.Redirect.appendTo(log.toFile()));
			var environment = builder.environment();
			environment.clear();
			for (String safe : Set.of("LANG", "LC_ALL", "SYSTEMROOT", "WINDIR", "TEMP", "TMP")) {
				String value = System.getenv(safe);
				if (value != null) {
					environment.put(safe, value);
				}
			}
			environment.put("CONTROL_HOST", settings.host());
			environment.put("CONTROL_PORT", Integer.toString(settings.port()));
			environment.put("CONTROL_DATA_DIR", settings.dataDirectory().toString());
			environment.put("CONTROL_LAUNCH_ID", launchId);
			environment.put("CONTROL_PARENT_PID", Long.toString(ProcessHandle.current().pid()));
			return new JdkManagedProcess(builder.start());
		}

		private static boolean isWindows() {
			return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
		}
	}

	static final class JdkHealthProbe implements HealthProbe {
		private final HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2))
				.followRedirects(HttpClient.Redirect.NEVER).build();

		@Override
		public boolean isHealthy(URI endpoint, Duration timeout, String launchId) {
			try {
				HttpRequest request = HttpRequest.newBuilder(endpoint.resolve("/api/v1/health"))
						.timeout(timeout).header("Accept", "application/json").GET().build();
				HttpResponse<byte[]> response = client.send(request, new BoundedHttpBodyHandler(64 * 1024, timeout));
				if (response.statusCode() != 200) return false;
				JsonElement parsed = JsonParser.parseString(
						new String(response.body(), java.nio.charset.StandardCharsets.UTF_8));
				if (!parsed.isJsonObject()) return false;
				JsonObject object = parsed.getAsJsonObject();
				JsonObject identity = object.has("identity") && object.get("identity").isJsonObject()
						? object.getAsJsonObject("identity") : null;
				return object.has("status") && "ok".equals(object.get("status").getAsString())
						&& object.has("launchId") && launchId.equals(object.get("launchId").getAsString())
						&& identity != null && identity.has("protocolVersion")
						&& identity.get("protocolVersion").getAsInt() == EXPECTED_PROTOCOL_VERSION;
			} catch (Exception e) {
				return false;
			}
		}
	}

	private record LaunchedProcess(ManagedProcess process, String launchId) { }

	private static final class JdkManagedProcess implements ManagedProcess {
		private final Process process;

		private JdkManagedProcess(Process process) {
			this.process = process;
		}

		@Override public boolean isAlive() { return process.isAlive(); }
		@Override public void destroy() { process.destroy(); }
		@Override public void destroyForcibly() { process.destroyForcibly(); }
		@Override public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
			return process.waitFor(timeout, unit);
		}
		@Override public CompletableFuture<Void> onExit() { return process.onExit().thenApply(ignored -> null); }
	}
}
