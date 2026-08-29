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
import java.util.concurrent.ConcurrentHashMap;
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
import com.bencodez.votingplugin.util.ControlCredentialFile;
import com.bencodez.votingplugin.util.ControlCredentialFile.PendingAutoEnrollment;
import com.bencodez.votingplugin.util.DurableFiles;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Opt-in provisioner for a separate VotingPlugin Control JVM. Downloads and
 * process supervision never run in the proxy event loop or vote-processing path.
 */
public final class HostedControlManager implements AutoCloseable {
	static final long MAX_DOWNLOAD_BYTES = 64L * 1024L * 1024L;
	static final int MAX_RELEASE_METADATA_BYTES = 256 * 1024;
	static final long LATEST_UPDATE_INTERVAL_SECONDS = TimeUnit.HOURS.toSeconds(6);
	static final URI LATEST_RELEASE_API = URI.create(
			"https://api.github.com/repos/BenCodez/VotingPlugin-Control/releases/latest");
	private static final int HEALTH_ATTEMPTS_PER_SECOND = 4;
	private static final int EXPECTED_PROTOCOL_VERSION = 1;

	private final Settings settings;
	private final ScheduledExecutorService executor;
	private final ArtifactDownloader downloader;
	private final ReleaseResolver releaseResolver;
	private final ProcessLauncher launcher;
	private final VerifierInstaller verifierInstaller;
	private final HealthProbe healthProbe;
	private final Sleeper sleeper;
	private final LongSupplier nanoTime;
	private final Consumer<String> logger;
	private final AtomicBoolean workInProgress = new AtomicBoolean();
	private final ConcurrentHashMap<String, CompletableFuture<Boolean>> enrollmentTasks = new ConcurrentHashMap<>();
	private final Object processLifecycle = new Object();
	private final Object preparationLifecycle = new Object();
	private volatile boolean closed;
	private volatile Status status = Status.STARTING;
	private volatile ManagedProcess managedProcess;
	private volatile ManagedProcess exitAwaitedProcess;
	private volatile Future<?> activeTask;
	private volatile Future<?> updateCheckTask;
	private volatile PreparedArtifact preparedArtifact;
	private volatile int failures;
	private volatile String quarantinedSha256;
	private volatile String trustedRollbackSha256;
	private volatile String rollbackCandidateSha256;
	private volatile boolean rollbackCandidateReachedHealth;
	private volatile boolean rollbackPending;
	private volatile String healthyLaunchId;
	private volatile ArtifactSpec trustedTargetArtifact;
	private volatile PendingAutoEnrollment localAutoEnrollment;

	private HostedControlManager(Settings settings, PendingAutoEnrollment localAutoEnrollment,
			Consumer<String> logger) {
		this(settings, daemonExecutor(), new JdkArtifactDownloader(), new JdkGithubReleaseResolver(),
				new JdkProcessLauncher(),
				new JdkVerifierInstaller(), new JdkHealthProbe(), Thread::sleep, System::nanoTime,
				localAutoEnrollment, logger);
	}

	HostedControlManager(Settings settings, ScheduledExecutorService executor, ArtifactDownloader downloader,
			ProcessLauncher launcher, HealthProbe healthProbe, Sleeper sleeper, LongSupplier nanoTime,
			Consumer<String> logger) {
		this(settings, executor, downloader, new JdkGithubReleaseResolver(), launcher,
				(configured, artifact, nodeId, verifier, timeout) -> { },
				healthProbe, sleeper, nanoTime, null, logger);
	}

	HostedControlManager(Settings settings, ScheduledExecutorService executor, ArtifactDownloader downloader,
			ProcessLauncher launcher, VerifierInstaller verifierInstaller, HealthProbe healthProbe, Sleeper sleeper,
			LongSupplier nanoTime, PendingAutoEnrollment localAutoEnrollment, Consumer<String> logger) {
		this(settings, executor, downloader, new JdkGithubReleaseResolver(), launcher, verifierInstaller,
				healthProbe, sleeper, nanoTime, localAutoEnrollment, logger);
	}

	HostedControlManager(Settings settings, ScheduledExecutorService executor, ArtifactDownloader downloader,
			ReleaseResolver releaseResolver, ProcessLauncher launcher, VerifierInstaller verifierInstaller,
			HealthProbe healthProbe, Sleeper sleeper, LongSupplier nanoTime,
			PendingAutoEnrollment localAutoEnrollment, Consumer<String> logger) {
		this.settings = Objects.requireNonNull(settings, "settings");
		this.executor = Objects.requireNonNull(executor, "executor");
		this.downloader = Objects.requireNonNull(downloader, "downloader");
		this.releaseResolver = Objects.requireNonNull(releaseResolver, "releaseResolver");
		this.launcher = Objects.requireNonNull(launcher, "launcher");
		this.verifierInstaller = Objects.requireNonNull(verifierInstaller, "verifierInstaller");
		this.healthProbe = Objects.requireNonNull(healthProbe, "healthProbe");
		this.sleeper = Objects.requireNonNull(sleeper, "sleeper");
		this.nanoTime = Objects.requireNonNull(nanoTime, "nanoTime");
		this.localAutoEnrollment = localAutoEnrollment;
		this.logger = Objects.requireNonNull(logger, "logger");
	}

	public static HostedControlManager create(VotingPluginProxy proxy) throws IOException {
		VotingPluginProxyConfig config = proxy.getConfig();
		HostConfiguration hosted = new HostConfiguration(config.getControlHostedEnabled(),
				config.getControlHostedAutoDownload(), config.getControlHostedAutoUpdate(),
				config.getControlHostedDownloadUrl(), config.getControlHostedSha256(),
				config.getControlHostedJarFile(), config.getControlHostedDataDirectory(),
				config.getControlHostedHost(), config.getControlHostedPort(),
				config.getControlHostedStartupTimeoutSeconds(), config.getControlHostedDownloadTimeoutSeconds());
		Path root = proxy.getDataFolderPlugin().toPath();
		PendingAutoEnrollment enrollment = null;
		if (config.getControlEnabled() && isDirectLocalEndpoint(config.getControlEndpoint(), hosted)) {
			try {
				String configuredNodeId = config.getControlNodeId();
				String nodeId = configuredNodeId == null || configuredNodeId.isBlank()
						? config.getProxyServerName() : configuredNodeId.trim();
				enrollment = ControlCredentialFile.prepareAutoEnrollment(root,
						config.getControlCredentialFile(), nodeId);
			} catch (IOException | IllegalArgumentException enrollmentFailure) {
				proxy.log("[Control] Automatic local credential enrollment was skipped because its connector settings are invalid");
			}
		}
		return create(root, hosted, enrollment, message -> proxy.log("[Control Host] " + message));
	}

	/**
	 * Creates the platform-neutral hosted Control supervisor. Bukkit uses this
	 * entry point so the shared manager never links against the Bukkit API when it
	 * is loaded on BungeeCord or Velocity.
	 */
	public static HostedControlManager create(Path rootDirectory, HostConfiguration config,
			Consumer<String> logger) {
		return create(rootDirectory, config, null, logger);
	}

	/** Creates a hosted supervisor with an optional verifier-only local enrollment. */
	public static HostedControlManager create(Path rootDirectory, HostConfiguration config,
			PendingAutoEnrollment localAutoEnrollment, Consumer<String> logger) {
		Objects.requireNonNull(config, "config");
		if (!config.enabled()) return null;
		Path root = Objects.requireNonNull(rootDirectory, "rootDirectory").toAbsolutePath().normalize();
		Settings settings = new Settings(root,
				resolveInside(root, config.jarFile(), "Control hosted JAR"),
				resolveInside(root, config.dataDirectory(), "Control hosted data directory"),
				config.autoDownload(), config.autoUpdate(), parseDownloadUri(config.downloadUrl()), config.sha256(),
				config.host(), config.port(), config.startupTimeoutSeconds(), config.downloadTimeoutSeconds());
		return new HostedControlManager(settings, localAutoEnrollment, Objects.requireNonNull(logger, "logger"));
	}

	public void start() {
		submitInitialTask();
	}

	/** Starts on the manager worker and waits for the initial health result. */
	public boolean startAndWaitForInitialResult() throws InterruptedException {
		Future<?> task = submitInitialTask();
		if (task == null) return false;
		try {
			task.get();
		} catch (java.util.concurrent.CancellationException | java.util.concurrent.ExecutionException e) {
			return false;
		}
		return status == Status.RUNNING || status == Status.ROLLED_BACK;
	}

	private synchronized Future<?> submitInitialTask() {
		if (closed) return null;
		if (activeTask == null) activeTask = executor.submit(this::runOnce);
		return activeTask;
	}

	/** Downloads and verifies a replacement without disturbing the active process. */
	public void prepareForReplacement() throws IOException, InterruptedException {
		synchronized (preparationLifecycle) {
			if (closed) throw new IOException("Hosted Control manager is closed");
			if (activeTask != null || workInProgress.get()) {
				throw new IOException("Hosted Control manager has already started");
			}
			if (preparedArtifact != null) return;
			secureDirectory(settings.rootDirectory(), settings.jarFile().getParent());
			secureDirectory(settings.rootDirectory(), settings.dataDirectory());
			ArtifactSpec target = resolveTargetArtifact();
			loadPersistedQuarantineStateForPreparation(target.sha256());
			preparedArtifact = stageArtifact(target);
		}
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
			PreparedArtifact prepared = takePreparedArtifact();
			ArtifactSpec target;
			if (prepared == null) {
				target = resolveTargetArtifact();
				recoverPersistedQuarantineState(target.sha256());
				prepared = stageArtifact(target);
			} else {
				target = prepared.target();
			}
			boolean updated;
			synchronized (processLifecycle) {
				if (closed) {
					deletePreparedArtifact(prepared);
					return;
				}
				updated = activatePreparedArtifact(prepared);
			}
			trustedTargetArtifact = target;
			if (updated) {
				rollbackCandidateSha256 = target.sha256();
				rollbackPending = true;
			}
			if (closed) {
				return;
			}
			installLocalAutoEnrollment();
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
				healthyLaunchId = launched.launchId();
				status = Status.RUNNING;
				if (target.sha256().equals(sha256(settings.jarFile()))) persistResolvedArtifactSafely(target);
				logger.accept("WebUI is available at " + settings.endpoint());
				monitor(process);
				scheduleLatestUpdateCheck(process);
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
				fail(e);
			}
		} catch (Exception e) {
			if (!closed) {
				fail(e);
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
			healthyLaunchId = previousLaunch.launchId();
			status = Status.ROLLED_BACK;
			logger.accept("The new Control release failed health checks; the previous release is running");
			monitor(previous);
			scheduleLatestUpdateCheck(previous);
			return true;
		}
		stopProcess(previous, true);
		return false;
	}

	private PreparedArtifact stageArtifact(ArtifactSpec target) throws IOException, InterruptedException {
		String installedSha256 = null;
		if (Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			installedSha256 = sha256(settings.jarFile());
			if (target.sha256().equals(installedSha256)) {
				return new PreparedArtifact(null, true, installedSha256, target);
			}
			if (target.sha256().equals(quarantinedSha256)) {
				if (installedSha256.equals(trustedRollbackSha256)) {
					return new PreparedArtifact(null, true, installedSha256, target);
				}
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
		if (target.downloadUri() == null) {
			throw new IOException("Control download URL is not configured");
		}

		status = Status.DOWNLOADING;
		Path staged = settings.jarFile().resolveSibling(settings.jarFile().getFileName() + ".staged-" + UUID.randomUUID());
		try {
			downloader.download(target.downloadUri(), staged, MAX_DOWNLOAD_BYTES,
					Duration.ofSeconds(settings.downloadTimeoutSeconds()));
			if (!target.sha256().equals(sha256(staged))) {
				throw new IOException("Downloaded Control artifact failed SHA-256 verification");
			}
			return new PreparedArtifact(staged, installed, installedSha256, target);
		} catch (IOException | InterruptedException | RuntimeException e) {
			Files.deleteIfExists(staged);
			throw e;
		}
	}

	private ArtifactSpec resolveTargetArtifact() throws IOException, InterruptedException {
		ArtifactSpec pinned = settings.pinnedArtifact();
		if (pinned != null) return pinned;
		if (!settings.autoUpdate()) {
			try {
				ArtifactSpec cached = readPersistedResolvedArtifact();
				if (cached != null) return cached;
			} catch (IOException ignored) {
				// A fresh official lookup may repair malformed cached metadata after launch.
			}
		}
		return resolveLatestArtifact(true);
	}

	private ArtifactSpec resolveLatestArtifact(boolean allowCachedFallback) throws IOException, InterruptedException {
		ArtifactSpec cached = null;
		IOException cachedFailure = null;
		try {
			cached = readPersistedResolvedArtifact();
		} catch (IOException e) {
			cachedFailure = e;
		}
		try {
			ArtifactSpec latest = releaseResolver.resolve(Duration.ofSeconds(settings.downloadTimeoutSeconds()));
			if (cached != null && compareReleaseVersions(latest.version(), cached.version()) <= 0) return cached;
			return latest;
		} catch (IOException e) {
			if (allowCachedFallback && cached != null) {
				logger.accept("GitHub latest-release lookup failed; using the last verified release metadata");
				return cached;
			}
			if (cachedFailure != null) e.addSuppressed(cachedFailure);
			throw e;
		}
	}

	private void persistResolvedArtifactSafely(ArtifactSpec artifact) {
		if (!settings.usesLatestRelease()) return;
		try {
			persistResolvedArtifact(artifact);
		} catch (IOException e) {
			logger.accept("Control is healthy, but its latest-release metadata could not be cached: "
					+ safeFailureMessage(e));
		}
	}

	private void persistResolvedArtifact(ArtifactSpec artifact) throws IOException {
		Path state = settings.releaseStateFile();
		if (Files.exists(state, LinkOption.NOFOLLOW_LINKS)
				&& !Files.isRegularFile(state, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control release metadata path is not a regular file");
		}
		Path staged = state.resolveSibling(state.getFileName() + ".staged-" + UUID.randomUUID());
		byte[] content = (artifact.version() + "\n" + artifact.sha256() + "\n"
				+ artifact.downloadUri() + "\n").getBytes(java.nio.charset.StandardCharsets.US_ASCII);
		try (FileChannel channel = FileChannel.open(staged, StandardOpenOption.CREATE_NEW,
				StandardOpenOption.WRITE)) {
			ByteBuffer buffer = ByteBuffer.wrap(content);
			while (buffer.hasRemaining()) channel.write(buffer);
			channel.force(true);
		}
		try {
			atomicMove(staged, state, true);
		} finally {
			Files.deleteIfExists(staged);
		}
	}

	private ArtifactSpec readPersistedResolvedArtifact() throws IOException {
		if (!settings.usesLatestRelease()) return null;
		Path state = settings.releaseStateFile();
		if (!Files.exists(state, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(state, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control release metadata is invalid");
		}
		byte[] bytes;
		try (FileChannel channel = FileChannel.open(state, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)) {
			long size = channel.size();
			if (size < 1 || size > 2048L) throw new IOException("Control release metadata is invalid");
			bytes = new byte[(int) size];
			ByteBuffer buffer = ByteBuffer.wrap(bytes);
			while (buffer.hasRemaining() && channel.read(buffer) >= 0) { }
		}
		String[] lines = new String(bytes, java.nio.charset.StandardCharsets.US_ASCII).split("\\R");
		if (lines.length != 3) throw new IOException("Control release metadata is invalid");
		ArtifactSpec artifact;
		try {
			artifact = new ArtifactSpec(parseDownloadUri(lines[2]), lines[1], lines[0]);
		} catch (IllegalArgumentException e) {
			throw new IOException("Control release metadata is invalid", e);
		}
		if (!isOfficialReleaseArtifact(artifact)) {
			throw new IOException("Control release metadata is not an official release asset");
		}
		return artifact;
	}

	static int compareReleaseVersions(String left, String right) {
		if (left == null || right == null || !left.matches("v[0-9]+\\.[0-9]+\\.[0-9]+")
				|| !right.matches("v[0-9]+\\.[0-9]+\\.[0-9]+")) return 0;
		String[] leftParts = left.substring(1).split("\\.");
		String[] rightParts = right.substring(1).split("\\.");
		for (int i = 0; i < 3; i++) {
			int comparison = Long.compare(Long.parseLong(leftParts[i]), Long.parseLong(rightParts[i]));
			if (comparison != 0) return comparison;
		}
		return 0;
	}

	private static boolean isOfficialReleaseArtifact(ArtifactSpec artifact) {
		if (artifact == null || !artifact.version().matches("v[0-9]+\\.[0-9]+\\.[0-9]+")) return false;
		URI expected = URI.create("https://github.com/BenCodez/VotingPlugin-Control/releases/download/"
				+ artifact.version() + "/votingplugin-control.jar");
		return expected.equals(artifact.downloadUri());
	}

	private boolean activatePreparedArtifact(PreparedArtifact prepared) throws IOException {
		if (prepared.staged() == null) return false;
		try {
			boolean installed = Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS);
			String installedSha256 = installed ? sha256(settings.jarFile()) : null;
			if (installed != prepared.installed()
					|| !Objects.equals(installedSha256, prepared.installedSha256())) {
				throw new IOException("Control artifact changed while its replacement was staged");
			}
			activate(prepared.staged(), installed, installedSha256, prepared.target().sha256());
			return installed;
		} finally {
			Files.deleteIfExists(prepared.staged());
		}
	}

	private PreparedArtifact takePreparedArtifact() {
		synchronized (preparationLifecycle) {
			PreparedArtifact prepared = preparedArtifact;
			preparedArtifact = null;
			return prepared;
		}
	}

	private void discardPreparedArtifact() {
		deletePreparedArtifact(takePreparedArtifact());
	}

	private void deletePreparedArtifact(PreparedArtifact prepared) {
		if (prepared == null || prepared.staged() == null) return;
		try {
			Files.deleteIfExists(prepared.staged());
		} catch (IOException ignored) {
			// The contained staging file is best-effort cleanup during shutdown.
		}
	}

	private void activate(Path staged, boolean installed, String installedSha256, String candidateSha256)
			throws IOException {
		DurableFiles.forceFile(staged);
		if (installed) {
			persistRollbackState(installedSha256, candidateSha256);
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

	private void persistRollbackState(String previousSha256, String candidateSha256) throws IOException {
		DurableFiles.deleteIfExists(settings.healthCheckingFile());
		persistDigestState(settings.rollbackPendingFile(), previousSha256, candidateSha256, "rollback");
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

	private void recoverPersistedQuarantineState(String targetSha256) throws IOException {
		String[] lines = readDigestState(settings.quarantineFile(), "quarantine");
		if (lines == null) return;
		String previousSha256 = lines[0];
		String candidateSha256 = lines[1];
		if (!targetSha256.equals(candidateSha256)) {
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

	/** Reads a matching quarantine without clearing state still owned by the active manager. */
	private void loadPersistedQuarantineStateForPreparation(String targetSha256) throws IOException {
		String[] lines = readDigestState(settings.quarantineFile(), "quarantine");
		if (lines == null || !targetSha256.equals(lines[1])) return;
		if (!Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)
				|| !lines[0].equals(sha256(settings.jarFile()))) {
			throw new IOException("Active Control artifact does not match quarantined rollback state");
		}
		trustedRollbackSha256 = lines[0];
		quarantinedSha256 = lines[1];
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
		DurableFiles.deleteIfExists(settings.rollbackPendingFile());
		DurableFiles.deleteIfExists(settings.healthCheckingFile());
	}

	private void clearPersistedQuarantineState() throws IOException {
		DurableFiles.deleteIfExists(settings.quarantineFile());
	}

	private void installLocalAutoEnrollment() throws IOException, InterruptedException {
		PendingAutoEnrollment enrollment = localAutoEnrollment;
		if (enrollment == null) return;
		requireTrustedInstalledArtifact();
		verifierInstaller.install(settings, settings.jarFile(), enrollment.nodeId(), enrollment.verifier(),
				Duration.ofSeconds(Math.min(30, settings.startupTimeoutSeconds())));
		ControlCredentialFile.completeAutoEnrollment(enrollment);
		localAutoEnrollment = null;
		logger.accept("Automatically enrolled local node " + enrollment.nodeId());
	}

	/**
	 * Proves a backend route reaches this hosted launch, then optionally installs
	 * its verifier. The raw credential never reaches this process.
	 */
	public CompletableFuture<Boolean> installNodeVerifier(String nodeId, String verifier, String endpoint) {
		if (nodeId == null || !nodeId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")
				|| verifier == null || (!verifier.isEmpty() && !verifier.matches("[0-9a-f]{64}"))) {
			return CompletableFuture.completedFuture(false);
		}
		URI requestedEndpoint = hostedBackendEndpoint(endpoint);
		if (requestedEndpoint == null) return CompletableFuture.completedFuture(false);
		String key = nodeId + ':' + verifier + ':' + requestedEndpoint;
		return enrollmentTasks.computeIfAbsent(key, ignored -> {
			CompletableFuture<Boolean> result = new CompletableFuture<>();
			try {
				executor.execute(() -> {
					try {
						String launchId = healthyLaunchId;
						if (closed || (status != Status.RUNNING && status != Status.ROLLED_BACK)
								|| launchId == null
								|| !Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
							result.complete(false);
							return;
						}
						if (!healthProbe.isHealthy(requestedEndpoint, Duration.ofSeconds(2), launchId)) {
							result.complete(false);
							return;
						}
						if (verifier.isEmpty()) {
							result.complete(true);
							return;
						}
						requireTrustedInstalledArtifact();
						verifierInstaller.install(settings, settings.jarFile(), nodeId, verifier,
								Duration.ofSeconds(Math.min(30, settings.startupTimeoutSeconds())));
						result.complete(true);
					} catch (Exception failure) {
						if (failure instanceof InterruptedException) Thread.currentThread().interrupt();
						result.complete(false);
					} finally {
						enrollmentTasks.remove(key, result);
					}
				});
			} catch (RuntimeException failure) {
				result.complete(false);
				enrollmentTasks.remove(key, result);
			}
			return result;
		});
	}

	private void requireTrustedInstalledArtifact() throws IOException {
		if (!Files.isRegularFile(settings.jarFile(), LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control artifact is unavailable");
		}
		String installed = sha256(settings.jarFile());
		ArtifactSpec target = trustedTargetArtifact;
		if ((target == null || !target.sha256().equals(installed)) && !installed.equals(trustedRollbackSha256)) {
			throw new IOException("Control artifact no longer matches a trusted digest");
		}
	}

	private LaunchedProcess launch(Path artifact) throws IOException {
		status = Status.STARTING_PROCESS;
		healthyLaunchId = null;
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
			healthyLaunchId = null;
			status = Status.FAILED;
			logger.accept("Control exited unexpectedly; a bounded restart will be attempted");
			scheduleRetry();
		});
	}

	private void scheduleLatestUpdateCheck(ManagedProcess process) {
		if (closed || !settings.usesLatestRelease() || !settings.autoUpdate()) return;
		Future<?> previous = updateCheckTask;
		if (previous != null && !previous.isDone()) previous.cancel(false);
		try {
			updateCheckTask = executor.schedule(() -> checkForLatestUpdate(process),
					LATEST_UPDATE_INTERVAL_SECONDS, TimeUnit.SECONDS);
		} catch (RuntimeException ignored) {
			// Executor shutdown raced with the successful launch.
		}
	}

	void checkForLatestUpdate(ManagedProcess process) {
		Future<?> scheduled = updateCheckTask;
		updateCheckTask = null;
		if (scheduled != null && !scheduled.isDone()) scheduled.cancel(false);
		if (closed || managedProcess != process || !process.isAlive()) return;
		PreparedArtifact prepared = null;
		try {
			ArtifactSpec target = resolveLatestArtifact(false);
			recoverPersistedQuarantineState(target.sha256());
			String installed = sha256(settings.jarFile());
			ArtifactSpec persisted = readPersistedResolvedArtifact();
			if (target.sha256().equals(installed) || target.sha256().equals(quarantinedSha256)) {
				scheduleLatestUpdateCheck(process);
				return;
			}
			if (persisted != null && persisted.sha256().equals(installed)
					&& compareReleaseVersions(target.version(), persisted.version()) <= 0) {
				logger.accept("Ignoring GitHub release metadata that is not newer than the running Control version");
				scheduleLatestUpdateCheck(process);
				return;
			}
			prepared = stageArtifact(target);
			if (prepared.staged() == null) {
				status = Status.RUNNING;
				scheduleLatestUpdateCheck(process);
				return;
			}
			synchronized (processLifecycle) {
				if (closed || managedProcess != process || !process.isAlive()) {
					deletePreparedArtifact(prepared);
					return;
				}
				managedProcess = null;
				healthyLaunchId = null;
			}
			try {
				stopProcess(process, true);
			} catch (RuntimeException stopFailure) {
				synchronized (processLifecycle) {
					if (!closed && process.isAlive() && managedProcess == null) managedProcess = process;
				}
				deletePreparedArtifact(prepared);
				failWhileProcessIsAlive(process);
				return;
			}
			synchronized (preparationLifecycle) {
				if (closed) {
					deletePreparedArtifact(prepared);
					return;
				}
				preparedArtifact = prepared;
			}
			logger.accept("A newer Control release was verified; restarting the hosted service");
			status = Status.STARTING;
			runOnce();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			deletePreparedArtifact(prepared);
			if (!closed && process.isAlive()) {
				status = Status.RUNNING;
				scheduleLatestUpdateCheck(process);
			}
		} catch (Exception e) {
			deletePreparedArtifact(prepared);
			if (!closed && process.isAlive()) {
				status = Status.RUNNING;
				logger.accept("Latest-release check failed; the current Control service remains running: "
						+ safeFailureMessage(e));
				scheduleLatestUpdateCheck(process);
			}
		}
	}

	private void fail(Throwable failure) {
		healthyLaunchId = null;
		status = Status.FAILED;
		ManagedProcess process = managedProcess;
		if (process != null && process.isAlive()) {
			failWhileProcessIsAlive(process);
			return;
		}
		if (process != null) clearManagedProcess(process);
		logger.accept("Control could not be provisioned or started; VotingPlugin remains unaffected and will retry: "
				+ safeFailureMessage(failure));
		scheduleRetry();
	}

	private static String safeFailureMessage(Throwable failure) {
		String message = failure == null ? null : failure.getMessage();
		if (message == null || message.isBlank()) {
			return failure == null ? "unknown failure" : failure.getClass().getSimpleName();
		}
		String safe = message.replaceAll("[\\r\\n\\t\\p{Cntrl}]", " ").trim();
		return safe.length() <= 240 ? safe : safe.substring(0, 240);
	}

	private void failWhileProcessIsAlive(ManagedProcess process) {
		healthyLaunchId = null;
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
			healthyLaunchId = null;
			status = Status.STOPPED;
			process = managedProcess;
		}
		Future<?> task = activeTask;
		if (task != null) {
			task.cancel(true);
		}
		Future<?> updateTask = updateCheckTask;
		if (updateTask != null) updateTask.cancel(true);
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
			discardPreparedArtifact();
			executor.shutdownNow();
			enrollmentTasks.values().forEach(result -> result.complete(false));
			enrollmentTasks.clear();
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
			DurableFiles.forceMoveDirectories(source, target);
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

	/** True only for direct HTTP to this hosted listener on the same node. */
	public static boolean isDirectLocalEndpoint(String configuredEndpoint, HostConfiguration hosted) {
		if (hosted == null || !hosted.enabled() || configuredEndpoint == null) return false;
		try {
			URI endpoint = URI.create(configuredEndpoint.trim());
			String host = normalizeHostLiteral(endpoint.getHost());
			int port = endpoint.getPort() < 0 ? 80 : endpoint.getPort();
			String path = endpoint.getPath();
			boolean loopback = isLoopbackHost(host);
			String listenerHost = normalizeHostLiteral(hosted.host());
			boolean configuredListener = host != null && host.equalsIgnoreCase(listenerHost);
			boolean listenerLoopback = isLoopbackHost(listenerHost);
			boolean listenerWildcard = "0.0.0.0".equals(listenerHost) || "::".equals(listenerHost)
					|| "0:0:0:0:0:0:0:0".equals(listenerHost);
			return "http".equalsIgnoreCase(endpoint.getScheme())
					&& (configuredListener || (loopback && (listenerLoopback || listenerWildcard)))
					&& port == hosted.port()
					&& endpoint.getUserInfo() == null && endpoint.getQuery() == null && endpoint.getFragment() == null
					&& (path == null || path.isEmpty() || "/".equals(path));
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	/**
	 * Accepts a backend route only when it can address this listener. A subsequent
	 * launch-id health probe proves that the route reaches this exact hosted process.
	 */
	private URI hostedBackendEndpoint(String configuredEndpoint) {
		if (configuredEndpoint == null || configuredEndpoint.isBlank()) return null;
		try {
			URI endpoint = URI.create(configuredEndpoint.trim());
			String host = normalizeHostLiteral(endpoint.getHost());
			String listenerHost = normalizeHostLiteral(settings.host());
			int port = endpoint.getPort() < 0 ? 80 : endpoint.getPort();
			String path = endpoint.getPath();
			boolean listenerWildcard = "0.0.0.0".equals(listenerHost) || "::".equals(listenerHost)
					|| "0:0:0:0:0:0:0:0".equals(listenerHost);
			if (!"http".equalsIgnoreCase(endpoint.getScheme()) || host.isBlank() || isLoopbackHost(host)
					|| (!listenerWildcard && !host.equalsIgnoreCase(listenerHost)) || port != settings.port()
					|| endpoint.getUserInfo() != null || endpoint.getQuery() != null || endpoint.getFragment() != null
					|| (path != null && !path.isEmpty() && !"/".equals(path))) {
				return null;
			}
			return endpoint;
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	private static String normalizeHostLiteral(String host) {
		if (host == null) return "";
		String normalized = host.trim();
		if (normalized.length() >= 2 && normalized.charAt(0) == '['
				&& normalized.charAt(normalized.length() - 1) == ']') {
			return normalized.substring(1, normalized.length() - 1);
		}
		return normalized;
	}

	private static boolean isLoopbackHost(String host) {
		return "localhost".equalsIgnoreCase(host) || host.startsWith("127.")
				|| "::1".equalsIgnoreCase(host) || "0:0:0:0:0:0:0:1".equalsIgnoreCase(host);
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

	/** Platform configuration for the separate hosted Control process. */
	public record HostConfiguration(boolean enabled, boolean autoDownload, boolean autoUpdate, String downloadUrl,
			String sha256, String jarFile, String dataDirectory, String host, int port, int startupTimeoutSeconds,
			int downloadTimeoutSeconds) { }

	private record PreparedArtifact(Path staged, boolean installed, String installedSha256, ArtifactSpec target) { }

	record ArtifactSpec(URI downloadUri, String sha256, String version) {
		ArtifactSpec {
			sha256 = sha256 == null ? "" : sha256.trim().toLowerCase(Locale.ROOT);
			version = version == null ? "" : version.trim();
			if (!sha256.matches("[0-9a-f]{64}")) {
				throw new IllegalArgumentException("Control release SHA-256 is invalid");
			}
			if (!version.isEmpty() && !version.matches("v(?:0|[1-9][0-9]{0,8})\\.(?:0|[1-9][0-9]{0,8})\\.(?:0|[1-9][0-9]{0,8})")) {
				throw new IllegalArgumentException("Control release version is invalid");
			}
			if (!version.isEmpty() && downloadUri == null) {
				throw new IllegalArgumentException("Control release download URL is invalid");
			}
		}
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
			if (!sha256.isEmpty() && !sha256.matches("[0-9a-f]{64}")) {
				throw new IllegalArgumentException("Control hosted SHA-256 pin must contain 64 hexadecimal characters");
			}
			if (downloadUri != null && sha256.isEmpty()) {
				throw new IllegalArgumentException("Control hosted DownloadUrl and Sha256 must be configured together");
			}
			if (!sha256.isEmpty() && (autoDownload || autoUpdate) && downloadUri == null) {
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
			parseEndpoint(host, port);
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

		Path releaseStateFile() {
			return jarFile.resolveSibling(jarFile.getFileName() + ".release");
		}

		boolean usesLatestRelease() {
			return sha256.isEmpty() && downloadUri == null;
		}

		ArtifactSpec pinnedArtifact() {
			return sha256.isEmpty() ? null : new ArtifactSpec(downloadUri, sha256, "");
		}

		URI endpoint() {
			return parseEndpoint(host, port);
		}

		private static URI parseEndpoint(String host, int port) {
			try {
				URI endpoint = new URI("http", null, host, port, "/", null, null);
				if (endpoint.getHost() == null) {
					throw new IllegalArgumentException("Control hosted endpoint is invalid");
				}
				return endpoint;
			} catch (Exception e) {
				throw new IllegalArgumentException("Control hosted endpoint is invalid");
			}
		}
	}

	interface ArtifactDownloader {
		void download(URI source, Path target, long maximumBytes, Duration timeout)
				throws IOException, InterruptedException;
	}

	interface ReleaseResolver {
		ArtifactSpec resolve(Duration timeout) throws IOException, InterruptedException;
	}

	interface ProcessLauncher {
		ManagedProcess launch(Settings settings, Path artifact, String launchId) throws IOException;
	}

	interface VerifierInstaller {
		void install(Settings settings, Path artifact, String nodeId, String verifier, Duration timeout)
				throws IOException, InterruptedException;
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

	static final class JdkGithubReleaseResolver implements ReleaseResolver {
		private final HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10))
				.followRedirects(HttpClient.Redirect.NEVER).build();

		@Override
		public ArtifactSpec resolve(Duration timeout) throws IOException, InterruptedException {
			HttpRequest request = HttpRequest.newBuilder(LATEST_RELEASE_API).timeout(timeout)
					.header("Accept", "application/vnd.github+json")
					.header("X-GitHub-Api-Version", "2022-11-28")
					.header("User-Agent", "VotingPlugin-Control-Host").GET().build();
			HttpResponse<byte[]> response = client.send(request,
					new BoundedHttpBodyHandler(MAX_RELEASE_METADATA_BYTES, timeout));
			if (response.statusCode() != 200) {
				throw new IOException("GitHub latest-release lookup returned HTTP " + response.statusCode());
			}
			return parseLatestRelease(response.body());
		}

		static ArtifactSpec parseLatestRelease(byte[] body) throws IOException {
			try {
				JsonElement parsed = JsonParser.parseString(
						new String(body, java.nio.charset.StandardCharsets.UTF_8));
				if (!parsed.isJsonObject()) throw new IOException("GitHub latest-release metadata is invalid");
				JsonObject release = parsed.getAsJsonObject();
				if (booleanValue(release, "draft") || booleanValue(release, "prerelease")) {
					throw new IOException("GitHub latest release is not stable");
				}
				String version = stringValue(release, "tag_name");
				JsonObject jarAsset = null;
				if (release.has("assets") && release.get("assets").isJsonArray()) {
					for (JsonElement element : release.getAsJsonArray("assets")) {
						if (!element.isJsonObject()) continue;
						JsonObject asset = element.getAsJsonObject();
						if ("votingplugin-control.jar".equals(stringValue(asset, "name"))) {
							if (jarAsset != null) throw new IOException("GitHub release contains duplicate Control JAR assets");
							jarAsset = asset;
						}
					}
				}
				if (jarAsset == null) throw new IOException("GitHub latest release has no Control JAR asset");
				String digest = stringValue(jarAsset, "digest").toLowerCase(Locale.ROOT);
				if (!digest.matches("sha256:[0-9a-f]{64}")) {
					throw new IOException("GitHub latest release has no valid SHA-256 digest");
				}
				ArtifactSpec artifact = new ArtifactSpec(
						parseDownloadUri(stringValue(jarAsset, "browser_download_url")),
						digest.substring("sha256:".length()), version);
				if (!isOfficialReleaseArtifact(artifact)) {
					throw new IOException("GitHub latest release asset URL is invalid");
				}
				return artifact;
			} catch (IOException e) {
				throw e;
			} catch (RuntimeException e) {
				throw new IOException("GitHub latest-release metadata is invalid", e);
			}
		}

		private static String stringValue(JsonObject object, String name) throws IOException {
			if (!object.has(name) || !object.get(name).isJsonPrimitive()
					|| !object.getAsJsonPrimitive(name).isString()) {
				throw new IOException("GitHub latest-release metadata is missing " + name);
			}
			return object.get(name).getAsString();
		}

		private static boolean booleanValue(JsonObject object, String name) throws IOException {
			if (!object.has(name) || !object.get(name).isJsonPrimitive()
					|| !object.getAsJsonPrimitive(name).isBoolean()) {
				throw new IOException("GitHub latest-release metadata is missing " + name);
			}
			return object.get(name).getAsBoolean();
		}
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

	private static final class JdkVerifierInstaller implements VerifierInstaller {
		@Override
		public void install(Settings settings, Path artifact, String nodeId, String verifier, Duration timeout)
				throws IOException, InterruptedException {
			Path java = Path.of(System.getProperty("java.home"), "bin", isWindows() ? "java.exe" : "java");
			ProcessBuilder builder = new ProcessBuilder(java.toString(), "-jar", artifact.toString(),
					"enroll-verifier", nodeId, verifier, settings.dataDirectory().toString());
			builder.directory(artifact.getParent().toFile());
			builder.redirectOutput(ProcessBuilder.Redirect.DISCARD);
			builder.redirectError(ProcessBuilder.Redirect.DISCARD);
			copySafeEnvironment(builder);
			Process process = builder.start();
			boolean exited;
			try {
				exited = process.waitFor(Math.max(1L, timeout.toMillis()), TimeUnit.MILLISECONDS);
			} catch (InterruptedException e) {
				process.destroyForcibly();
				throw e;
			}
			if (!exited) {
				process.destroy();
				if (!process.waitFor(2, TimeUnit.SECONDS)) process.destroyForcibly();
				throw new IOException("Control verifier enrollment timed out");
			}
			if (process.exitValue() != 0) throw new IOException("Control verifier enrollment failed");
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
			copySafeEnvironment(builder);
			var environment = builder.environment();
			environment.put("CONTROL_HOST", settings.host());
			environment.put("CONTROL_PORT", Integer.toString(settings.port()));
			environment.put("CONTROL_DATA_DIR", settings.dataDirectory().toString());
			environment.put("CONTROL_LAUNCH_ID", launchId);
			environment.put("CONTROL_PARENT_PID", Long.toString(ProcessHandle.current().pid()));
			return new JdkManagedProcess(builder.start());
		}

		private static boolean isWindows() {
			return HostedControlManager.isWindows();
		}
	}

	private static void copySafeEnvironment(ProcessBuilder builder) {
		var environment = builder.environment();
		environment.clear();
		for (String safe : Set.of("LANG", "LC_ALL", "SYSTEMROOT", "WINDIR", "TEMP", "TMP")) {
			String value = System.getenv(safe);
			if (value != null) environment.put(safe, value);
		}
	}

	private static boolean isWindows() {
		return DurableFiles.isWindowsName(System.getProperty("os.name", ""));
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
