package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.charset.CodingErrorAction;
import java.nio.channels.FileChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.HexFormat;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.BooleanSupplier;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import com.bencodez.votingplugin.util.DurableFiles;

/**
 * Bounded, pull-only staging for the optional {@code plugin.deploy.v1} capability.
 * It deliberately never reloads a plugin or starts a server.  The small on-disk
 * marker makes a leased task idempotent after a lost result acknowledgement.
 */
public final class PluginDeploymentService {
	public static final String CAPABILITY = "plugin.deploy.v1";
	public static final long MAX_ARTIFACT_BYTES = 64L * 1024L * 1024L;
	private static final int BUFFER_BYTES = 32 * 1024;
	private static final int MAX_JAR_ENTRIES = 10_000;
	private static final long MAX_INSPECTED_UNCOMPRESSED_BYTES = 256L * 1024L * 1024L;
	private static final int MAX_PLUGIN_YML_BYTES = 64 * 1024;
	private static final String MARKER = ".control-deployment";
	private static final ScheduledExecutorService BODY_DEADLINE_EXECUTOR = Executors.newSingleThreadScheduledExecutor(r -> {
		Thread thread = new Thread(r, "VotingPlugin-control-artifact-deadline");
		thread.setDaemon(true);
		return thread;
	});

	private final Path target;
	private final Path root;
	private final Path marker;
	private final Path installedBackendJar;
	private final boolean replaceExisting;
	private final AtomicBoolean staging = new AtomicBoolean();
	private final AtomicReference<InputStream> activeResponse = new AtomicReference<>();

	private PluginDeploymentService(Path target, Path installedBackendJar, boolean replaceExisting) throws IOException {
		this.target = target.toAbsolutePath().normalize();
		this.installedBackendJar = installedBackendJar == null ? null : installedBackendJar.toAbsolutePath().normalize();
		this.replaceExisting = replaceExisting;
		Path parent = this.target.getParent();
		if (parent == null) throw new IOException("deployment target has no parent");
		Files.createDirectories(parent);
		this.root = parent.toRealPath(LinkOption.NOFOLLOW_LINKS);
		if (!Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(root)) {
			throw new IOException("deployment staging directory is unsafe");
		}
		if (!this.target.startsWith(root) || Files.isSymbolicLink(this.target)) {
			throw new IOException("deployment target escapes staging directory");
		}
		if (Files.exists(this.target, LinkOption.NOFOLLOW_LINKS)
				&& !Files.isRegularFile(this.target, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("deployment target is not a regular file");
		}
		this.marker = root.resolve(this.target.getFileName() + MARKER);
	}

	/** Bukkit consumes an update only when its file name matches the currently loaded plugin JAR. */
	public static PluginDeploymentService backend(Path updateDirectory, Path currentPluginJar) throws IOException {
		if (updateDirectory == null) throw new IOException("Bukkit update folder is unavailable");
		if (currentPluginJar == null || currentPluginJar.getFileName() == null
				|| !currentPluginJar.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".jar")) {
			throw new IOException("Bukkit plugin JAR is unavailable");
		}
		return new PluginDeploymentService(updateDirectory.resolve(currentPluginJar.getFileName().toString()),
				currentPluginJar, false);
	}

	/** Proxies atomically replace their discovered plugin JAR only after a durable backup. */
	public static PluginDeploymentService proxy(Path currentPluginJar) throws IOException {
		if (currentPluginJar == null || !currentPluginJar.getFileName().toString().endsWith(".jar")) {
			throw new IOException("proxy plugin JAR is unavailable");
		}
		if (!Files.isRegularFile(currentPluginJar, LinkOption.NOFOLLOW_LINKS)
				|| Files.isSymbolicLink(currentPluginJar)) throw new IOException("proxy plugin JAR is unsafe");
		return new PluginDeploymentService(currentPluginJar, null, true);
	}

	public boolean isStaging() { return staging.get(); }

	/** Unblocks an in-progress response read during connector shutdown. */
	public void cancel() {
		InputStream response = activeResponse.getAndSet(null);
		if (response != null) {
			try { response.close(); } catch (IOException ignored) { /* Shutdown is already in progress. */ }
		}
	}

	public Result deploy(Task task, URI endpoint, boolean directLocalHosted, String nodeId, UUID sessionId,
			String credential, HttpClient http, Duration timeout, BooleanSupplier active) {
		if (!staging.compareAndSet(false, true)) return Result.failure("DEPLOYMENT_FAILED", "Another deployment is still staging");
		try {
			validate(task);
			if (!credentialEndpointAllowed(endpoint, directLocalHosted)) {
				return Result.failure("INSECURE_ENDPOINT",
						"Verified update staging requires HTTPS unless Control is hosted directly on this node");
			}
			if (!active.getAsBoolean()) return Result.failure("CANCELLED", "Deployment was cancelled before download");
			if (alreadyStaged(task)) return Result.restartRequired();
			URI artifact = endpoint.resolve("/api/v1/nodes/" + nodeId + "/deployments/" + task.deploymentId()
					+ "/artifact");
			HttpRequest request = HttpRequest.newBuilder(artifact).timeout(timeout)
					.header("Authorization", "Bearer " + credential)
					.header("X-Node-Session", sessionId.toString())
					.header("X-Deployment-Attempt", task.attemptId().toString()).GET().build();
			HttpResponse<InputStream> response = http.send(request, HttpResponse.BodyHandlers.ofInputStream());
			InputStream body = response.body();
			activeResponse.set(body);
			ScheduledFuture<?> bodyDeadline = BODY_DEADLINE_EXECUTOR.schedule(() -> {
				try { body.close(); } catch (IOException ignored) { /* The staging operation reports the timeout. */ }
			}, timeout.toMillis(), TimeUnit.MILLISECONDS);
			try {
				if (response.statusCode() != 200) {
					body.close();
					return Result.failure("DOWNLOAD_FAILED", "Artifact download was rejected");
				}
				String contentLength = response.headers().firstValue("Content-Length").orElse(null);
				if (contentLength != null && (!contentLength.matches("[0-9]{1,9}")
						|| Long.parseLong(contentLength) != task.size())) {
					body.close();
					return Result.failure("SIZE_MISMATCH", "Artifact size did not match the deployment task");
				}
				if (!active.getAsBoolean()) {
					body.close();
					return Result.failure("CANCELLED", "Deployment was cancelled before download");
				}
				try (body) {
					return stage(task, body, active);
				} catch (IOException failure) {
					if (!active.getAsBoolean()) return Result.failure("CANCELLED", "Deployment was cancelled before staging");
					return Result.failure("STAGING_FAILED", "Artifact could not be verified or staged on this node");
				}
			} finally {
				bodyDeadline.cancel(false);
				activeResponse.compareAndSet(body, null);
			}
		} catch (InterruptedException failure) {
			Thread.currentThread().interrupt();
			return Result.failure("CANCELLED", "Deployment download was interrupted");
		} catch (IOException | RuntimeException failure) {
			return Result.failure("DOWNLOAD_FAILED", "Artifact download failed");
		} finally {
			staging.set(false);
		}
	}

	/** Package-visible for deterministic artifact-validation tests. */
	Result stage(Task task, InputStream body, BooleanSupplier active) throws IOException {
		validate(task);
		if (!active.getAsBoolean()) return Result.failure("CANCELLED", "Deployment was cancelled before staging");
		if (alreadyStaged(task)) return Result.restartRequired();
		activeResponse.compareAndSet(null, body);
		Path temporary = Files.createTempFile(root, target.getFileName().toString() + ".", ".download");
		Activation activation = null;
		try {
			MessageDigest digest = sha256();
			long written = copyExact(body, temporary, task.size(), digest, active);
			force(temporary);
			if (written != task.size()) return Result.failure("SIZE_MISMATCH", "Artifact size did not match the deployment task");
			if (!HexFormat.of().formatHex(digest.digest()).equals(task.sha256())) {
				return Result.failure("HASH_MISMATCH", "Artifact digest did not match the deployment task");
			}
			inspectJar(temporary);
			if (!active.getAsBoolean()) return Result.failure("CANCELLED", "Deployment was cancelled before staging");
			activation = new Activation();
			prepareProxyBackup();
			writeMarker(task, activation);
			activate(temporary, activation);
			activation.discard();
			return Result.restartRequired();
		} catch (CancelledDeploymentException failure) {
			return Result.failure("CANCELLED", "Deployment was cancelled before staging");
		} catch (InvalidArtifactException failure) {
			return Result.failure("INVALID_ARTIFACT", "Artifact is not a bounded VotingPlugin JAR");
		} catch (IOException failure) {
			if (activation != null) {
				try { activation.rollback(); }
				catch (IOException rollbackFailure) { failure.addSuppressed(rollbackFailure); }
			}
			if (!active.getAsBoolean()) return Result.failure("CANCELLED", "Deployment was cancelled before staging");
			throw failure;
		} finally {
			activeResponse.compareAndSet(body, null);
			Files.deleteIfExists(temporary);
		}
	}

	private long copyExact(InputStream input, Path temporary, long expected, MessageDigest digest,
			BooleanSupplier active) throws IOException {
		long total = 0;
		byte[] buffer = new byte[BUFFER_BYTES];
		try (var output = Files.newOutputStream(temporary, StandardOpenOption.TRUNCATE_EXISTING)) {
			for (;;) {
				if (!active.getAsBoolean()) throw new CancelledDeploymentException();
				int read = input.read(buffer);
				if (read == -1) break;
				total += read;
				if (total > expected || total > MAX_ARTIFACT_BYTES) throw new InvalidArtifactException();
				digest.update(buffer, 0, read);
				output.write(buffer, 0, read);
			}
		}
		return total;
	}

	private void inspectJar(Path artifact) throws IOException {
		try (JarFile jar = new JarFile(artifact.toFile(), false)) {
			int entries = 0;
			long uncompressed = 0;
			JarEntry pluginYml = null;
			var iterator = jar.entries();
			while (iterator.hasMoreElements()) {
				JarEntry entry = iterator.nextElement();
				if (++entries > MAX_JAR_ENTRIES || unsafeEntry(entry.getName())) throw new InvalidArtifactException();
				long size = entry.getSize();
				long compressed = entry.getCompressedSize();
				if (size < 0 || compressed < 0 || compressed > 0 && size > compressed * 200L
						|| (uncompressed += size) > MAX_INSPECTED_UNCOMPRESSED_BYTES) {
					throw new InvalidArtifactException();
				}
				if ("plugin.yml".equals(entry.getName())) {
					if (pluginYml != null || size > MAX_PLUGIN_YML_BYTES) throw new InvalidArtifactException();
					pluginYml = entry;
				}
			}
			if (pluginYml == null || !isVotingPluginYml(jar, pluginYml)) throw new InvalidArtifactException();
		} catch (java.util.zip.ZipException failure) {
			throw new InvalidArtifactException();
		}
	}

	private static boolean unsafeEntry(String name) {
		if (name == null || name.isEmpty() || name.length() > 512 || name.startsWith("/")
				|| name.startsWith("\\") || name.contains("\\") || name.indexOf('\0') >= 0) return true;
		for (String component : name.split("/", -1)) {
			if (component.equals(".") || component.equals("..")) return true;
		}
		return false;
	}

	private static boolean isVotingPluginYml(JarFile jar, JarEntry entry) throws IOException {
		byte[] bytes;
		try (InputStream input = jar.getInputStream(entry)) { bytes = input.readNBytes(MAX_PLUGIN_YML_BYTES + 1); }
		if (bytes.length > MAX_PLUGIN_YML_BYTES) return false;
		String yml;
		try {
			yml = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(java.nio.ByteBuffer.wrap(bytes)).toString();
		} catch (java.nio.charset.CharacterCodingException failure) {
			return false;
		}
		boolean found = false;
		for (String line : yml.split("\\R")) {
			if (!line.startsWith("name:")) continue;
			if (found) return false;
			found = true;
			String value = line.substring(5).trim();
			int comment = value.indexOf('#');
			if (comment >= 0) value = value.substring(0, comment).trim();
			if (!"VotingPlugin".equals(unquote(value))) return false;
		}
		return found;
	}

	private static String unquote(String value) {
		return value.length() >= 2 && ((value.startsWith("\"") && value.endsWith("\""))
				|| (value.startsWith("'") && value.endsWith("'"))) ? value.substring(1, value.length() - 1) : value;
	}

	private void prepareProxyBackup() throws IOException {
		if (replaceExisting) {
			if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) {
				throw new IOException("current proxy plugin JAR is unsafe");
			}
			Path backup = root.resolve(target.getFileName() + ".control-backup");
			if (Files.exists(backup, LinkOption.NOFOLLOW_LINKS)
					&& (!Files.isRegularFile(backup, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(backup))) {
				throw new IOException("proxy plugin backup is unsafe");
			}
			Path backupTemp = Files.createTempFile(root, target.getFileName().toString() + ".", ".backup");
			try {
				Files.copy(target, backupTemp, StandardCopyOption.REPLACE_EXISTING);
				force(backupTemp);
				move(backupTemp, backup);
				forceDirectory(root);
			} finally { Files.deleteIfExists(backupTemp); }
		}
	}

	private void activate(Path temporary, Activation activation) throws IOException {
		move(temporary, target);
		activation.published = true;
		force(target);
		forceDirectory(root);
	}

	/** Holds a private copy until the durable idempotency marker has been published. */
	private final class Activation {
		private final Path previous;
		private boolean published;
		private boolean markerPublished;

		private Activation() throws IOException {
			if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
				previous = null;
				return;
			}
			if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) {
				throw new IOException("deployment target is unsafe");
			}
			previous = Files.createTempFile(root, target.getFileName().toString() + ".", ".rollback");
			Files.copy(target, previous, StandardCopyOption.REPLACE_EXISTING);
			force(previous);
		}

		private void rollback() throws IOException {
			if (markerPublished) DurableFiles.deleteIfExists(marker);
			if (!published) {
				discard();
				return;
			}
			if (previous == null) {
				if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) {
					throw new IOException("deployed target cannot be safely removed");
				}
				Files.delete(target);
			} else {
				move(previous, target);
				force(target);
			}
			forceDirectory(root);
			discard();
		}

		private void discard() {
			if (previous == null) return;
			try { Files.deleteIfExists(previous); }
			catch (IOException ignored) { /* A private stale rollback copy is safer than a false deployment result. */ }
		}
	}

	private void writeMarker(Task task) throws IOException {
		writeMarker(task, null);
	}

	private void writeMarker(Task task, Activation activation) throws IOException {
		Path temporary = Files.createTempFile(root, target.getFileName().toString() + ".", ".marker");
		try {
			Files.writeString(temporary, task.deploymentId() + "\n" + task.sha256() + "\n" + task.size() + "\n",
					StandardCharsets.US_ASCII, StandardOpenOption.TRUNCATE_EXISTING);
			force(temporary);
			move(temporary, marker);
			if (activation != null) activation.markerPublished = true;
			forceDirectory(root);
		} finally { Files.deleteIfExists(temporary); }
	}

	private boolean alreadyStaged(Task task) throws IOException {
		if (!Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(marker)
				|| Files.size(marker) > 256) return false;
		if (replaceExisting) {
			Path backup = root.resolve(target.getFileName() + ".control-backup");
			if (!Files.isRegularFile(backup, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(backup)) return false;
		}
		String[] fields = Files.readString(marker, StandardCharsets.US_ASCII).split("\\R", -1);
		// A lost acknowledgement can cause Control to issue a new deployment ID
		// for the same verified artifact after reconnect or restart. The marker
		// only proves a completed prior stage; the target is checked below by
		// exact size, digest and plugin identity before acknowledging the retry.
		if (fields.length < 3 || !task.sha256().equals(fields[1])
				|| !Long.toString(task.size()).equals(fields[2])) return false;
		if (!replaceExisting && !Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
			// Bukkit removes the staged update JAR after consuming it on restart. A
			// missing staging file alone is not proof: it may have been deleted or
			// quarantined. Confirm the installed backend JAR is the exact artifact.
			return installedBackendJar != null && fileMatches(installedBackendJar, task);
		}
		return targetMatches(task);
	}

	private boolean targetMatches(Task task) throws IOException {
		return fileMatches(target, task);
	}

	private boolean fileMatches(Path candidate, Task task) throws IOException {
		if (!Files.isRegularFile(candidate, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(candidate)
				|| Files.size(candidate) != task.size()) return false;
		MessageDigest digest = sha256();
		try (InputStream input = Files.newInputStream(candidate, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)) {
			byte[] bytes = new byte[BUFFER_BYTES];
			for (int read; (read = input.read(bytes)) != -1;) digest.update(bytes, 0, read);
		}
		if (!task.sha256().equals(HexFormat.of().formatHex(digest.digest()))) return false;
		try {
			inspectJar(candidate);
			return true;
		} catch (InvalidArtifactException invalid) {
			return false;
		}
	}

	private static void move(Path source, Path destination) throws IOException {
		try { Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING); }
		catch (AtomicMoveNotSupportedException ignored) { Files.move(source, destination, StandardCopyOption.REPLACE_EXISTING); }
	}

	private static void force(Path file) throws IOException {
		try (FileChannel channel = FileChannel.open(file, StandardOpenOption.WRITE)) { channel.force(true); }
	}

	private static void forceDirectory(Path directory) throws IOException {
		DurableFiles.forceDirectory(directory);
	}

	/** True when a deployment bearer credential may be sent to this Control endpoint. */
	public static boolean credentialEndpointAllowed(URI endpoint, boolean directLocalHosted) {
		if (endpoint == null) return false;
		if ("https".equalsIgnoreCase(endpoint.getScheme())) return true;
		return directLocalHosted && "http".equalsIgnoreCase(endpoint.getScheme())
				&& isLoopbackHost(endpoint.getHost());
	}

	private static boolean isLoopbackHost(String host) {
		if (host == null) return false;
		String normalized = host;
		if (normalized.length() >= 2 && normalized.charAt(0) == '['
				&& normalized.charAt(normalized.length() - 1) == ']') {
			normalized = normalized.substring(1, normalized.length() - 1);
		}
		if ("localhost".equalsIgnoreCase(normalized) || "::1".equalsIgnoreCase(normalized)
				|| "0:0:0:0:0:0:0:1".equalsIgnoreCase(normalized)) return true;
		String[] octets = normalized.split("\\.", -1);
		if (octets.length != 4 || !"127".equals(octets[0])) return false;
		for (int index = 1; index < octets.length; index++) {
			if (!octets[index].matches("[0-9]{1,3}") || Integer.parseInt(octets[index]) > 255) return false;
		}
		return true;
	}

	private static MessageDigest sha256() {
		try { return MessageDigest.getInstance("SHA-256"); }
		catch (java.security.NoSuchAlgorithmException failure) { throw new IllegalStateException("SHA-256 is unavailable", failure); }
	}

	private static void validate(Task task) {
		if (task == null || task.deploymentId() == null || task.attemptId() == null || task.artifactId() == null
				|| !task.artifactId().matches("[A-Za-z0-9][A-Za-z0-9._-]{0,127}") || task.sha256() == null
				|| !task.sha256().matches("[0-9a-fA-F]{64}") || task.size() < 1 || task.size() > MAX_ARTIFACT_BYTES) {
			throw new IllegalArgumentException("deployment task is invalid");
		}
	}

	public record Task(UUID deploymentId, String artifactId, String sha256, long size, UUID attemptId) {
		public Task { if (sha256 != null) sha256 = sha256.toLowerCase(Locale.ROOT); }
	}
	public record Result(boolean success, String code, String message) {
		static Result restartRequired() { return new Result(true, "RESTART_REQUIRED", "Plugin update staged; restart is required"); }
		static Result failure(String code, String message) { return new Result(false, code, message); }
	}
	private static final class InvalidArtifactException extends IOException { private static final long serialVersionUID = 1L; }
	private static final class CancelledDeploymentException extends IOException { private static final long serialVersionUID = 1L; }
}
