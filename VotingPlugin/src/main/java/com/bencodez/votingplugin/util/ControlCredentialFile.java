package com.bencodez.votingplugin.util;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.HexFormat;
import java.util.Set;
import java.util.regex.Pattern;

/** Symlink-safe, bounded credential loading and supervised enrollment state. */
public final class ControlCredentialFile {
	private static final int MAX_BYTES = 512;
	private static final int MAX_MARKER_BYTES = 256;
	private static final Pattern NODE_ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
	private static final Pattern VERIFIER = Pattern.compile("[0-9a-f]{64}");
	private static final Set<PosixFilePermission> OWNER_ONLY = Set.of(
			PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE);
	private static final SecureRandom RANDOM = new SecureRandom();

	private ControlCredentialFile() { }

	public static String read(Path rootDirectory, String configuredPath) throws IOException {
		String credential = readCredential(resolveAndCreate(rootDirectory, configuredPath));
		if (credential == null) throw invalid();
		return credential;
	}

	/** Inspects existing enrollment state without generating a new credential. */
	public static synchronized AutoEnrollmentInspection inspectAutoEnrollment(Path rootDirectory,
			String configuredPath) throws IOException {
		Path target = resolveAndCreate(rootDirectory, configuredPath);
		Path marker = marker(target);
		String credential = readCredential(target);
		PendingAutoEnrollment pending = readMarker(marker, target, configuredPath);
		if (credential != null && pending != null && !sha256Verifier(credential).equals(pending.verifier())) {
			DurableFiles.deleteIfExists(marker);
			pending = null;
		}
		if (credential == null) pending = null;
		return new AutoEnrollmentInspection(credential != null, pending);
	}

	/**
	 * Generates a credential only when the configured file is empty and retains a
	 * verifier-only marker until the supervising host acknowledges enrollment.
	 * Existing manually managed credentials are never replaced.
	 */
	public static synchronized PendingAutoEnrollment prepareAutoEnrollment(Path rootDirectory, String configuredPath,
			String nodeId) throws IOException {
		if (nodeId == null || !NODE_ID.matcher(nodeId).matches()) throw invalid();
		Path target = resolveAndCreate(rootDirectory, configuredPath);
		Path marker = marker(target);
		String credential = readCredential(target);
		PendingAutoEnrollment pending = readMarker(marker, target, configuredPath);
		if (credential != null && pending == null) return null;
		if (credential != null && !sha256Verifier(credential).equals(pending.verifier())) {
			DurableFiles.deleteIfExists(marker);
			return null;
		}
		if (credential != null && nodeId.equals(pending.nodeId())) return pending;

		String generated = token();
		String generatedVerifier = sha256Verifier(generated);
		PendingAutoEnrollment replacement = new PendingAutoEnrollment(target, configuredPath, nodeId,
				generatedVerifier);
		writeAtomically(marker, markerContents(replacement));
		writeAtomically(target, generated);
		return replacement;
	}

	/** Removes the pending marker only when it still describes the installed verifier. */
	public static synchronized void completeAutoEnrollment(PendingAutoEnrollment enrollment) throws IOException {
		if (enrollment == null) return;
		Path target = enrollment.credentialFile().toAbsolutePath().normalize();
		Path marker = marker(target);
		PendingAutoEnrollment current = readMarker(marker, target, enrollment.configuredPath());
		String credential = readCredential(target);
		if (current == null && credential != null && enrollment.verifier().equals(sha256Verifier(credential))) return;
		if (current == null || credential == null || !current.nodeId().equals(enrollment.nodeId())
				|| !current.verifier().equals(enrollment.verifier())
				|| !current.verifier().equals(sha256Verifier(credential))) {
			throw new IOException("Control automatic enrollment state changed");
		}
		DurableFiles.deleteIfExists(marker);
	}

	private static String readCredential(Path target) throws IOException {
		ByteBuffer bytes = ByteBuffer.allocate(MAX_BYTES + 1);
		try (SeekableByteChannel channel = Files.newByteChannel(target, StandardOpenOption.READ,
				LinkOption.NOFOLLOW_LINKS)) {
			while (bytes.hasRemaining() && channel.read(bytes) > 0) { }
		}
		if (bytes.position() == 0) return null;
		if (bytes.position() > MAX_BYTES) throw invalid();
		bytes.flip();
		String credential;
		try {
			credential = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(bytes).toString().trim();
		} catch (java.nio.charset.CharacterCodingException e) {
			throw invalid();
		}
		if (credential.isEmpty()) return null;
		if (credential.length() > MAX_BYTES || credential.indexOf('\r') >= 0
				|| credential.indexOf('\n') >= 0) throw invalid();
		return credential;
	}

	private static PendingAutoEnrollment readMarker(Path marker, Path credentialFile, String configuredPath)
			throws IOException {
		if (!Files.exists(marker, LinkOption.NOFOLLOW_LINKS)) return null;
		if (!Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(marker)) throw invalid();
		ByteBuffer bytes = ByteBuffer.allocate(MAX_MARKER_BYTES + 1);
		try (SeekableByteChannel channel = Files.newByteChannel(marker, StandardOpenOption.READ,
				LinkOption.NOFOLLOW_LINKS)) {
			while (bytes.hasRemaining() && channel.read(bytes) > 0) { }
		}
		if (bytes.position() > MAX_MARKER_BYTES) throw invalid();
		bytes.flip();
		String contents;
		try {
			contents = StandardCharsets.US_ASCII.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(bytes).toString();
		} catch (java.nio.charset.CharacterCodingException e) {
			throw invalid();
		}
		String[] lines = contents.split("\\n", -1);
		if (lines.length != 3 || contents.indexOf('\r') >= 0 || !"1".equals(lines[0])
				|| !NODE_ID.matcher(lines[1]).matches()
				|| !VERIFIER.matcher(lines[2]).matches()) throw invalid();
		restrictPermissions(marker);
		return new PendingAutoEnrollment(credentialFile, configuredPath, lines[1], lines[2]);
	}

	private static String markerContents(PendingAutoEnrollment enrollment) {
		return "1\n" + enrollment.nodeId() + "\n" + enrollment.verifier();
	}

	private static Path marker(Path credentialFile) {
		return credentialFile.resolveSibling(credentialFile.getFileName() + ".auto-enroll");
	}

	private static void writeAtomically(Path target, String value) throws IOException {
		Path temporary = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".temporary");
		try {
			restrictPermissions(temporary);
			Files.writeString(temporary, value, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING,
					StandardOpenOption.WRITE);
			DurableFiles.forceFile(temporary);
			try {
				Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException e) {
				throw new IOException("Atomic Control credential publication is unsupported", e);
			}
			DurableFiles.forceMoveDirectories(temporary, target);
		} finally {
			Files.deleteIfExists(temporary);
		}
	}

	private static Path resolveAndCreate(Path rootDirectory, String configuredPath) throws IOException {
		if (configuredPath == null || configuredPath.isBlank()) throw invalid();
		Path lexicalRoot = rootDirectory.toAbsolutePath().normalize();
		Path requested = lexicalRoot.resolve(configuredPath).normalize();
		if (!requested.startsWith(lexicalRoot) || requested.getParent() == null) throw invalid();

		Path realRoot = lexicalRoot.toRealPath();
		Path current = realRoot;
		for (Path component : lexicalRoot.relativize(requested.getParent())) {
			Path child = current.resolve(component.toString());
			if (!Files.exists(child, LinkOption.NOFOLLOW_LINKS)) {
				try {
					Files.createDirectory(child);
				} catch (java.nio.file.FileAlreadyExistsException ignored) {
					// Revalidate below in case another process created it.
				}
			}
			if (Files.isSymbolicLink(child) || !Files.isDirectory(child, LinkOption.NOFOLLOW_LINKS)) throw invalid();
			current = child.toRealPath();
			if (!current.startsWith(realRoot)) throw invalid();
		}

		Path target = current.resolve(requested.getFileName());
		if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
			try (SeekableByteChannel ignored = Files.newByteChannel(target,
					StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS)) {
				// Publish an empty placeholder for manual or supervised enrollment.
			} catch (java.nio.file.FileAlreadyExistsException ignored) {
				// Revalidate below in case another process created it.
			}
		}
		if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) throw invalid();
		restrictPermissions(target);
		return target;
	}

	private static void restrictPermissions(Path target) throws IOException {
		PosixFileAttributeView view = Files.getFileAttributeView(target, PosixFileAttributeView.class,
				LinkOption.NOFOLLOW_LINKS);
		if (view == null) return;
		try {
			view.setPermissions(OWNER_ONLY);
		} catch (UnsupportedOperationException ignored) {
			// Non-POSIX providers retain their platform-default access controls.
		}
	}

	private static String token() {
		byte[] bytes = new byte[32];
		RANDOM.nextBytes(bytes);
		return "vpctl_node_" + Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
	}

	/** Returns the non-secret verifier used to bind connector authentication to pending enrollment. */
	public static String sha256Verifier(String credential) {
		if (credential == null) throw new IllegalArgumentException("credential is required");
		try {
			return HexFormat.of().formatHex(
					MessageDigest.getInstance("SHA-256").digest(credential.getBytes(StandardCharsets.UTF_8)));
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable", e);
		}
	}

	private static IOException invalid() {
		return new IOException("Control credential file is missing or invalid");
	}

	/** Non-secret durable request for a supervising Control host to install. */
	public record PendingAutoEnrollment(Path credentialFile, String configuredPath, String nodeId, String verifier) { }

	public record AutoEnrollmentInspection(boolean credentialPresent, PendingAutoEnrollment pending) { }
}
