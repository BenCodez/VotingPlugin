package com.bencodez.votingplugin.backendproxy.http;

import com.bencodez.votingplugin.util.DurableFiles;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

/**
 * Crash-durable fence for proxy deliveries that may already have caused backend side effects.
 * A delivery is reserved before its callback runs and removed only after the proxy confirms its ACK.
 */
final class HttpInboundDeliveryStore {
	private static final String DIRECTORY = "http-transport-inbound-deliveries";
	private static final String SUFFIX = ".seen";
	private static final int MAX_ENTRIES = HttpTransportProtocol.MAX_QUEUE;
	private final Path root;
	private final Set<String> entries = new LinkedHashSet<>();

	HttpInboundDeliveryStore(Path credentialDirectory) throws IOException {
		Path credentials = credentialDirectory.toAbsolutePath().normalize();
		if (Files.isSymbolicLink(credentials) || !Files.isDirectory(credentials, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP credential directory is unsafe");
		ownerOnlyDirectory(credentials);
		root = credentials.resolve(DIRECTORY).normalize();
		if (!root.getParent().equals(credentials)) throw new IOException("HTTP inbound delivery directory is invalid");
		Files.createDirectories(root);
		if (Files.isSymbolicLink(root) || !Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP inbound delivery directory is unsafe");
		ownerOnlyDirectory(root);
		load();
	}

	synchronized boolean contains(String id) { return entries.contains(canonical(id)); }

	synchronized void reserve(String id) throws IOException {
		id = canonical(id);
		if (entries.contains(id)) return;
		if (entries.size() >= MAX_ENTRIES) throw new IOException("HTTP inbound delivery fence is full");
		requireRoot();
		Path target = root.resolve(id + SUFFIX);
		if (Files.exists(target, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP inbound delivery fence is inconsistent");
		Path temporary = Files.createTempFile(root, ".pending-", ".tmp");
		try {
			ownerOnlyFile(temporary);
			Files.writeString(temporary, id, StandardCharsets.US_ASCII, StandardOpenOption.TRUNCATE_EXISTING);
			DurableFiles.forceFile(temporary);
			try { Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE); }
			catch (java.nio.file.AtomicMoveNotSupportedException unsupported) { Files.move(temporary, target); }
			ownerOnlyFile(target);
			DurableFiles.forceDirectory(root);
			entries.add(id);
		} finally { Files.deleteIfExists(temporary); }
	}

	synchronized boolean remove(String id) throws IOException {
		id = canonical(id);
		if (!entries.contains(id)) return true;
		requireRoot();
		DurableFiles.deleteIfExists(root.resolve(id + SUFFIX));
		entries.remove(id);
		return true;
	}

	synchronized Set<String> snapshot() { return Set.copyOf(entries); }

	private void load() throws IOException {
		try (DirectoryStream<Path> files = Files.newDirectoryStream(root)) {
			for (Path file : files) {
				String name = file.getFileName().toString();
				if (name.startsWith(".pending-") && name.endsWith(".tmp") && !Files.isSymbolicLink(file)
						&& Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) {
					DurableFiles.deleteIfExists(file);
					continue;
				}
				if (Files.isSymbolicLink(file) || !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)
						|| !name.endsWith(SUFFIX) || Files.size(file) > 64L)
					throw new IOException("HTTP inbound delivery fence contains an invalid entry");
				String id;
				try { id = canonical(name.substring(0, name.length() - SUFFIX.length())); }
				catch (IllegalArgumentException invalid) {
					throw new IOException("HTTP inbound delivery fence entry is invalid", invalid);
				}
				if (!name.equals(id + SUFFIX) || !Files.readString(file, StandardCharsets.US_ASCII).equals(id)
						|| !entries.add(id))
					throw new IOException("HTTP inbound delivery fence entry is invalid");
				if (entries.size() > MAX_ENTRIES) throw new IOException("HTTP inbound delivery fence exceeds its bound");
			}
		}
	}

	private static String canonical(String id) {
		if (id == null) throw new IllegalArgumentException("HTTP delivery id is invalid");
		String canonical = UUID.fromString(id).toString();
		if (!canonical.equals(id)) throw new IllegalArgumentException("HTTP delivery id is not canonical");
		return canonical;
	}

	private void requireRoot() throws IOException {
		if (Files.isSymbolicLink(root) || !Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS))
			throw new IOException("HTTP inbound delivery directory is unsafe");
	}

	private static void ownerOnlyFile(Path path) throws IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)); }
		catch (UnsupportedOperationException ignored) { }
	}
	private static void ownerOnlyDirectory(Path path) throws IOException {
		try { Files.setPosixFilePermissions(path, EnumSet.of(PosixFilePermission.OWNER_READ,
				PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_EXECUTE)); }
		catch (UnsupportedOperationException ignored) { }
	}
}
