package com.bencodez.votingplugin.voteshop.service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;

import com.bencodez.votingplugin.util.DurableFiles;

/** Restart-safe local proof that a rejected reward callback is safe to refund. */
final class SharedMysqlCompensationStore {
	private static final String DIRECTORY = ".voteshop-compensations";
	private static final String SUFFIX = ".pending";
	private static final int MAX_PURCHASE_ID_BYTES = 256;
	private static final int RECOVERY_BATCH_SIZE = 128;

	private final Path directory;

	SharedMysqlCompensationStore(Path dataDirectory) {
		this.directory = dataDirectory.toAbsolutePath().normalize().resolve(DIRECTORY);
	}

	void record(String purchaseId) throws IOException {
		byte[] contents = contents(purchaseId);
		ensureSafeDirectory();
		Path target = marker(purchaseId);
		if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
			if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) throw unsafe();
			return;
		}
		Path temporary = Files.createTempFile(directory, ".compensation-", ".tmp");
		try {
			Files.write(temporary, contents, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.forceFile(temporary);
			try {
				Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE);
			} catch (AtomicMoveNotSupportedException unsupported) {
				Files.move(temporary, target);
			}
			DurableFiles.forceDirectory(directory);
		} finally {
			Files.deleteIfExists(temporary);
		}
	}

	List<String> loadBatch() throws IOException {
		if (!Files.exists(directory, LinkOption.NOFOLLOW_LINKS)) return List.of();
		if (!Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(directory)) throw unsafe();
		List<String> purchases = new ArrayList<>();
		try (DirectoryStream<Path> entries = Files.newDirectoryStream(directory, "*" + SUFFIX)) {
			for (Path entry : entries) {
				if (purchases.size() == RECOVERY_BATCH_SIZE) break;
				if (!Files.isRegularFile(entry, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(entry)
						|| Files.size(entry) > MAX_PURCHASE_ID_BYTES) continue;
				String purchaseId = Files.readString(entry, StandardCharsets.UTF_8);
				if (marker(purchaseId).equals(entry.toAbsolutePath().normalize())) purchases.add(purchaseId);
			}
		}
		return purchases;
	}

	void remove(String purchaseId) throws IOException {
		DurableFiles.deleteIfExists(marker(purchaseId));
	}

	private void ensureSafeDirectory() throws IOException {
		if (Files.exists(directory, LinkOption.NOFOLLOW_LINKS)) {
			if (!Files.isDirectory(directory, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(directory)) throw unsafe();
			return;
		}
		Files.createDirectories(directory);
		DurableFiles.forceDirectory(directory.getParent());
	}

	private Path marker(String purchaseId) throws IOException {
		return directory.resolve(hash(contents(purchaseId)) + SUFFIX).toAbsolutePath().normalize();
	}

	private static byte[] contents(String purchaseId) throws IOException {
		if (purchaseId == null || purchaseId.isBlank()) throw unsafe();
		byte[] contents = purchaseId.getBytes(StandardCharsets.UTF_8);
		if (contents.length > MAX_PURCHASE_ID_BYTES) throw unsafe();
		return contents;
	}

	private static String hash(byte[] value) throws IOException {
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(value));
		} catch (NoSuchAlgorithmException impossible) {
			throw new IOException("SHA-256 is unavailable", impossible);
		}
	}

	private static IOException unsafe() {
		return new IOException("Unsafe vote shop compensation marker");
	}
}
