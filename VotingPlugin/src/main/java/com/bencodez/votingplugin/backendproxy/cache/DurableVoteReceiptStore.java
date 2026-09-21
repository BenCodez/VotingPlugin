package com.bencodez.votingplugin.backendproxy.cache;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import com.bencodez.votingplugin.util.DurableFiles;

/** Bounded append journal for backend vote IDs completed before acknowledgement. */
final class DurableVoteReceiptStore {
	static final long RECEIPT_TTL_MILLIS = TimeUnit.DAYS.toMillis(7);
	private static final int MAX_RECEIPTS = 262144;
	private static final long MAX_FILE_BYTES = 16L * 1024L * 1024L;
	private static final String HEADER = "VP-VOTE-RECEIPTS-1";
	private static final ConcurrentHashMap<Path, Object> FILE_LOCKS = new ConcurrentHashMap<>();

	private final Path file;
	private final Object fileLock;
	private final LinkedHashMap<UUID, Long> receipts = new LinkedHashMap<>();
	private int journalRecords;

	DurableVoteReceiptStore(Path file) throws IOException {
		this.file = file.toAbsolutePath().normalize();
		this.fileLock = FILE_LOCKS.computeIfAbsent(this.file, ignored -> new Object());
		synchronized (fileLock) {
			load();
		}
	}

	synchronized Map<UUID, Long> snapshot() {
		cleanup(System.currentTimeMillis());
		return new LinkedHashMap<>(receipts);
	}

	synchronized long complete(UUID voteId) {
		if (voteId == null) return 0L;
		long now = System.currentTimeMillis();
		cleanup(now);
		Long current = receipts.get(voteId);
		if (current != null && current > now) return current;
		if (receipts.size() >= MAX_RECEIPTS) return 0L;
		long expiresAt = now + RECEIPT_TTL_MILLIS;
		String record = voteId + "\t" + expiresAt + '\n';
		synchronized (fileLock) {
			if (!prepareAppend(record) || !append(record)) return 0L;
		}
		receipts.put(voteId, expiresAt);
		journalRecords++;
		return expiresAt;
	}

	private void load() throws IOException {
		if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) return;
		if (Files.isSymbolicLink(file) || !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Vote receipt journal is not a regular file");
		}
		if (Files.size(file) > MAX_FILE_BYTES) throw new IOException("Vote receipt journal exceeds size limit");
		String content = Files.readString(file, StandardCharsets.UTF_8);
		String[] lines = content.split("\\n", -1);
		if (lines.length == 0 || !HEADER.equals(lines[0])) throw new IOException("Unsupported vote receipt journal");
		long now = System.currentTimeMillis();
		for (int index = 1; index < lines.length; index++) {
			if (lines[index].isBlank()) continue;
			try {
				String[] fields = lines[index].split("\\t", 2);
				if (fields.length != 2) throw new IllegalArgumentException("Malformed receipt");
				UUID voteId = UUID.fromString(fields[0]);
				long expiresAt = Long.parseLong(fields[1]);
				if (expiresAt > now) receipts.put(voteId, expiresAt);
			} catch (RuntimeException malformed) {
				if (index == lines.length - 1 && !content.endsWith("\n")) {
					if (!compact()) throw new IOException("Unable to repair vote receipt journal", malformed);
					return;
				}
				throw new IOException("Malformed vote receipt journal", malformed);
			}
			journalRecords++;
			if (receipts.size() > MAX_RECEIPTS) throw new IOException("Vote receipt journal exceeds entry limit");
		}
	}

	private boolean prepareAppend(String record) {
		try {
			long projected = (Files.exists(file) ? Files.size(file) : HEADER.length() + 1L)
					+ record.getBytes(StandardCharsets.UTF_8).length;
			boolean shouldCompact = journalRecords > receipts.size() * 2 + 256;
			if ((projected > MAX_FILE_BYTES || shouldCompact) && compact()) {
				projected = Files.size(file) + record.getBytes(StandardCharsets.UTF_8).length;
			}
			return projected <= MAX_FILE_BYTES;
		} catch (IOException failure) {
			return false;
		}
	}

	private boolean append(String record) {
		try {
			Files.createDirectories(file.getParent());
			if (!Files.exists(file, LinkOption.NOFOLLOW_LINKS)) {
				Path staged = file.resolveSibling(file.getFileName() + ".tmp");
				Files.writeString(staged, HEADER + '\n' + record, StandardCharsets.UTF_8,
						StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
				DurableFiles.publishStagedFile(staged, file);
			} else {
				Files.writeString(file, record, StandardCharsets.UTF_8, StandardOpenOption.APPEND,
						StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS);
				DurableFiles.forceFile(file);
			}
			return true;
		} catch (IOException failure) {
			return false;
		}
	}

	private boolean compact() {
		try {
			StringBuilder content = new StringBuilder(HEADER).append('\n');
			for (Map.Entry<UUID, Long> receipt : receipts.entrySet()) {
				content.append(receipt.getKey()).append('\t').append(receipt.getValue()).append('\n');
			}
			Path staged = file.resolveSibling(file.getFileName() + ".tmp");
			Files.writeString(staged, content, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			DurableFiles.publishStagedFile(staged, file);
			journalRecords = receipts.size();
			return true;
		} catch (IOException failure) {
			return false;
		}
	}

	private void cleanup(long now) {
		receipts.entrySet().removeIf(entry -> entry.getValue() <= now);
	}
}
