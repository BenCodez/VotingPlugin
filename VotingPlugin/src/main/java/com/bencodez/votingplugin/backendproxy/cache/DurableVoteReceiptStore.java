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
	private static final int MAX_ACTIVE_RECEIPTS = 262144;
	/* Larger than the complete in-memory and durable ordered lane (256 + 512). */
	private static final int COMPLETION_HEADROOM = 1024;
	/* One proxy cannot retain more release markers than its bounded outbox. */
	private static final int MAX_RELEASE_TOMBSTONES = 4096;
	private static final long MAX_FILE_BYTES = 16L * 1024L * 1024L;
	private static final String HEADER = "VP-VOTE-RECEIPTS-1";
	private static final String RELEASE = "R";
	static final long RELEASE_TOMBSTONE_TTL_MILLIS = TimeUnit.HOURS.toMillis(24);
	private static final ConcurrentHashMap<Path, Object> FILE_LOCKS = new ConcurrentHashMap<>();

	private final Path file;
	private final Object fileLock;
	private final int maxActiveReceipts;
	private final int completionHeadroom;
	private final int maxReleaseTombstones;
	private final LinkedHashMap<UUID, Long> receipts = new LinkedHashMap<>();
	private int activeReceipts;
	private int releaseTombstones;
	private int journalRecords;

	DurableVoteReceiptStore(Path file) throws IOException {
		this(file, MAX_ACTIVE_RECEIPTS, COMPLETION_HEADROOM, MAX_RELEASE_TOMBSTONES);
	}

	DurableVoteReceiptStore(Path file, int maxActiveReceipts, int completionHeadroom,
			int maxReleaseTombstones) throws IOException {
		this.file = file.toAbsolutePath().normalize();
		this.fileLock = FILE_LOCKS.computeIfAbsent(this.file, ignored -> new Object());
		this.maxActiveReceipts = maxActiveReceipts;
		this.completionHeadroom = completionHeadroom;
		this.maxReleaseTombstones = maxReleaseTombstones;
		synchronized (fileLock) {
			load();
		}
	}

	synchronized Map<UUID, Long> snapshot() {
		cleanupReleasedTombstones(System.currentTimeMillis());
		return new LinkedHashMap<>(receipts);
	}

	synchronized boolean contains(UUID voteId) {
		cleanupReleasedTombstones(System.currentTimeMillis());
		return voteId != null && receipts.containsKey(voteId);
	}

	synchronized long complete(UUID voteId) {
		if (voteId == null) return 0L;
		cleanupReleasedTombstones(System.currentTimeMillis());
		Long current = receipts.get(voteId);
		if (current != null) return current;
		if (activeReceipts >= maxActiveReceipts + completionHeadroom) return 0L;
		long expiresAt = Long.MAX_VALUE;
		String record = voteId + "\t" + expiresAt + '\n';
		synchronized (fileLock) {
			if (!prepareAppend(record) || !append(record)) return 0L;
		}
		putReceipt(voteId, expiresAt);
		journalRecords++;
		return expiresAt;
	}

	synchronized long release(UUID voteId) {
		if (voteId == null) return 0L;
		long now = System.currentTimeMillis();
		cleanupReleasedTombstones(now);
		Long current = receipts.get(voteId);
		if (current == null && releaseTombstones >= maxReleaseTombstones) return 0L;
		if (current != null && current != Long.MAX_VALUE) return current;
		long expiresAt = now + RELEASE_TOMBSTONE_TTL_MILLIS;
		String record = RELEASE + '\t' + voteId + '\t' + expiresAt + '\n';
		synchronized (fileLock) {
			if (!prepareAppend(record) || !append(record)) return 0L;
		}
		putReceipt(voteId, expiresAt);
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
		boolean unterminatedTail = !content.endsWith("\n");
		int completeLineLimit = unterminatedTail ? lines.length - 1 : lines.length;
		for (int index = 1; index < completeLineLimit; index++) {
			if (lines[index].isBlank()) continue;
			try {
				String[] fields = lines[index].split("\\t", 3);
				if (RELEASE.equals(fields[0])) {
					if (fields.length == 2) removeReceipt(UUID.fromString(fields[1]));
					else if (fields.length == 3) {
						UUID voteId = UUID.fromString(fields[1]);
						long expiresAt = Long.parseLong(fields[2]);
						if (expiresAt > System.currentTimeMillis()) putReceipt(voteId, expiresAt);
						else removeReceipt(voteId);
					} else throw new IllegalArgumentException("Malformed receipt release");
				} else {
					if (fields.length != 2) throw new IllegalArgumentException("Malformed receipt");
					UUID voteId = UUID.fromString(fields[0]);
					long expiresAt = Long.parseLong(fields[1]);
					putReceipt(voteId, expiresAt);
				}
			} catch (RuntimeException malformed) {
				throw new IOException("Malformed vote receipt journal", malformed);
			}
			journalRecords++;
			if (activeReceipts > maxActiveReceipts + completionHeadroom
					|| releaseTombstones > maxReleaseTombstones) {
				throw new IOException("Vote receipt journal exceeds entry limit");
			}
		}
		if (unterminatedTail && !compact()) throw new IOException("Unable to repair vote receipt journal");
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

	private void cleanupReleasedTombstones(long now) {
		int before = receipts.size();
		receipts.entrySet().removeIf(entry -> entry.getValue() != Long.MAX_VALUE && entry.getValue() <= now);
		releaseTombstones -= before - receipts.size();
	}

	private void putReceipt(UUID voteId, long expiresAt) {
		Long previous = receipts.put(voteId, expiresAt);
		if (previous != null) {
			if (previous == Long.MAX_VALUE) activeReceipts--;
			else releaseTombstones--;
		}
		if (expiresAt == Long.MAX_VALUE) activeReceipts++;
		else releaseTombstones++;
	}

	private void removeReceipt(UUID voteId) {
		Long previous = receipts.remove(voteId);
		if (previous == null) return;
		if (previous == Long.MAX_VALUE) activeReceipts--;
		else releaseTombstones--;
	}
}
