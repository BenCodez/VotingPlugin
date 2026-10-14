package com.bencodez.votingplugin.maintenance;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Objects;
import java.util.Set;

import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Snapshot;
import com.bencodez.votingplugin.util.DurableFiles;

/**
 * A fixed, checksummed active-runtime checkpoint in the plugin's existing data
 * directory. One reminder worker owns all load/save operations. This store never
 * creates the directory, repairs corrupt records, or follows a record symlink.
 */
public final class AutoCreateVoteSitesReminderStore {

	private static final String FILE_NAME = ".auto-create-sites-reminder.dat";
	private static final byte[] MAGIC = "VPACR001".getBytes(StandardCharsets.US_ASCII);
	private static final int HEADER_BYTES = 24;
	private static final int RECORD_BYTES = 56;
	private final Path directory;
	private final Path target;
	private final Publisher publisher;
	private FileChannel leaseChannel;
	private FileLock lease;

	@FunctionalInterface
	interface Publisher {
		void publish(Path staged, Path target) throws IOException;
	}

	public AutoCreateVoteSitesReminderStore(Path dataDirectory) throws IOException {
		this(dataDirectory, DurableFiles::publishStagedFile);
	}

	AutoCreateVoteSitesReminderStore(Path dataDirectory, Publisher publisher) throws IOException {
		this.directory = Objects.requireNonNull(dataDirectory, "dataDirectory").toAbsolutePath().normalize();
		this.target = directory.resolve(FILE_NAME);
		this.publisher = Objects.requireNonNull(publisher, "publisher");
		validateDirectory();
	}

	/**
	 * Nonblocking admission across plugin instances/class loaders in one JVM, held
	 * through final publication. Multiple JVMs sharing the directory are unsupported:
	 * closing another channel may release native locks on some platforms.
	 */
	public boolean tryAcquireLease() throws IOException {
		if (lease != null && lease.isValid()) return true;
		validateDirectory();
		Path lockPath = directory.resolve(".auto-create-sites-reminder.lock");
		try {
			BasicFileAttributes attributes = Files.readAttributes(lockPath, BasicFileAttributes.class,
					LinkOption.NOFOLLOW_LINKS);
			if (!attributes.isRegularFile() || attributes.isSymbolicLink() || attributes.size() != 0) {
				throw new IOException("Invalid reminder ownership file");
			}
		} catch (NoSuchFileException absent) {
			// Create only this fixed empty metadata file inside the checked data directory.
		}
		Set<OpenOption> options = Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE,
				LinkOption.NOFOLLOW_LINKS);
		FileChannel channel = Files.getFileAttributeView(directory, PosixFileAttributeView.class,
				LinkOption.NOFOLLOW_LINKS) == null ? FileChannel.open(lockPath, options)
				: FileChannel.open(lockPath, options,
						PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
		boolean acquired = false;
		try {
			if (channel.size() != 0) throw new IOException("Invalid reminder ownership file");
			FileLock candidate;
			try {
				candidate = channel.tryLock();
			} catch (OverlappingFileLockException busy) {
				return false;
			}
			if (candidate == null) return false;
			leaseChannel = channel;
			lease = candidate;
			acquired = true;
			return true;
		} finally {
			if (!acquired) channel.close();
		}
	}

	/** Closing the channel releases its lock; keep the fixed lock file for successors. */
	public void releaseLease() throws IOException {
		FileChannel channel = leaseChannel;
		leaseChannel = null;
		lease = null;
		if (channel != null) channel.close();
	}

	/** Returns null only when the record is absent from a valid existing directory. */
	public Snapshot load() throws IOException {
		validateDirectory();
		try {
			return readRecord(target);
		} catch (NoSuchFileException absent) {
			// A missing/replaced data directory is a failure, not an absent checkpoint.
			validateDirectory();
			if (Files.notExists(target, LinkOption.NOFOLLOW_LINKS)) {
				return null;
			}
			throw absent;
		}
	}

	/**
	 * Publish a checked same-directory staged record using DurableFiles. Existing
	 * invalid or unreadable records block replacement. PublishedException propagates
	 * unchanged: the new record may already be visible, so no rollback is attempted.
	 */
	public void save(Snapshot snapshot) throws IOException {
		Objects.requireNonNull(snapshot, "snapshot");
		load();
		Path staged = createStagedFile();
		Throwable failure = null;
		try {
			ByteBuffer record = ByteBuffer.wrap(encode(snapshot));
			try (FileChannel channel = FileChannel.open(staged, StandardOpenOption.WRITE,
					StandardOpenOption.TRUNCATE_EXISTING, LinkOption.NOFOLLOW_LINKS)) {
				while (record.hasRemaining()) {
					if (channel.write(record) <= 0) {
						throw new IOException("Could not write reminder checkpoint");
					}
				}
			}
			if (!snapshot.equals(readRecord(staged))) {
				throw new IOException("Staged reminder checkpoint did not match");
			}
			// Recheck after staging, before replacing any existing destination.
			load();
			publisher.publish(staged, target);
		} catch (IOException | RuntimeException | Error primary) {
			failure = primary;
			throw primary;
		} finally {
			try {
				Files.deleteIfExists(staged);
			} catch (IOException | RuntimeException cleanup) {
				if (failure != null) {
					failure.addSuppressed(cleanup);
				} else {
					throw new DurableFiles.PublishedException(cleanup instanceof IOException io ? io
							: new IOException("Reminder staging cleanup failed after publication", cleanup));
				}
			}
		}
	}

	private void validateDirectory() throws IOException {
		BasicFileAttributes attributes = Files.readAttributes(directory, BasicFileAttributes.class,
				LinkOption.NOFOLLOW_LINKS);
		if (!attributes.isDirectory() || attributes.isSymbolicLink()) {
			throw new IOException("Reminder data directory must be an existing directory");
		}
	}

	private Path createStagedFile() throws IOException {
		if (Files.getFileAttributeView(directory, PosixFileAttributeView.class, LinkOption.NOFOLLOW_LINKS) != null) {
			return Files.createTempFile(directory, ".auto-create-sites-reminder-", ".tmp",
					PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
		}
		return Files.createTempFile(directory, ".auto-create-sites-reminder-", ".tmp");
	}

	private static Snapshot readRecord(Path recordPath) throws IOException {
		BasicFileAttributes attributes = Files.readAttributes(recordPath, BasicFileAttributes.class,
				LinkOption.NOFOLLOW_LINKS);
		if (!attributes.isRegularFile() || attributes.isSymbolicLink() || attributes.size() != RECORD_BYTES) {
			throw new IOException("Invalid reminder checkpoint file");
		}
		ByteBuffer record = ByteBuffer.allocate(RECORD_BYTES);
		try (FileChannel channel = FileChannel.open(recordPath, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)) {
			if (channel.size() != RECORD_BYTES) {
				throw new IOException("Invalid reminder checkpoint length");
			}
			while (record.hasRemaining()) {
				if (channel.read(record) <= 0) {
					throw new IOException("Incomplete reminder checkpoint");
				}
			}
			if (channel.size() != RECORD_BYTES || channel.read(ByteBuffer.allocate(1)) != -1) {
				throw new IOException("Trailing reminder checkpoint data");
			}
		}
		return decode(record.array());
	}

	private static byte[] encode(Snapshot snapshot) {
		ByteBuffer record = ByteBuffer.allocate(RECORD_BYTES).order(ByteOrder.BIG_ENDIAN);
		record.put(MAGIC).putLong(snapshot.totalRuntimeMillis()).putLong(snapshot.lastReminderRuntimeMillis());
		record.put(digest(Arrays.copyOf(record.array(), HEADER_BYTES)));
		return record.array();
	}

	private static Snapshot decode(byte[] record) throws IOException {
		if (!Arrays.equals(MAGIC, Arrays.copyOf(record, MAGIC.length))) {
			throw new IOException("Invalid reminder checkpoint schema");
		}
		byte[] expected = digest(Arrays.copyOf(record, HEADER_BYTES));
		if (!MessageDigest.isEqual(expected, Arrays.copyOfRange(record, HEADER_BYTES, RECORD_BYTES))) {
			throw new IOException("Invalid reminder checkpoint checksum");
		}
		ByteBuffer header = ByteBuffer.wrap(record).order(ByteOrder.BIG_ENDIAN);
		header.position(MAGIC.length);
		try {
			return new Snapshot(header.getLong(), header.getLong());
		} catch (IllegalArgumentException invalid) {
			throw new IOException("Invalid reminder checkpoint counters", invalid);
		}
	}

	private static byte[] digest(byte[] header) {
		try {
			return MessageDigest.getInstance("SHA-256").digest(header);
		} catch (NoSuchAlgorithmException unavailable) {
			throw new IllegalStateException("SHA-256 is unavailable", unavailable);
		}
	}
}
