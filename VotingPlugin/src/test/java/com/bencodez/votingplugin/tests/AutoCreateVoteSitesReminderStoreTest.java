package com.bencodez.votingplugin.maintenance;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Snapshot;
import com.bencodez.votingplugin.util.DurableFiles;
import com.bencodez.votingplugin.util.DurableFiles.PublishedException;

class AutoCreateVoteSitesReminderStoreTest {

	private static final String FILE_NAME = ".auto-create-sites-reminder.dat";

	@TempDir
	Path directory;
	private final List<AutoCreateVoteSitesReminderStore> leasedStores = new ArrayList<>();

	@AfterEach
	void releaseTestLeases() throws IOException {
		for (AutoCreateVoteSitesReminderStore store : leasedStores) store.releaseLease();
	}

	@Test
	void leaseIsIdempotentAndOtherStoreDefersUntilExplicitRelease() throws Exception {
		AutoCreateVoteSitesReminderStore owner = leaseStore();
		AutoCreateVoteSitesReminderStore successor = leaseStore();
		assertTrue(owner.tryAcquireLease());
		assertTrue(owner.tryAcquireLease());
		assertFalse(successor.tryAcquireLease());
		assertFalse(successor.tryAcquireLease());
		assertTrue(owner.tryAcquireLease());
		Path lock = lockTarget();
		assertTrue(Files.isRegularFile(lock));
		assertEquals(0, Files.size(lock));
		owner.releaseLease();
		owner.releaseLease();
		assertTrue(Files.exists(lock), "Released lock file must remain present");
		assertEquals(0, Files.size(lock));
		assertTrue(successor.tryAcquireLease());
		assertFalse(owner.tryAcquireLease());
		successor.releaseLease();
		assertTrue(owner.tryAcquireLease());
	}

	@Test
	void separatePluginClassLoaderDefersUntilOwnerReleasesJvmLease() throws Exception {
		AutoCreateVoteSitesReminderStore owner = leaseStore();
		String storeName = AutoCreateVoteSitesReminderStore.class.getName();
		URL classes = AutoCreateVoteSitesReminderStore.class.getProtectionDomain().getCodeSource().getLocation();
		try (URLClassLoader loader = new URLClassLoader(new URL[] { classes },
				AutoCreateVoteSitesReminderStore.class.getClassLoader()) {
			@Override
			protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
				if (!name.equals(storeName) && !name.startsWith(storeName + "$")) {
					return super.loadClass(name, resolve);
				}
				synchronized (getClassLoadingLock(name)) {
					Class<?> loaded = findLoadedClass(name);
					if (loaded == null) loaded = findClass(name);
					if (resolve) resolveClass(loaded);
					return loaded;
				}
			}
		}) {
			Class<?> successorType = loader.loadClass(storeName);
			assertFalse(successorType == AutoCreateVoteSitesReminderStore.class);
			Object successor = successorType.getConstructor(Path.class).newInstance(directory);
			try {
				assertTrue(owner.tryAcquireLease());
				assertEquals(false, successorType.getMethod("tryAcquireLease").invoke(successor));
				assertEquals(false, successorType.getMethod("tryAcquireLease").invoke(successor));
				Snapshot finalSnapshot = new Snapshot(2000, 1000);
				owner.save(finalSnapshot);
				assertTrue(owner.tryAcquireLease());
				owner.releaseLease();
				assertEquals(true, successorType.getMethod("tryAcquireLease").invoke(successor));
				assertEquals(finalSnapshot, successorType.getMethod("load").invoke(successor));
				assertFalse(owner.tryAcquireLease());
			} finally {
				successorType.getMethod("releaseLease").invoke(successor);
				owner.releaseLease();
			}
		}
	}

	@Test
	void ordinaryPublicationFailureKeepsLeaseUntilOwnerReleasesIt() throws Exception {
		AutoCreateVoteSitesReminderStore owner = new AutoCreateVoteSitesReminderStore(directory,
				(staged, target) -> { throw new IOException("Simulated publication failure"); });
		leasedStores.add(owner);
		AutoCreateVoteSitesReminderStore successor = leaseStore();
		assertTrue(owner.tryAcquireLease());
		assertThrows(IOException.class, () -> owner.save(new Snapshot(1000, -1)));
		assertFalse(successor.tryAcquireLease());
		owner.releaseLease();
		assertTrue(successor.tryAcquireLease());
		assertEquals(0, Files.size(lockTarget()));
		assertFalse(Files.exists(target()));
	}

	@Test
	void publishedFailureCanReleaseAndReacquireWithoutDeletingVisibleRecord() throws Exception {
		PublishedException uncertain = new PublishedException(new IOException("Simulated metadata failure"));
		AutoCreateVoteSitesReminderStore owner = new AutoCreateVoteSitesReminderStore(directory, (staged, target) -> {
			DurableFiles.publishStagedFile(staged, target);
			throw uncertain;
		});
		leasedStores.add(owner);
		AutoCreateVoteSitesReminderStore successor = leaseStore();
		assertTrue(owner.tryAcquireLease());
		Snapshot published = new Snapshot(1000, 1000);
		assertSame(uncertain, assertThrows(PublishedException.class, () -> owner.save(published)));
		assertFalse(successor.tryAcquireLease());
		owner.releaseLease();
		assertTrue(successor.tryAcquireLease());
		assertEquals(published, successor.load());
		assertEquals(List.of(FILE_NAME, ".auto-create-sites-reminder.lock"), children());
	}

	@Test
	void nonEmptyLockFileIsRejectedRatherThanTruncated() throws Exception {
		byte[] original = { 1, 2, 3 };
		Files.write(lockTarget(), original);
		AutoCreateVoteSitesReminderStore store = leaseStore();
		assertThrows(IOException.class, store::tryAcquireLease);
		assertArrayEquals(original, Files.readAllBytes(lockTarget()));
		store.releaseLease();
		assertArrayEquals(original, Files.readAllBytes(lockTarget()));
	}

	@Test
	void lockSymlinkIsRejectedWithoutChangingItsDestination() throws Exception {
		Path outside = directory.resolve("outside-lock");
		Files.write(outside, new byte[0]);
		Files.createSymbolicLink(lockTarget(), outside);
		AutoCreateVoteSitesReminderStore store = leaseStore();
		assertThrows(IOException.class, store::tryAcquireLease);
		assertTrue(Files.isSymbolicLink(lockTarget()));
		assertEquals(0, Files.size(outside));
		store.releaseLease();
		assertTrue(Files.isSymbolicLink(lockTarget()));
	}

	@Test
	void danglingLockSymlinkAndNonRegularLockAreRejected() throws Exception {
		AutoCreateVoteSitesReminderStore store = leaseStore();
		Files.createSymbolicLink(lockTarget(), directory.resolve("missing-lock"));
		assertThrows(IOException.class, store::tryAcquireLease);
		assertTrue(Files.isSymbolicLink(lockTarget()));
		Files.delete(lockTarget());
		Files.createDirectory(lockTarget());
		assertThrows(IOException.class, store::tryAcquireLease);
		assertTrue(Files.isDirectory(lockTarget()));
	}

	@Test
	void createdLeaseFileHasOwnerOnlyPermissionsWhereSupported() throws Exception {
		AutoCreateVoteSitesReminderStore store = leaseStore();
		assertTrue(store.tryAcquireLease());
		if (Files.getFileAttributeView(lockTarget(), PosixFileAttributeView.class) != null) {
			assertEquals(PosixFilePermissions.fromString("rw-------"), Files.getPosixFilePermissions(lockTarget()));
		}
		assertEquals(0, Files.size(lockTarget()));
	}

	@Test
	void absentRecordReturnsNullWithoutCreatingAnything() throws Exception {
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		assertNull(store.load());
		assertEquals(List.of(), children());
	}

	@Test
	void roundTripAndRestartPreserveBothCountersAndNeverRemindedMarker() throws Exception {
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		Snapshot first = new Snapshot(12345, -1);
		store.save(first);
		assertEquals(first, store.load());
		assertEquals(first, new AutoCreateVoteSitesReminderStore(directory).load());
		Snapshot reminded = new Snapshot(15000, 14000);
		store.save(reminded);
		assertEquals(reminded, new AutoCreateVoteSitesReminderStore(directory).load());
		assertEquals(List.of(FILE_NAME), children());
	}

	@Test
	void encodedRecordIsExactly56BytesWithBigEndianCountersAndHeaderChecksum() throws Exception {
		Snapshot snapshot = new Snapshot(0x0102030405060708L, 0x0001020304050607L);
		new AutoCreateVoteSitesReminderStore(directory).save(snapshot);
		byte[] actual = Files.readAllBytes(target());
		assertEquals(56, actual.length);
		assertArrayEquals(record(snapshot.totalRuntimeMillis(), snapshot.lastReminderRuntimeMillis()), actual);
		assertArrayEquals("VPACR001".getBytes(StandardCharsets.US_ASCII), Arrays.copyOf(actual, 8));
		ByteBuffer counters = ByteBuffer.wrap(actual).order(ByteOrder.BIG_ENDIAN);
		assertEquals(snapshot.totalRuntimeMillis(), counters.getLong(8));
		assertEquals(snapshot.lastReminderRuntimeMillis(), counters.getLong(16));
	}

	@Test
	void zeroAndSaturatedValidCountersRoundTrip() throws Exception {
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		for (Snapshot snapshot : new Snapshot[] { new Snapshot(0, -1), new Snapshot(0, 0),
				new Snapshot(Long.MAX_VALUE, -1), new Snapshot(Long.MAX_VALUE, Long.MAX_VALUE) }) {
			store.save(snapshot);
			assertEquals(snapshot, store.load());
		}
	}

	@Test
	void badMagicWithValidChecksumIsRejectedAndCannotBeOverwritten() throws Exception {
		byte[] invalid = record(1000, -1);
		invalid[7] = '2';
		checksum(invalid);
		assertInvalidPreserved(invalid);
	}

	@Test
	void badChecksumIsRejectedAndCannotBeOverwritten() throws Exception {
		byte[] invalid = record(1000, -1);
		invalid[55] ^= 1;
		assertInvalidPreserved(invalid);
	}

	@Test
	void alteredCountersWithoutNewChecksumAreRejected() throws Exception {
		byte[] invalid = record(1000, -1);
		invalid[15] ^= 1;
		assertInvalidPreserved(invalid);
	}

	@Test
	void emptyShortAndTrailingRecordsAreRejectedWithoutRepair() throws Exception {
		byte[] valid = record(1000, -1);
		for (byte[] invalid : new byte[][] { new byte[0], Arrays.copyOf(valid, 23), Arrays.copyOf(valid, 55),
				Arrays.copyOf(valid, 57), Arrays.copyOf(valid, 4096) }) {
			assertInvalidPreserved(invalid);
		}
	}

	@Test
	void invalidCountersWithCorrectChecksumAreRejected() throws Exception {
		for (long[] invalid : new long[][] { { -1, -1 }, { 0, -2 }, { 1000, 1001 },
				{ Long.MAX_VALUE - 1, Long.MAX_VALUE } }) {
			assertInvalidPreserved(record(invalid[0], invalid[1]));
		}
	}

	@Test
	void recordSymlinkIsRejectedWithoutTouchingItsDestination() throws Exception {
		Path outside = directory.resolve("outside-record.dat");
		byte[] original = record(1000, -1);
		Files.write(outside, original);
		Files.createSymbolicLink(target(), outside);
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		assertThrows(IOException.class, store::load);
		assertThrows(IOException.class, () -> store.save(new Snapshot(2000, 2000)));
		assertTrue(Files.isSymbolicLink(target()));
		assertArrayEquals(original, Files.readAllBytes(outside));
		assertEquals(List.of(FILE_NAME, "outside-record.dat"), children());
	}

	@Test
	void danglingRecordSymlinkIsFailureRatherThanAbsent() throws Exception {
		Files.createSymbolicLink(target(), directory.resolve("missing-record.dat"));
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		assertThrows(IOException.class, store::load);
		assertThrows(IOException.class, () -> store.save(new Snapshot(1, -1)));
		assertTrue(Files.isSymbolicLink(target()));
	}

	@Test
	void nonRegularRecordIsRejectedAndPreserved() throws Exception {
		Files.createDirectory(target());
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		assertThrows(IOException.class, store::load);
		assertThrows(IOException.class, () -> store.save(new Snapshot(1, -1)));
		assertTrue(Files.isDirectory(target()));
		assertEquals(List.of(FILE_NAME), children());
	}

	@Test
	void directoryMustAlreadyExistAndMustNotBeAFileOrSymlink() throws Exception {
		Path missing = directory.resolve("missing").resolve("nested");
		assertThrows(IOException.class, () -> new AutoCreateVoteSitesReminderStore(missing));
		assertFalse(Files.exists(directory.resolve("missing")));
		Path file = directory.resolve("ordinary-file");
		Files.writeString(file, "test");
		assertThrows(IOException.class, () -> new AutoCreateVoteSitesReminderStore(file));
		Path link = directory.resolve("directory-link");
		Files.createSymbolicLink(link, directory);
		assertThrows(IOException.class, () -> new AutoCreateVoteSitesReminderStore(link));
	}

	@Test
	void removedDataDirectoryIsNotReportedAsAbsentAndIsNotRecreated() throws Exception {
		Path child = Files.createDirectory(directory.resolve("data"));
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(child);
		Files.delete(child);
		assertThrows(IOException.class, store::load);
		assertThrows(IOException.class, () -> store.save(new Snapshot(1, -1)));
		assertFalse(Files.exists(child));
	}

	@Test
	void prePublicationFailurePreservesOriginalAndCleansStagedRecord() throws Exception {
		Snapshot original = new Snapshot(1000, -1);
		new AutoCreateVoteSitesReminderStore(directory).save(original);
		byte[] originalBytes = Files.readAllBytes(target());
		byte[] proposedBytes = record(2000, 2000);
		IOException failure = new IOException("Simulated pre-publication failure");
		AtomicInteger attempts = new AtomicInteger();
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory, (staged, target) -> {
			attempts.incrementAndGet();
			assertEquals(directory.toAbsolutePath(), staged.getParent());
			assertEquals(56, Files.size(staged));
			assertArrayEquals(proposedBytes, Files.readAllBytes(staged));
			throw failure;
		});
		assertSame(failure, assertThrows(IOException.class, () -> store.save(new Snapshot(2000, 2000))));
		assertEquals(1, attempts.get());
		assertArrayEquals(originalBytes, Files.readAllBytes(target()));
		assertEquals(original, store.load());
		assertEquals(List.of(FILE_NAME), children());
	}

	@Test
	void failedInitialPublicationLeavesNoRecordOrTempAndCanBeRetried() throws Exception {
		IOException failure = new IOException("Simulated initial publication failure");
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory,
				(staged, target) -> { throw failure; });
		assertSame(failure, assertThrows(IOException.class, () -> store.save(new Snapshot(1000, -1))));
		assertNull(store.load());
		assertEquals(List.of(), children());
		Snapshot retry = new Snapshot(1500, -1);
		new AutoCreateVoteSitesReminderStore(directory).save(retry);
		assertEquals(retry, store.load());
	}

	@Test
	void postPublicationFailureIsPropagatedWithoutPretendingToRollBack() throws Exception {
		new AutoCreateVoteSitesReminderStore(directory).save(new Snapshot(1000, -1));
		Snapshot published = new Snapshot(2000, 2000);
		PublishedException uncertain = new PublishedException(new IOException("Simulated directory force failure"));
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory, (staged, target) -> {
			DurableFiles.publishStagedFile(staged, target);
			throw uncertain;
		});
		assertSame(uncertain, assertThrows(PublishedException.class, () -> store.save(published)));
		assertEquals(published, new AutoCreateVoteSitesReminderStore(directory).load());
		assertEquals(List.of(FILE_NAME), children());
	}

	@Test
	void cleanupFailureIsSuppressedOntoPrimaryPublicationFailure() throws Exception {
		new AutoCreateVoteSitesReminderStore(directory).save(new Snapshot(1000, -1));
		IOException primary = new IOException("Simulated publication failure with cleanup failure");
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory, (staged, target) -> {
			Files.delete(staged);
			Files.createDirectory(staged);
			Files.writeString(staged.resolve("obstruction"), "test");
			throw primary;
		});
		assertSame(primary, assertThrows(IOException.class, () -> store.save(new Snapshot(2000, 2000))));
		assertEquals(1, primary.getSuppressed().length);
		assertTrue(primary.getSuppressed()[0] instanceof IOException);
		assertEquals(new Snapshot(1000, -1), store.load());
	}

	@Test
	void stagedAndPublishedFileHaveOwnerOnlyPermissionsWhenPosixIsAvailable() throws Exception {
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory, (staged, target) -> {
			if (Files.getFileAttributeView(staged, PosixFileAttributeView.class) != null) {
				assertEquals(PosixFilePermissions.fromString("rw-------"), Files.getPosixFilePermissions(staged));
			}
			DurableFiles.publishStagedFile(staged, target);
		});
		store.save(new Snapshot(1000, -1));
		if (Files.getFileAttributeView(target(), PosixFileAttributeView.class) != null) {
			assertEquals(PosixFilePermissions.fromString("rw-------"), Files.getPosixFilePermissions(target()));
		}
		assertEquals(List.of(FILE_NAME), children());
	}

	private void assertInvalidPreserved(byte[] invalid) throws Exception {
		Files.write(target(), invalid);
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		assertThrows(IOException.class, store::load);
		assertThrows(IOException.class, () -> store.save(new Snapshot(2000, 2000)));
		assertArrayEquals(invalid, Files.readAllBytes(target()));
		assertEquals(List.of(FILE_NAME), children());
	}

	private Path target() {
		return directory.resolve(FILE_NAME);
	}

	private Path lockTarget() {
		return directory.resolve(".auto-create-sites-reminder.lock");
	}

	private AutoCreateVoteSitesReminderStore leaseStore() throws IOException {
		AutoCreateVoteSitesReminderStore store = new AutoCreateVoteSitesReminderStore(directory);
		leasedStores.add(store);
		return store;
	}

	private List<String> children() throws IOException {
		try (var entries = Files.list(directory)) {
			return entries.map(path -> path.getFileName().toString()).sorted().toList();
		}
	}

	private static byte[] record(long total, long lastReminder) throws Exception {
		byte[] bytes = new byte[56];
		ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).put("VPACR001".getBytes(StandardCharsets.US_ASCII))
				.putLong(total).putLong(lastReminder);
		checksum(bytes);
		return bytes;
	}

	private static void checksum(byte[] record) throws Exception {
		byte[] checksum = MessageDigest.getInstance("SHA-256").digest(Arrays.copyOf(record, 24));
		System.arraycopy(checksum, 0, record, 24, checksum.length);
	}
}
