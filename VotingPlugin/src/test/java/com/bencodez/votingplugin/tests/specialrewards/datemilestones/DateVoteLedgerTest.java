package com.bencodez.votingplugin.tests.specialrewards.datemilestones;

import com.bencodez.votingplugin.specialrewards.datemilestones.*;
import static org.junit.jupiter.api.Assertions.*;
import com.bencodez.votingplugin.core.datemilestones.DateVoteEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.StandardOpenOption;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
class DateVoteLedgerTest {
    @TempDir Path root;
    final UUID player = UUID.randomUUID();

    @Test void progressOfMissingLedgerDoesNotInitializeStorage() throws Exception {
        Path missing = root.resolve("missing");
        var progress = new DateVoteLedger(missing).progress(event("empty", "Empty", 200), player);
        assertEquals(0, progress.votes());
        assertTrue(progress.reservedAwards().isEmpty());
        assertTrue(progress.submittedAwards().isEmpty());
        assertTrue(progress.deferredAwards().isEmpty());
        assertFalse(Files.exists(missing));
    }

    @Test void invalidLedgerPathCannotBeReportedAsAnEmptySnapshot() throws Exception {
        Path invalid = root.resolve("invalid");
        Files.writeString(invalid, "synthetic non-directory");
        assertThrows(IOException.class, () -> new DateVoteLedger(invalid)
                .progress(event("empty", "Empty", 200), player));
        assertEquals("synthetic non-directory", Files.readString(invalid));
    }

    @Test void unavailableCrossProcessLockRejectsEveryTransactionWithoutChangingProgress() throws Exception {
        var e = event("a", "A", 200); UUID occurrence = UUID.randomUUID();
        var first = new DateVoteLedger(root);
        first.record(e, player, occurrence, false);
        Path lockPath = root.resolve(".ledger.lock");
        Process owner = lockProcess(lockPath, "hold");
        try {
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
            while (owner.getInputStream().available() == 0 && owner.isAlive() && System.nanoTime() < deadline) Thread.sleep(5);
            assertTrue(owner.getInputStream().available() > 0, "External lock owner did not become ready");
            assertEquals(1, owner.getInputStream().read());
            var successor = new DateVoteLedger(root);
            assertThrows(IOException.class, () -> successor.record(e, player, UUID.randomUUID()));
            assertThrows(IOException.class, () -> successor.reserve(e, player, 1));
            assertThrows(IOException.class, () -> successor.submitted(e, player, 1));
            assertThrows(IOException.class, () -> successor.progress(e, player));
            owner.getOutputStream().write(1); owner.getOutputStream().flush();
            assertTrue(owner.waitFor(10, TimeUnit.SECONDS)); assertEquals(0, owner.exitValue());
        } finally {
            owner.destroyForcibly(); owner.waitFor(10, TimeUnit.SECONDS);
        }
        assertEquals(1, first.progress(e, player).votes());
        assertEquals(Set.of(1), first.progress(e, player).deferredAwards());
        var successor = new DateVoteLedger(root);
        assertEquals(List.of(), successor.record(e, player, occurrence, false));
        assertEquals(1, successor.progress(e, player).votes());
        assertEquals(Set.of(1), successor.progress(e, player).deferredAwards());
    }

    @Test void classloaderAndPathAliasContenderCannotReleaseAnOwnersNativeLock() throws Exception {
        Path actual = Files.createDirectory(root.resolve("actual"));
        Path alias = Files.createSymbolicLink(root.resolve("alias"), actual);
        Path directory = actual.resolve("ledger");
        var e = event("a", "A", 200);
        var owner = new DateVoteLedger(directory);
        owner.record(e, player, UUID.randomUUID(), false);
        CountDownLatch locked = new CountDownLatch(1), release = new CountDownLatch(1), contenderStarted = new CountDownLatch(1);
        AtomicReference<Thread> contenderThread = new AtomicReference<>();
        try (var loader = separateLedgerClassloader(); var pool = Executors.newFixedThreadPool(2)) {
            Class<?> ledgerClass = loader.loadClass(DateVoteLedger.class.getName());
            assertNotSame(DateVoteLedger.class, ledgerClass);
            Object successor = ledgerClass.getConstructor(Path.class).newInstance(alias.resolve("ledger"));
            Future<?> owning = pool.submit(() -> withFence(owner, () -> {
                locked.countDown();
                if (!release.await(20, TimeUnit.SECONDS)) throw new IOException("Fixture lock owner timed out");
                return null;
            }));
            try {
                assertTrue(locked.await(10, TimeUnit.SECONDS));
                assertEquals(3, probeLock(directory.resolve(".ledger.lock")));
                Future<?> contending = pool.submit(() -> {
                    contenderThread.set(Thread.currentThread()); contenderStarted.countDown();
                    return invoke(ledgerClass.getMethod("record", DateVoteEvent.class, UUID.class, UUID.class, boolean.class),
                            successor, e, player, UUID.randomUUID(), false);
                });
                assertTrue(contenderStarted.await(10, TimeUnit.SECONDS));
                long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10);
                while (!contending.isDone() && contenderThread.get().getState() != Thread.State.BLOCKED
                        && System.nanoTime() < deadline) Thread.sleep(5);
                assertEquals(3, probeLock(directory.resolve(".ledger.lock")), "A contender must not close another descriptor and drop the owner's native lock");
                assertFalse(contending.isDone(), "The contender must serialize before opening the OS channel");
                assertEquals(Thread.State.BLOCKED, contenderThread.get().getState());
                release.countDown(); owning.get(10, TimeUnit.SECONDS); contending.get(10, TimeUnit.SECONDS);
            } finally { release.countDown(); }
        }
        assertEquals(2, owner.progress(e, player).votes());
        assertEquals(0, probeLock(directory.resolve(".ledger.lock")));
    }

    @Test void reentrantTransactionRejectsBeforeOpeningAnotherChannel() throws Exception {
        var e = event("a", "A", 200); var owner = new DateVoteLedger(root); var successor = new DateVoteLedger(root);
        owner.record(e, player, UUID.randomUUID(), false);
        withFence(owner, () -> {
            assertEquals(3, probeLock(root.resolve(".ledger.lock")));
            assertThrows(IOException.class, () -> successor.record(e, player, UUID.randomUUID()));
            assertEquals(3, probeLock(root.resolve(".ledger.lock")));
            return null;
        });
        assertEquals(1, successor.progress(e, player).votes());
        assertEquals(0, probeLock(root.resolve(".ledger.lock")));
        assertThrows(IOException.class, () -> withFence(owner, () -> { throw new IOException("Fixture transaction failed"); }));
        assertEquals(0, probeLock(root.resolve(".ledger.lock")), "Failure must release the native lock");
        successor.record(e, player, UUID.randomUUID(), false);
        assertEquals(2, successor.progress(e, player).votes());
    }

    @Test void serialInstancesEnforceCurrentCapacityAndStillUpdateExistingRecords() throws Exception {
        var e = event("a", "A", 200); var first = DateVoteLedgerAccess.create(root, 2); var successor = DateVoteLedgerAccess.create(root, 2);
        UUID otherPlayer = UUID.randomUUID(), rejectedPlayer = UUID.randomUUID();
        first.record(e, player, UUID.randomUUID(), false);
        successor.record(e, otherPlayer, UUID.randomUUID(), false);
        assertThrows(IOException.class, () -> first.record(e, rejectedPlayer, UUID.randomUUID()));
        assertThrows(IOException.class, () -> DateVoteLedgerAccess.create(root, 2).record(e, rejectedPlayer, UUID.randomUUID()));
        assertFalse(Files.exists(root.resolve(e.fileId() + "-" + rejectedPlayer + ".properties")));
        first.record(e, player, UUID.randomUUID(), false);
        assertTrue(successor.reserve(e, player, 1)); successor.submitted(e, player, 1);
        assertEquals(2, DateVoteLedgerAccess.create(root, 2).progress(e, player).votes());
        assertEquals(Set.of(1), first.progress(e, player).submittedAwards());
        assertEquals(1, successor.progress(e, otherPlayer).votes());
        assertThrows(IllegalArgumentException.class, () -> DateVoteLedgerAccess.create(root, 0));
        assertThrows(IllegalArgumentException.class, () -> DateVoteLedgerAccess.create(root, 100_001));
    }

    private static Object withFence(Object ledger, Callable<?> action) throws Exception {
        Class<?> actionClass = Class.forName(DateVoteLedger.class.getName() + "$LedgerAction", true, ledger.getClass().getClassLoader());
        var method = ledger.getClass().getDeclaredMethod("withLedgerLock", actionClass); method.setAccessible(true);
        Object proxy = Proxy.newProxyInstance(actionClass.getClassLoader(), new Class<?>[] { actionClass },
                (ignored, called, arguments) -> action.call());
        return invoke(method, ledger, proxy);
    }
    private static Object invoke(java.lang.reflect.Method method, Object target, Object... arguments) throws Exception {
        try { return method.invoke(target, arguments); }
        catch (InvocationTargetException failure) {
            if (failure.getCause() instanceof Exception cause) throw cause;
            if (failure.getCause() instanceof Error cause) throw cause;
            throw new AssertionError(failure.getCause());
        }
    }
    private static URLClassLoader separateLedgerClassloader() {
        return new URLClassLoader(new URL[] { DateVoteLedger.class.getProtectionDomain().getCodeSource().getLocation() },
                DateVoteLedger.class.getClassLoader()) {
            @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                if (!name.equals(DateVoteLedger.class.getName()) && !name.startsWith(DateVoteLedger.class.getName() + "$"))
                    return super.loadClass(name, resolve);
                synchronized (getClassLoadingLock(name)) {
                    Class<?> loaded = findLoadedClass(name);
                    if (loaded == null) loaded = findClass(name);
                    if (resolve) resolveClass(loaded);
                    return loaded;
                }
            }
        };
    }
    private static Process lockProcess(Path file, String mode) throws IOException {
        String executable = System.getProperty("os.name", "").startsWith("Windows") ? "java.exe" : "java";
        return new ProcessBuilder(Path.of(System.getProperty("java.home"), "bin", executable).toString(), "-cp",
                System.getProperty("surefire.test.class.path", System.getProperty("java.class.path")),
                LockProbe.class.getName(), file.toString(), mode).redirectError(ProcessBuilder.Redirect.INHERIT).start();
    }
    private static int probeLock(Path file) throws Exception {
        Process probe = lockProcess(file, "probe");
        try {
            assertTrue(probe.waitFor(10, TimeUnit.SECONDS), "External lock probe timed out");
            return probe.exitValue();
        } finally { probe.destroyForcibly(); probe.waitFor(10, TimeUnit.SECONDS); }
    }
    public static final class LockProbe {
        public static void main(String[] arguments) throws Exception {
            int result;
            try (FileChannel channel = FileChannel.open(Path.of(arguments[0]), StandardOpenOption.WRITE);
                    FileLock lock = channel.tryLock()) {
                result = lock == null ? 3 : 0;
                if (lock != null && arguments[1].equals("hold")) {
                    System.out.write(1); System.out.flush(); System.in.read();
                }
            }
            System.exit(result);
        }
    }
    private DateVoteEvent event(String id, String name, long end) {
        return new DateVoteEvent(id, name, true, 100, end, "UTC", List.of(1, 2), Set.of(), "");
    }
    @Test void deferredThresholdPromotesOnceAcrossConcurrentReplayAndNeverDemotesAnUncertainReservation() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200); UUID occurrence = UUID.randomUUID();
        assertEquals(List.of(), ledger.record(e, player, occurrence, false));
        ledger = new DateVoteLedger(root); assertEquals(Set.of(1), ledger.progress(e, player).deferredAwards());
        final DateVoteLedger resumed = ledger;
        try (var pool = Executors.newFixedThreadPool(4)) {
            var jobs = java.util.stream.IntStream.range(0, 20).mapToObj(i -> (java.util.concurrent.Callable<List<Integer>>)
                    () -> {
                        var pending = resumed.record(e, player, occurrence, true);
                        return pending.stream().filter(threshold -> {
                            try { return resumed.reserve(e, player, threshold); }
                            catch (IOException failure) { throw new java.io.UncheckedIOException(failure); }
                        }).toList();
                    }).toList();
            int admitted = 0;
            for (var result : pool.invokeAll(jobs)) admitted += result.get().size();
            assertEquals(1, admitted);
        }
        assertEquals(List.of(), ledger.record(e, player, occurrence, false));
        assertEquals(Set.of(1), ledger.progress(e, player).reservedAwards());
        assertTrue(ledger.progress(e, player).deferredAwards().isEmpty()); assertEquals(1, ledger.progress(e, player).votes());
        assertEquals(List.of(), new DateVoteLedger(root).record(e, player, occurrence, true));
    }
    @Test void duplicateAndRestartNeverResubmitReservedOrSubmittedAwards() throws Exception {
        var e = event("a", "A", 200); var ledger = new DateVoteLedger(root); UUID id = UUID.randomUUID();
        assertEquals(List.of(1), ledger.record(e, player, id));
        assertTrue(ledger.reserve(e, player, 1));
        ledger = new DateVoteLedger(root);
        assertEquals(List.of(), ledger.record(e, player, id));
        assertEquals(Set.of(1), ledger.progress(e, player).reservedAwards());
        ledger.submitted(e, player, 1); ledger.submitted(e, player, 1);
        ledger = new DateVoteLedger(root);
        assertEquals(List.of(2), ledger.record(e, player, UUID.randomUUID()));
        assertEquals(Set.of(1), ledger.progress(e, player).submittedAwards());
        assertEquals(2, ledger.progress(e, player).votes());
    }
    @Test void overlapsCountOnceForEachEventButUnrelatedPlayersAreSeparate() throws Exception {
        var ledger = new DateVoteLedger(root); UUID occurrence = UUID.randomUUID();
        assertEquals(List.of(1), ledger.record(event("a", "A", 200), player, occurrence));
        assertEquals(List.of(1), ledger.record(event("b", "B", 300), player, occurrence));
        assertEquals(List.of(1), ledger.record(event("a", "A", 200), UUID.randomUUID(), occurrence));
    }
    @Test void sameOccurrenceConcurrentRequestsReserveExactlyOnce() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200); UUID occurrence = UUID.randomUUID();
        try (var pool = Executors.newFixedThreadPool(4)) {
            var jobs = java.util.stream.IntStream.range(0, 20).mapToObj(i -> (java.util.concurrent.Callable<List<Integer>>)
                    () -> {
                        var pending = ledger.record(e, player, occurrence);
                        return pending.stream().filter(threshold -> {
                            try { return ledger.reserve(e, player, threshold); }
                            catch (IOException failure) { throw new java.io.UncheckedIOException(failure); }
                        }).toList();
                    }).toList();
            int awarded = 0;
            for (var result : pool.invokeAll(jobs)) awarded += result.get().size();
            assertEquals(1, awarded); assertEquals(1, ledger.progress(e, player).votes());
        }
    }
    @Test void immutableContractIsSealedAcrossPlayersAndDisplayRenames() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        assertEquals(1, ledger.progress(event("a", "Renamed", 200), player).votes());
        assertThrows(IOException.class, () -> ledger.record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> ledger.progress(event("a", "A", 300), player));
    }
    @Test void malformedStateFailsClosedWithoutResettingOrRewards() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        Path file = root.resolve(e.fileId() + "-" + player + ".properties");
        String original = Files.readString(file);
        for (String corruption : List.of("", "seen=\nfingerprint=bad\n", original + "seen=\n", original + "unknown=value\n")) {
            Files.writeString(file, corruption);
            assertThrows(IOException.class, () -> new DateVoteLedger(root).record(e, player, UUID.randomUUID()));
            assertEquals(corruption, Files.readString(file));
        }
    }
    @Test void missingReachedAwardFailsClosedForReadsWritesAndAcknowledgements() throws Exception {
        var e = event("a", "A", 200); var ledger = new DateVoteLedger(root);
        ledger.record(e, player, UUID.randomUUID()); assertTrue(ledger.reserve(e, player, 1)); ledger.submitted(e, player, 1);
        Path file = root.resolve(e.fileId() + "-" + player + ".properties");
        String broken = Files.readString(file).replace("award.1=SUBMITTED\n", "");
        Files.writeString(file, broken);
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(e, player, UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(e, player));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).submitted(e, player, 1));
        assertEquals(broken, Files.readString(file));
    }
    @Test void caseDistinctIdsHaveDistinctPortableFilesAndIndependentProgress() throws Exception {
        var first = event("Festival", "First", 200); var second = event("festival", "Second", 200);
        var ledger = new DateVoteLedger(root); UUID occurrence = UUID.randomUUID();
        assertNotEquals(first.fileId().toLowerCase(), second.fileId().toLowerCase());
        assertEquals(List.of(1), ledger.record(first, player, occurrence));
        assertEquals(List.of(1), ledger.record(second, player, occurrence));
        assertEquals(1, new DateVoteLedger(root).progress(first, player).votes());
        assertEquals(1, new DateVoteLedger(root).progress(second, player).votes());
    }
    @Test void delimiterContainingFilterCannotChangeASealedContract() throws Exception {
        var a = new DateVoteEvent("event", "A", true, 100, 200, "UTC", List.of(1), Set.of("a", "b"), "");
        var b = new DateVoteEvent("event", "B", true, 100, 200, "UTC", List.of(1), Set.of("a, b"), "");
        var ledger = new DateVoteLedger(root); ledger.record(a, player, UUID.randomUUID());
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(b, player, UUID.randomUUID()));
    }
    @Test void truncatedDefinitionsCannotResetSealedContractsForNewPlayers() throws Exception {
        var ledger = new DateVoteLedger(root); var e = event("a", "A", 200);
        ledger.record(e, player, UUID.randomUUID());
        Path definitions = root.resolve("definitions.properties"); Files.writeString(definitions, "");
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(e, player));
        assertEquals("", Files.readString(definitions));
    }
    @Test void missingDefinitionsCannotResealSurvivingHistoryForANewPlayer() throws Exception {
        var ledger = new DateVoteLedger(root); var original = event("a", "A", 200);
        ledger.record(original, player, UUID.randomUUID());
        Files.delete(root.resolve("definitions.properties"));
        var changed = event("a", "A", 300); UUID newcomer = UUID.randomUUID();
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(changed, newcomer, UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(changed, newcomer));
        assertFalse(Files.exists(root.resolve("definitions.properties")));
        assertFalse(Files.exists(root.resolve(changed.fileId() + "-" + newcomer + ".properties")));
    }
    @Test void partialDefinitionsCannotResealAnOmittedHistoricalEvent() throws Exception {
        var ledger = new DateVoteLedger(root); var first = event("a", "A", 200); var second = event("b", "B", 200);
        ledger.record(first, player, UUID.randomUUID()); ledger.record(second, player, UUID.randomUUID());
        Path definitions = root.resolve("definitions.properties");
        String partial = Files.readString(definitions).replace("a=" + first.fingerprint() + "\n", "");
        Files.writeString(definitions, partial);
        assertThrows(IOException.class, () -> new DateVoteLedger(root).record(event("a", "A", 300), UUID.randomUUID(), UUID.randomUUID()));
        assertThrows(IOException.class, () -> new DateVoteLedger(root).progress(first, UUID.randomUUID()));
        assertEquals(partial, Files.readString(definitions));
        assertEquals(1, new DateVoteLedger(root).progress(second, player).votes());
    }
    @Test void newIdsWithNoHistoryRemainUsableAlongsideSealedEvents() throws Exception {
        var ledger = new DateVoteLedger(root); ledger.record(event("a", "A", 200), player, UUID.randomUUID());
        assertEquals(List.of(1), new DateVoteLedger(root).record(event("new", "New", 300), player, UUID.randomUUID()));
    }
    @Test void unsafeDirectoryAndUnavailableStorageNeverAdmitAwards() throws Exception {
        Path file = root.resolve("file"); Files.writeString(file, "keep");
        assertThrows(IOException.class, () -> new DateVoteLedger(file).record(event("a", "A", 200), player, UUID.randomUUID()));
        Path link = root.resolve("link"); Files.createSymbolicLink(link, root);
        assertThrows(IOException.class, () -> new DateVoteLedger(link).record(event("a", "A", 200), player, UUID.randomUUID()));
        assertEquals("keep", Files.readString(file));
    }
}
