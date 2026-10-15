package com.bencodez.votingplugin.util;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.Locale;
import java.util.Properties;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

/** Memory-only admission for participating plugin generations sharing one server-data path in a JVM. */
public final class ServerDataPublicationFence {
    private static final String PREFIX = "com.bencodez.votingplugin.ServerDataPublicationFence.owner.";
    private static final int MAX_OWNERS = 1024;
    private final Properties registry;
    private final String key;
    private final String generation;
    private final AtomicBoolean retirementStarted = new AtomicBoolean();
    private final java.util.concurrent.CompletableFuture<Void> retirementCompleted = new java.util.concurrent.CompletableFuture<>();

    private ServerDataPublicationFence(Properties registry, String key, String generation) {
        this.registry = registry;
        this.key = key;
        this.generation = generation;
    }

    /** Runs before platform initialization and performs no filesystem operation or wait for storage. */
    public static ServerDataPublicationFence beginGeneration(Path file) {
        String identity = file.toAbsolutePath().normalize().toString();
        if (DurableFiles.isWindowsName(System.getProperty("os.name", ""))) identity = identity.toLowerCase(Locale.ROOT);
        String key;
        try {
            key = PREFIX + HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
                    .digest(identity.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException unavailable) { throw new IllegalStateException(unavailable); }
        Properties registry = System.getProperties();
        synchronized (registry) {
            Object value = registry.get(key);
            if (value != null) {
                State previous = state(value);
                if (previous.users != 0 || !previous.retiring)
                    throw new IllegalStateException("Previous server-data generation still owns storage work; replacement was not started");
            } else if (registry.keySet().stream().filter(k -> k instanceof String s && s.startsWith(PREFIX)).count() >= MAX_OWNERS) {
                throw new IllegalStateException("Server-data publication owner capacity is unavailable");
            }
            String generation = UUID.randomUUID().toString();
            registry.setProperty(key, generation + ":0:false");
            return new ServerDataPublicationFence(registry, key, generation);
        }
    }

    /** Hold through the entire operation, including a blocked write or admitted shutdown handoff. */
    public Lease acquire() {
        synchronized (registry) {
            State current = current();
            if (current.users >= MAX_OWNERS) throw new IllegalStateException("Server-data publication admission is unavailable");
            registry.setProperty(key, generation + ":" + (current.users + 1) + ":" + current.retiring);
            return new Lease(this);
        }
    }

    /** Existing operations/retries retain ownership; a successor requires zero outstanding leases. */
    public void retire() {
        synchronized (registry) {
            State current = state(registry.get(key));
            if (generation.equals(current.generation)) registry.setProperty(key, generation + ":" + current.users + ":true");
        }
    }

    /** Reserve before disable and release only after existing SDK writers actually terminate. */
    public java.util.concurrent.CompletionStage<Void> retainUntilTermination(java.util.concurrent.ScheduledExecutorService[] writers) {
        var snapshot = writers.clone();
        if (!retirementStarted.compareAndSet(false, true)) return retirementCompleted.minimalCompletionStage();
        Lease lease;
        try { lease = acquire(); }
        catch (RuntimeException | Error failure) { retirementStarted.set(false); throw failure; }
        try {
            Thread guardian = new Thread(() -> {
                boolean interrupted = false;
                try (lease) {
                    for (var writer : snapshot) {
                        if (writer == null) continue;
                        while (!writer.isTerminated()) {
                            try { writer.awaitTermination(100, java.util.concurrent.TimeUnit.MILLISECONDS); }
                            catch (InterruptedException ignored) { interrupted = true; }
                        }
                    }
                } catch (RuntimeException | Error failure) {
                    retirementCompleted.completeExceptionally(failure);
                    return;
                } finally { if (interrupted) Thread.currentThread().interrupt(); }
                retirementCompleted.complete(null);
            }, "VotingPlugin-server-data-retirement");
            guardian.setDaemon(true);
            guardian.start();
        } catch (RuntimeException | Error failure) {
            try { lease.close(); }
            catch (RuntimeException | Error cleanupFailure) { if (cleanupFailure != failure) failure.addSuppressed(cleanupFailure); }
            finally { retirementStarted.set(false); }
            throw failure;
        }
        return retirementCompleted.minimalCompletionStage();
    }

    private State current() {
        State current = state(registry.get(key));
        if (registry != System.getProperties() || !generation.equals(current.generation))
            throw new IllegalStateException("Server-data publication belongs to another generation");
        return current;
    }

    private static State state(Object value) {
        if (!(value instanceof String text) || text.length() > 64)
            throw new IllegalStateException("Server-data publication ownership is invalid");
        String[] fields = text.split(":", -1);
        try {
            if (fields.length != 3 || !UUID.fromString(fields[0]).toString().equals(fields[0])
                    || !(fields[2].equals("true") || fields[2].equals("false"))) throw new IllegalArgumentException();
            int users = Integer.parseInt(fields[1]);
            if (users < 0 || users > MAX_OWNERS || !Integer.toString(users).equals(fields[1])) throw new IllegalArgumentException();
            return new State(fields[0], users, Boolean.parseBoolean(fields[2]));
        } catch (IllegalArgumentException invalid) {
            throw new IllegalStateException("Server-data publication ownership is invalid");
        }
    }

    private record State(String generation, int users, boolean retiring) { }

    public static final class Lease implements AutoCloseable {
        private final ServerDataPublicationFence owner;
        private final AtomicBoolean closed = new AtomicBoolean();
        private Lease(ServerDataPublicationFence owner) { this.owner = owner; }
        @Override public void close() {
            synchronized (owner.registry) {
                if (closed.get()) return;
                State current = owner.current();
                if (current.users <= 0) throw new IllegalStateException("Server-data publication ownership is invalid");
                owner.registry.setProperty(owner.key, owner.generation + ":" + (current.users - 1) + ":" + current.retiring);
                closed.set(true);
            }
        }
    }
}
