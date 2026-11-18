package com.bencodez.votingplugin.fabric;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;
import java.util.function.BooleanSupplier;
import java.util.function.Function;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Runs live player lookups on the owning server's tick thread. */
final class FabricServerRuntime implements AutoCloseable {
    private static final int MAX_PENDING = 256;
    private static final int LOOKUPS_PER_TICK = 32;

    private final BooleanSupplier onServerThread;
    private final Function<UUID, Optional<SharedVoteIdentity>> lookup;
    private final Queue<Request> pending = new ArrayDeque<>();
    private boolean closed;

    FabricServerRuntime(BooleanSupplier onServerThread, Function<UUID, Optional<SharedVoteIdentity>> lookup) {
        this.onServerThread = onServerThread;
        this.lookup = lookup;
    }

    synchronized CompletableFuture<Optional<SharedVoteIdentity>> onlinePlayer(UUID uuid) {
        Objects.requireNonNull(uuid, "uuid");
        if (closed || pending.size() >= MAX_PENDING) {
            return CompletableFuture.failedFuture(new RejectedExecutionException(
                    closed ? "Fabric server runtime has stopped" : "Fabric player lookup queue is full"));
        }
        var result = new CompletableFuture<Optional<SharedVoteIdentity>>();
        pending.add(new Request(uuid, result));
        return result;
    }

    void tick() {
        requireServerThread();
        for (int count = 0; count < LOOKUPS_PER_TICK; count++) {
            Request request;
            synchronized (this) {
                request = pending.poll();
            }
            if (request == null) {
                return;
            }
            if (!request.result().isDone()) {
                try {
                    request.result().complete(lookup.apply(request.uuid()));
                } catch (RuntimeException failure) {
                    request.result().completeExceptionally(failure);
                }
            }
        }
    }

    @Override
    public void close() {
        requireServerThread();
        List<Request> abandoned;
        synchronized (this) {
            closed = true;
            abandoned = new ArrayList<>(pending);
            pending.clear();
        }
        for (Request request : abandoned) {
            request.result().completeExceptionally(new RejectedExecutionException("Fabric server runtime has stopped"));
        }
    }

    private void requireServerThread() {
        if (!onServerThread.getAsBoolean()) {
            throw new IllegalStateException("Expected the Fabric server thread");
        }
    }

    private record Request(UUID uuid, CompletableFuture<Optional<SharedVoteIdentity>> result) {
    }
}
