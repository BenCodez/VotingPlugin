package com.bencodez.votingplugin.neoforge;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.logging.Logger;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

/** Shared durable NeoForge vote/presence protocol. Only transport adapters supply verified envelopes. */
final class NeoForgeProxyMessageService implements AutoCloseable {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeProxyMessageService.class.getName());
    private static final int SNAPSHOT_CHUNK_SIZE = 100;
    private static final int MAX_PENDING_SENDS = 256;
    private static final long HEARTBEAT_SECONDS = 30;

    private final String server;
    private final NeoForgeVoteProcessor processor;
    private final NeoForgePlayerDirectory players;
    private final Consumer<JsonEnvelope> outbound;
    private final Runnable transportClose;
    private final ThreadPoolExecutor sender;
    private final ScheduledExecutorService heartbeat;
    private final Map<UUID, BackendPlayerPresenceSession> sessions = new ConcurrentHashMap<>();
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final AtomicBoolean presenceDirty = new AtomicBoolean();
    private final AtomicBoolean broadcastWarningLogged = new AtomicBoolean();
    private final Object presenceLock = new Object();
    private final Object sendLock = new Object();
    private final Object timestampLock = new Object();
    private final UUID incarnationId = UUID.randomUUID();
    private final long startedAt = System.currentTimeMillis();
    private long lastTimestamp = startedAt;

    NeoForgeProxyMessageService(String server, NeoForgeVoteProcessor processor,
            NeoForgePlayerDirectory players, Consumer<JsonEnvelope> outbound,
            Runnable transportClose, boolean scheduleHeartbeat) {
        this.server = Objects.requireNonNull(server, "server");
        this.processor = Objects.requireNonNull(processor, "processor");
        this.players = Objects.requireNonNull(players, "players");
        this.outbound = Objects.requireNonNull(outbound, "outbound");
        this.transportClose = Objects.requireNonNull(transportClose, "transportClose");
        heartbeat = scheduleHeartbeat ? Executors.newSingleThreadScheduledExecutor(task -> {
            Thread thread = new Thread(task, "VotingPlugin-NeoForge-Presence");
            thread.setDaemon(true);
            return thread;
        }) : null;
        sender = scheduleHeartbeat ? new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS,
                new ArrayBlockingQueue<>(MAX_PENDING_SENDS), task -> {
                    Thread thread = new Thread(task, "VotingPlugin-NeoForge-Send");
                    thread.setDaemon(true);
                    return thread;
                }, new ThreadPoolExecutor.AbortPolicy()) : null;
        for (SharedVoteIdentity identity : players.onlineIdentities()) addSession(identity);
        announceStarted();
        sendHeartbeat();
        sessions.values().forEach(this::sendLogin);
        if (heartbeat != null) heartbeat.scheduleAtFixedRate(this::sendHeartbeat,
                HEARTBEAT_SECONDS, HEARTBEAT_SECONDS, TimeUnit.SECONDS);
    }

    boolean isOpen() { return open.get(); }

    void transportReady() {
        synchronized (presenceLock) {
            if (!open.get()) return;
            announceStarted();
            sendHeartbeat();
            sessions.values().forEach(this::sendLogin);
        }
    }

    public void playerOnline(SharedVoteIdentity identity) {
        synchronized (presenceLock) {
            if (!open.get()) return;
            BackendPlayerPresenceSession session = addSession(identity);
            announceStarted();
            sendLogin(session);
        }
    }

    public void playerOffline(SharedVoteIdentity identity) {
        synchronized (presenceLock) {
            if (!open.get()) return;
            BackendPlayerPresenceSession session = sessions.remove(identity.uuid());
            if (session == null) return;
            send(VotingPluginWire.logout(session.getPlayerName(), session.getUuid(), server,
                    session.getConnectionId(), incarnationId, startedAt, nextTimestamp()));
        }
    }

    /** True only after a supported envelope has reached its confirmed application boundary. */
    boolean receive(JsonEnvelope envelope) {
        if (!open.get() || envelope == null) return false;
        if (envelope.getSchema() != VotingPluginWire.SCHEMA_VERSION) return false;
        try {
            return switch (envelope.getSubChannel()) {
                case VotingPluginWire.SUB_VOTE, VotingPluginWire.SUB_VOTE_ONLINE -> receiveVote(envelope);
                case VotingPluginWire.SUB_STATUS -> receiveStatus(envelope);
                case VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE -> receiveReceiptRelease(envelope);
                case VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST -> receiveResync(envelope);
                case VotingPluginWire.SUB_PRESENCE_SNAPSHOT_REQUEST -> receiveSnapshotRequest(envelope);
                default -> false;
            };
        } catch (RuntimeException failure) {
            LOGGER.warning("NeoForge proxy message " + safeSubchannel(envelope)
                    + " failed: " + failure.getClass().getSimpleName());
            return false;
        }
    }

    private boolean receiveVote(JsonEnvelope envelope) {
        VotingPluginWire.Vote vote = VotingPluginWire.readVote(envelope);
        if (vote.voteId == null || vote.player.isBlank() || vote.player.length() > 64
                || !ServiceSiteValidator.isValid(vote.service)) return false;
        // Backend broadcast delivery is not supported by the NeoForge replay
        // boundary. Leave the occurrence unacknowledged in the proxy durable
        // outbox instead of silently completing an omitted side effect.
        if (vote.broadcast) {
            if (broadcastWarningLogged.compareAndSet(false, true)) {
                LOGGER.warning("NeoForge cannot execute delegated vote broadcasts; deliveries remain unacknowledged. "
                        + "Exclude this backend from proxy broadcast routing for new votes.");
            }
            return false;
        }
        UUID playerId;
        try {
            playerId = UUID.fromString(vote.uuid);
        } catch (IllegalArgumentException invalid) {
            return false;
        }
        // NeoForge does not yet import a proxy-owned totals snapshot. Fail closed
        // instead of silently completing a vote with incorrect totals.
        if (vote.manageTotals || !vote.setTotals || vote.numberOfVotes < 1
                || vote.num < 1 || vote.num > vote.numberOfVotes) return false;
        boolean currentlyOnline = players.online(playerId).isPresent();
        boolean wasOnline = vote.wasOnlineKnown ? vote.wasOnline : currentlyOnline;
        NeoForgeVoteResult result = processor.process(new NeoForgeVoteRequest(vote.voteId, playerId,
                vote.player, vote.service, vote.time, vote.realVote, true,
                currentlyOnline, wasOnline, NeoForgeVoteRequest.Scope.COMPLETE));
        boolean retained = result.durablyRetained() || result.durablyCompleted();
        if (VotingPluginWire.requestsVoteDeliveryAcknowledgement(envelope) && retained) {
            send(VotingPluginWire.voteDeliveryAcknowledgement(server, vote.voteId,
                    envelope.getSubChannel()));
        }
        return retained;
    }

    private boolean receiveStatus(JsonEnvelope envelope) {
        if (!isTargetedAtThisBackend(envelope)) return false;
        String raw = envelope.getFields().getOrDefault(VotingPluginWire.K_REQUEST_ID, "");
        if (raw.isBlank()) {
            send(VotingPluginWire.statusOkay(server));
            return true;
        }
        try {
            send(VotingPluginWire.statusOkay(server, UUID.fromString(raw)));
        } catch (IllegalArgumentException invalid) {
            // A malformed probe receives no capability response.
            return false;
        }
        return true;
    }

    private boolean receiveReceiptRelease(JsonEnvelope envelope) {
        if (!isTargetedAtThisBackend(envelope)
                || !VotingPluginWire.requestsVoteDeliveryAcknowledgement(envelope)) return false;
        String voteSubchannel = envelope.getFields().getOrDefault(
                VotingPluginWire.K_VOTE_DELIVERY_SUBCHANNEL, "");
        if (!VotingPluginWire.SUB_VOTE.equals(voteSubchannel)
                && !VotingPluginWire.SUB_VOTE_ONLINE.equals(voteSubchannel)) return false;
        UUID voteId;
        UUID playerId;
        try {
            voteId = UUID.fromString(envelope.getFields().getOrDefault(VotingPluginWire.K_VOTE_ID, ""));
            playerId = UUID.fromString(envelope.getFields().getOrDefault(VotingPluginWire.K_UUID, ""));
        } catch (IllegalArgumentException invalid) {
            return false;
        }
        NeoForgeDeferredVoteStore.ReleaseResult result = processor.deferredVotes().release(playerId, voteId);
        if (result != NeoForgeDeferredVoteStore.ReleaseResult.RELEASED
                && result != NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED) return false;
        send(VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(
                server, voteId, voteSubchannel));
        return true;
    }

    private boolean receiveResync(JsonEnvelope envelope) {
        VotingPluginWire.PresenceResyncRequest request = VotingPluginWire.readPresenceResyncRequest(envelope);
        if (request.requestId == null || request.requestedAt <= 0L
                || !server.equalsIgnoreCase(request.server)) return false;
        announceStarted();
        return true;
    }

    private boolean receiveSnapshotRequest(JsonEnvelope envelope) {
        VotingPluginWire.PresenceSnapshotRequest request = VotingPluginWire.readPresenceSnapshotRequest(envelope);
        if (request.requestId == null || !server.equalsIgnoreCase(request.server)
                || !incarnationId.equals(request.backendIncarnationId)
                || request.backendStartedAt != startedAt || request.presenceTimestamp <= 0L) return false;
        synchronized (presenceLock) {
            List<VotingPluginWire.PresencePlayer> snapshot = sessions.values().stream()
                    .map(session -> new VotingPluginWire.PresencePlayer(session.getPlayerName(), session.getUuid(),
                            session.getConnectionId().toString()))
                    .toList();
            int chunks = Math.max(1, (snapshot.size() + SNAPSHOT_CHUNK_SIZE - 1) / SNAPSHOT_CHUNK_SIZE);
            long timestamp = nextTimestamp();
            for (int index = 0; index < chunks; index++) {
                int from = index * SNAPSHOT_CHUNK_SIZE;
                int to = Math.min(snapshot.size(), from + SNAPSHOT_CHUNK_SIZE);
                send(VotingPluginWire.presenceSnapshot(server, request.requestId, index, chunks,
                        snapshot.subList(from, to), incarnationId, startedAt, timestamp));
            }
        }
        return true;
    }

    private BackendPlayerPresenceSession addSession(SharedVoteIdentity identity) {
        BackendPlayerPresenceSession session = BackendPlayerPresenceSession.create(
                identity.playerName(), identity.uuid().toString());
        if (session == null) throw new IllegalArgumentException("Invalid player identity");
        sessions.put(identity.uuid(), session);
        return session;
    }

    private void sendLogin(BackendPlayerPresenceSession session) {
        synchronized (presenceLock) {
            send(VotingPluginWire.login(session.getPlayerName(), session.getUuid(), server,
                    session.getConnectionId(), incarnationId, startedAt, nextTimestamp()));
        }
    }

    private void sendHeartbeat() {
        synchronized (presenceLock) {
            if (!open.get()) return;
            announceStarted();
            send(VotingPluginWire.backendHeartbeat(server, incarnationId,
                    startedAt, nextTimestamp()));
        }
    }

    private void announceStarted() {
        synchronized (presenceLock) {
            send(VotingPluginWire.backendStarted(server, incarnationId, startedAt, startedAt));
        }
    }

    private boolean isTargetedAtThisBackend(JsonEnvelope envelope) {
        return server.equalsIgnoreCase(
                envelope.getFields().getOrDefault(VotingPluginWire.K_SERVER, ""));
    }

    private long nextTimestamp() {
        synchronized (timestampLock) {
            lastTimestamp = Math.max(System.currentTimeMillis(), lastTimestamp + 1L);
            return lastTimestamp;
        }
    }

    private void send(JsonEnvelope envelope) {
        if (!open.get()) return;
        if (sender != null) {
            JsonEnvelope queued = envelope;
            boolean presence = isPresenceEnvelope(queued);
            try {
                sender.execute(() -> {
                    try {
                        sendNow(queued);
                    } catch (RuntimeException failure) {
                        if (presence) presenceDirty.set(true);
                        LOGGER.warning("Proxy send failed: " + failure.getClass().getSimpleName());
                    } finally {
                        recoverPresenceIfDrained();
                    }
                });
            } catch (RejectedExecutionException full) {
                if (presence) presenceDirty.set(true);
                LOGGER.warning("Send queue full; delivery rejected");
            }
            return;
        }
        sendNow(envelope);
    }

    private void recoverPresenceIfDrained() {
        if (!open.get() || !presenceDirty.get() || !sender.getQueue().isEmpty()
                || !presenceDirty.compareAndSet(true, false)) return;
        try {
            synchronized (presenceLock) {
                sendNow(VotingPluginWire.backendStarted(server, incarnationId,
                        startedAt, nextTimestamp()));
            }
        } catch (RuntimeException failure) {
            presenceDirty.set(true);
            LOGGER.warning("Presence recovery send failed: " + failure.getClass().getSimpleName());
        }
    }

    private static boolean isPresenceEnvelope(JsonEnvelope envelope) {
        return switch (envelope.getSubChannel()) {
            case VotingPluginWire.SUB_LOGIN, VotingPluginWire.SUB_LOGOUT,
                    VotingPluginWire.SUB_BACKEND_STARTED, VotingPluginWire.SUB_PRESENCE_SNAPSHOT -> true;
            default -> false;
        };
    }

    private void sendNow(JsonEnvelope envelope) {
        synchronized (sendLock) {
            outbound.accept(envelope);
        }
    }

    @Override public void close() {
        if (!open.compareAndSet(true, false)) return;
        if (heartbeat != null) heartbeat.shutdownNow();
        JsonEnvelope stopped;
        synchronized (presenceLock) {
            stopped = VotingPluginWire.backendStopped(server, incarnationId,
                    startedAt, nextTimestamp());
            sessions.clear();
        }
        if (sender != null) {
            try {
                sender.execute(() -> sendStopped(stopped));
            } catch (RejectedExecutionException full) {
                LOGGER.warning("Stopped-presence send rejected");
            }
            sender.shutdown();
            try {
                if (!sender.awaitTermination(1, TimeUnit.SECONDS)) sender.shutdownNow();
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                sender.shutdownNow();
            }
        } else {
            sendStopped(stopped);
        }
        transportClose.run();
    }

    private void sendStopped(JsonEnvelope stopped) {
        try {
            sendNow(stopped);
        } catch (RuntimeException failure) {
            LOGGER.warning("Stopped-presence send failed: "
                    + failure.getClass().getSimpleName());
        }
    }

    private static String safeSubchannel(JsonEnvelope envelope) {
        String value = envelope.getSubChannel();
        if (value == null) return "unknown";
        return value.replaceAll("[^A-Za-z0-9._-]", "?").substring(0, Math.min(value.length(), 64));
    }
}
