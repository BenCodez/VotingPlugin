package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.file.Path;
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
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.logging.Logger;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.simpleapi.servercomm.sockets.SocketServer;
import com.bencodez.votingplugin.backendproxy.presence.BackendPlayerPresenceSession;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

/** NeoForge receiver for the existing encrypted VotingPlugin SOCKETS proxy transport. */
public final class NeoForgeProxySocketService implements AutoCloseable {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeProxySocketService.class.getName());
    private static final int SNAPSHOT_CHUNK_SIZE = 100;
    private static final int MAX_PENDING_SENDS = 256;
    private static final long HEARTBEAT_SECONDS = 30;

    private final NeoForgeProxySocketConfiguration configuration;
    private final NeoForgeVoteProcessor processor;
    private final NeoForgePlayerDirectory players;
    private final Consumer<JsonEnvelope> outbound;
    private final SharedTransportEnvelopeAuthenticator authenticator;
    private final TransportEnvelopeEncryption encryption;
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

    static NeoForgeProxySocketService start(Path directory, NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players) {
        Objects.requireNonNull(directory, "directory");
        EncryptionHandler encryption = new EncryptionHandler("VotingPlugin",
                directory.resolve("secretkey.key").toFile());
        SharedTransportEnvelopeAuthenticator authenticator;
        TransportEnvelopeEncryption transportEncryption;
        try {
            Path root = directory.toAbsolutePath().normalize();
            Path keyFile = root.resolve(configuration.authenticationKeyFile()).normalize();
            if (!keyFile.startsWith(root)) throw new IOException("Socket key outside data directory");
            authenticator = SharedTransportEnvelopeAuthenticator.load(keyFile, Mode.REQUIRED);
            transportEncryption = TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                    TransportEnvelopeEncryption.Domain.PROXY_BACKEND,
                    configuration.communicationEncryption());
        } catch (IOException unavailable) {
            throw new IllegalStateException("Socket authentication unavailable", unavailable);
        }
        verifyListenerPortAvailable(configuration.backendHost(), configuration.backendPort());
        ClientHandler client = new ClientHandler(configuration.proxyHost(), configuration.proxyPort(),
                encryption, configuration.debug());
        SocketHandler listener;
        try {
            listener = new SocketHandler("vp-neoforge-socket", configuration.backendHost(),
                    configuration.backendPort(), encryption, configuration.debug()) {
                @Override public void log(String message) { LOGGER.info(message); }
            };
        } catch (RuntimeException failure) {
            client.stopConnection();
            throw failure;
        }
        AtomicReference<NeoForgeProxySocketService> service = new AtomicReference<>();
        listener.add(new SocketReceiver() {
            @Override public void onReceiveEnvelope(JsonEnvelope envelope) {
                NeoForgeProxySocketService active = service.get();
                if (active != null) active.receive(envelope);
            }
        });
        Runnable close = () -> closeTransport(listener, client);
        try {
            NeoForgeProxySocketService active = new NeoForgeProxySocketService(configuration, processor, players,
                    client::sendEnvelope, authenticator, transportEncryption, close, true);
            service.set(active);
            return active;
        } catch (RuntimeException failure) {
            close.run();
            throw failure;
        }
    }

    NeoForgeProxySocketService(NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players,
            Consumer<JsonEnvelope> outbound, SharedTransportEnvelopeAuthenticator authenticator) {
        this(configuration, processor, players, outbound, authenticator,
                TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND),
                () -> { }, false);
    }

    NeoForgeProxySocketService(NeoForgeProxySocketConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players,
            Consumer<JsonEnvelope> outbound,
            SharedTransportEnvelopeAuthenticator authenticator,
            TransportEnvelopeEncryption encryption,
            Runnable transportClose, boolean scheduleHeartbeat) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        this.processor = Objects.requireNonNull(processor, "processor");
        this.players = Objects.requireNonNull(players, "players");
        this.outbound = Objects.requireNonNull(outbound, "outbound");
        this.authenticator = Objects.requireNonNull(authenticator, "authenticator");
        this.encryption = Objects.requireNonNull(encryption, "encryption");
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
            send(VotingPluginWire.logout(session.getPlayerName(), session.getUuid(), configuration.server(),
                    session.getConnectionId(), incarnationId, startedAt, nextTimestamp()));
        }
    }

    void receive(JsonEnvelope envelope) {
        if (!open.get() || envelope == null) return;
        String sender = envelope.getFields().get(SharedTransportEnvelopeAuthenticator.K_SENDER);
        SharedTransportEnvelopeAuthenticator.Verification verification = authenticator.verify(envelope,
                Domain.SOCKET_PROXY_BACKEND, configuration.server());
        if (!verification.accepted() || !configuration.proxyName().equals(sender)) {
            LOGGER.warning("Rejected unauthenticated socket message");
            return;
        }
        TransportEnvelopeEncryption.Decryption decryption = encryption.decrypt(verification.envelope());
        if (!decryption.accepted()) {
            LOGGER.warning("Rejected invalid encrypted socket message");
            return;
        }
        envelope = decryption.envelope();
        if (envelope.getSchema() != VotingPluginWire.SCHEMA_VERSION) return;
        try {
            switch (envelope.getSubChannel()) {
                case VotingPluginWire.SUB_VOTE, VotingPluginWire.SUB_VOTE_ONLINE -> receiveVote(envelope);
                case VotingPluginWire.SUB_STATUS -> receiveStatus(envelope);
                case VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE -> receiveReceiptRelease(envelope);
                case VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST -> receiveResync(envelope);
                case VotingPluginWire.SUB_PRESENCE_SNAPSHOT_REQUEST -> receiveSnapshotRequest(envelope);
                default -> { }
            }
        } catch (RuntimeException failure) {
            LOGGER.warning("NeoForge proxy message " + safeSubchannel(envelope)
                    + " failed: " + failure.getClass().getSimpleName());
        }
    }

    private void receiveVote(JsonEnvelope envelope) {
        VotingPluginWire.Vote vote = VotingPluginWire.readVote(envelope);
        if (vote.voteId == null || vote.player.isBlank() || vote.player.length() > 64
                || !ServiceSiteValidator.isValid(vote.service)) return;
        // Backend broadcast delivery is not supported by the NeoForge replay
        // boundary. Leave the occurrence unacknowledged in the proxy durable
        // outbox instead of silently completing an omitted side effect.
        if (vote.broadcast) {
            if (broadcastWarningLogged.compareAndSet(false, true)) {
                LOGGER.warning("NeoForge cannot execute delegated vote broadcasts; deliveries remain unacknowledged. "
                        + "Exclude this backend from proxy broadcast routing for new votes.");
            }
            return;
        }
        UUID playerId;
        try {
            playerId = UUID.fromString(vote.uuid);
        } catch (IllegalArgumentException invalid) {
            return;
        }
        // NeoForge does not yet import a proxy-owned totals snapshot. Fail closed
        // instead of silently completing a vote with incorrect totals.
        if (vote.manageTotals || !vote.setTotals || vote.numberOfVotes < 1
                || vote.num < 1 || vote.num > vote.numberOfVotes) return;
        boolean currentlyOnline = players.online(playerId).isPresent();
        boolean wasOnline = vote.wasOnlineKnown ? vote.wasOnline : currentlyOnline;
        NeoForgeVoteResult result = processor.process(new NeoForgeVoteRequest(vote.voteId, playerId,
                vote.player, vote.service, vote.time, vote.realVote, true,
                currentlyOnline, wasOnline, NeoForgeVoteRequest.Scope.COMPLETE));
        if (VotingPluginWire.requestsVoteDeliveryAcknowledgement(envelope)
                && (result.durablyRetained() || result.durablyCompleted())) {
            send(VotingPluginWire.voteDeliveryAcknowledgement(configuration.server(), vote.voteId,
                    envelope.getSubChannel()));
        }
    }

    private void receiveStatus(JsonEnvelope envelope) {
        if (!isTargetedAtThisBackend(envelope)) return;
        String raw = envelope.getFields().getOrDefault(VotingPluginWire.K_REQUEST_ID, "");
        if (raw.isBlank()) {
            send(VotingPluginWire.statusOkay(configuration.server()));
            return;
        }
        try {
            send(VotingPluginWire.statusOkay(configuration.server(), UUID.fromString(raw)));
        } catch (IllegalArgumentException invalid) {
            // A malformed probe receives no capability response.
        }
    }

    private void receiveReceiptRelease(JsonEnvelope envelope) {
        if (!isTargetedAtThisBackend(envelope)
                || !VotingPluginWire.requestsVoteDeliveryAcknowledgement(envelope)) return;
        String voteSubchannel = envelope.getFields().getOrDefault(
                VotingPluginWire.K_VOTE_DELIVERY_SUBCHANNEL, "");
        if (!VotingPluginWire.SUB_VOTE.equals(voteSubchannel)
                && !VotingPluginWire.SUB_VOTE_ONLINE.equals(voteSubchannel)) return;
        UUID voteId;
        UUID playerId;
        try {
            voteId = UUID.fromString(envelope.getFields().getOrDefault(VotingPluginWire.K_VOTE_ID, ""));
            playerId = UUID.fromString(envelope.getFields().getOrDefault(VotingPluginWire.K_UUID, ""));
        } catch (IllegalArgumentException invalid) {
            return;
        }
        NeoForgeDeferredVoteStore.ReleaseResult result = processor.deferredVotes().release(playerId, voteId);
        if (result != NeoForgeDeferredVoteStore.ReleaseResult.RELEASED
                && result != NeoForgeDeferredVoteStore.ReleaseResult.ALREADY_RELEASED) return;
        send(VotingPluginWire.voteDeliveryReceiptReleaseAcknowledgement(
                configuration.server(), voteId, voteSubchannel));
    }

    private void receiveResync(JsonEnvelope envelope) {
        VotingPluginWire.PresenceResyncRequest request = VotingPluginWire.readPresenceResyncRequest(envelope);
        if (request.requestId == null || request.requestedAt <= 0L
                || !configuration.server().equalsIgnoreCase(request.server)) return;
        announceStarted();
    }

    private void receiveSnapshotRequest(JsonEnvelope envelope) {
        VotingPluginWire.PresenceSnapshotRequest request = VotingPluginWire.readPresenceSnapshotRequest(envelope);
        if (request.requestId == null || !configuration.server().equalsIgnoreCase(request.server)
                || !incarnationId.equals(request.backendIncarnationId)
                || request.backendStartedAt != startedAt || request.presenceTimestamp <= 0L) return;
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
                send(VotingPluginWire.presenceSnapshot(configuration.server(), request.requestId, index, chunks,
                        snapshot.subList(from, to), incarnationId, startedAt, timestamp));
            }
        }
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
            send(VotingPluginWire.login(session.getPlayerName(), session.getUuid(), configuration.server(),
                    session.getConnectionId(), incarnationId, startedAt, nextTimestamp()));
        }
    }

    private void sendHeartbeat() {
        synchronized (presenceLock) {
            if (!open.get()) return;
            announceStarted();
            send(VotingPluginWire.backendHeartbeat(configuration.server(), incarnationId,
                    startedAt, nextTimestamp()));
        }
    }

    private void announceStarted() {
        synchronized (presenceLock) {
            send(VotingPluginWire.backendStarted(configuration.server(), incarnationId, startedAt, startedAt));
        }
    }

    private boolean isTargetedAtThisBackend(JsonEnvelope envelope) {
        return configuration.server().equalsIgnoreCase(
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
        envelope = supportedCapabilities(envelope);
        if (sender != null) {
            JsonEnvelope queued = envelope;
            boolean presence = isPresenceEnvelope(queued);
            try {
                sender.execute(() -> {
                    try {
                        sendNow(queued);
                    } catch (RuntimeException failure) {
                        if (presence) presenceDirty.set(true);
                        LOGGER.warning("Socket send failed: " + failure.getClass().getSimpleName());
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
                sendNow(supportedCapabilities(VotingPluginWire.backendStarted(configuration.server(), incarnationId,
                        startedAt, nextTimestamp())));
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

    private static JsonEnvelope supportedCapabilities(JsonEnvelope envelope) {
        if (!switch (envelope.getSubChannel()) {
            case VotingPluginWire.SUB_STATUS_OKAY, VotingPluginWire.SUB_BACKEND_STARTED,
                    VotingPluginWire.SUB_BACKEND_HEARTBEAT -> true;
            default -> false;
        }) return envelope;
        JsonEnvelope authenticated = VotingPluginWire.authenticatedSocketVoteDeliveryCapability(envelope);
        JsonEnvelope.Builder builder = JsonEnvelope.builder(authenticated.getSubChannel())
                .schema(authenticated.getSchema());
        authenticated.getFields().forEach((key, value) -> {
            if (!VotingPluginWire.K_VOTE_DELAY_REJECTION_ACK_VERSION.equals(key)) builder.put(key, value);
        });
        return builder.build();
    }

    private void sendNow(JsonEnvelope envelope) {
        synchronized (sendLock) {
            outbound.accept(authenticator.sign(encryption.encrypt(envelope), Domain.SOCKET_PROXY_BACKEND,
                    configuration.server(), configuration.proxyName()));
        }
    }

    @Override public void close() {
        if (!open.compareAndSet(true, false)) return;
        if (heartbeat != null) heartbeat.shutdownNow();
        JsonEnvelope stopped;
        synchronized (presenceLock) {
            stopped = VotingPluginWire.backendStopped(configuration.server(), incarnationId,
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

    private static void verifyListenerPortAvailable(String host, int port) {
        try (ServerSocket probe = new ServerSocket()) {
            probe.setReuseAddress(false);
            probe.bind(new InetSocketAddress(host, port));
        } catch (IOException unavailable) {
            throw new IllegalStateException("Proxy listener unavailable at " + host + ":" + port,
                    unavailable);
        }
    }

    static void closeTransport(SocketHandler listener, ClientHandler client) {
        SocketServer server = listener.getServer();
        RuntimeException failure = null;
        try {
            listener.closeConnection();
        } catch (RuntimeException closeFailure) {
            failure = closeFailure;
        }
        if (server != null) {
            try {
                server.join(1_000L);
                if (server.isAlive()) LOGGER.warning("Socket stop timed out; forcing close");
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                LOGGER.warning("Socket stop interrupted; forcing close");
            } finally {
                try {
                    server.close();
                } catch (RuntimeException closeFailure) {
                    failure = appendFailure(failure, closeFailure);
                }
            }
        }
        try {
            client.stopConnection();
        } catch (RuntimeException closeFailure) {
            failure = appendFailure(failure, closeFailure);
        }
        if (failure != null) throw failure;
    }

    private static RuntimeException appendFailure(RuntimeException current, RuntimeException added) {
        if (current == null) return added;
        current.addSuppressed(added);
        return current;
    }

    private static String safeSubchannel(JsonEnvelope envelope) {
        String value = envelope.getSubChannel();
        if (value == null) return "unknown";
        return value.replaceAll("[^A-Za-z0-9._-]", "?").substring(0, Math.min(value.length(), 64));
    }
}
