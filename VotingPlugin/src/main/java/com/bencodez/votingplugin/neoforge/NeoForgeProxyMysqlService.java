package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** SQL credentials establish the shared database trust domain, not per-node cryptographic identity. */
final class NeoForgeProxyMysqlService implements NeoForgeProxyTransport {
    interface Receiver { boolean receive(String source, String destination, byte[] payload); }
    interface Queue extends AutoCloseable {
        void poll(Receiver receiver) throws Exception;
        void publish(JsonEnvelope envelope) throws Exception;
        @Override void close() throws Exception;
    }
    interface QueueFactory { Queue open() throws Exception; }

    private static final Logger LOGGER = Logger.getLogger(NeoForgeProxyMysqlService.class.getName());
    private final String server;
    private final TransportEnvelopeEncryption encryption;
    private final QueueFactory factory;
    private final NeoForgeProxyMessageService messages;
    private final AtomicReference<Queue> queue = new AtomicReference<>();
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final AtomicBoolean invalidWarning = new AtomicBoolean();
    private final AtomicBoolean unavailableWarning = new AtomicBoolean();
    private final Object lifecycle = new Object();
    private final ScheduledThreadPoolExecutor worker;
    private long retrySeconds = 1;
    private long pollRetryMillis = 250;

    static NeoForgeProxyMysqlService start(Path directory, NeoForgeProxyTransportConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players) throws IOException {
        var encryption = TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, configuration.communicationEncryption());
        return new NeoForgeProxyMysqlService(configuration, processor, players, encryption,
                () -> {
                    var source = NeoForgeMysqlDataSource.open(configuration.mysql());
                    try { return new NeoForgeMysqlQueue(source, source, configuration.server()); }
                    catch (Exception failure) { source.close(); throw failure; }
                }, true);
    }

    NeoForgeProxyMysqlService(NeoForgeProxyTransportConfiguration configuration, NeoForgeVoteProcessor processor,
            NeoForgePlayerDirectory players, TransportEnvelopeEncryption encryption, QueueFactory factory,
            boolean scheduleHeartbeat) {
        if (configuration.method() != NeoForgeProxyTransportConfiguration.Method.MYSQL) {
            throw new IllegalArgumentException("MySQL transport settings required");
        }
        server = configuration.server();
        this.encryption = Objects.requireNonNull(encryption, "encryption");
        this.factory = Objects.requireNonNull(factory, "factory");
        worker = new ScheduledThreadPoolExecutor(1, task -> {
            var thread = new Thread(task, "VotingPlugin-NeoForge-MySQL");
            thread.setDaemon(true);
            return thread;
        });
        worker.setRemoveOnCancelPolicy(true);
        worker.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        messages = new NeoForgeProxyMessageService(server, processor, players, this::send, () -> { }, scheduleHeartbeat);
        worker.execute(this::connect);
    }

    private void connect() {
        if (!open.get()) return;
        Queue candidate = null;
        boolean adopted = false;
        try {
            candidate = factory.open();
            synchronized (lifecycle) {
                if (!open.get()) return;
                queue.set(candidate);
                adopted = true;
            }
            messages.transportReady();
            schedulePoll(0);
        } catch (Exception failure) {
            if (unavailableWarning.compareAndSet(false, true)) LOGGER.warning("MySQL transport unavailable; retrying");
            synchronized (lifecycle) {
                if (open.get() && !adopted) {
                    worker.schedule(this::connect, retrySeconds, TimeUnit.SECONDS);
                    retrySeconds = Math.min(60, retrySeconds * 2);
                } else if (open.get()) schedulePoll(250);
            }
        } finally {
            if (!adopted && candidate != null) closeQueue(candidate);
        }
    }

    private void schedulePoll(long delayMillis) {
        synchronized (lifecycle) {
            if (open.get()) worker.schedule(this::poll, delayMillis, TimeUnit.MILLISECONDS);
        }
    }

    private void poll() {
        if (!open.get()) return;
        try {
            Queue current = queue.get();
            if (current != null) current.poll(this::receive);
            pollRetryMillis = 250;
        } catch (Exception failure) {
            if (unavailableWarning.compareAndSet(false, true)) LOGGER.warning("MySQL polling failed; rows remain retryable");
            pollRetryMillis = Math.min(60_000, pollRetryMillis * 2);
        } finally { schedulePoll(pollRetryMillis); }
    }

    boolean receive(String source, String destination, byte[] payload) {
        if (!open.get() || !"proxy".equals(source) || !server.equals(destination)) return false;
        try {
            var decrypted = encryption.decrypt(NeoForgeProxyEnvelopeCodec.decode(payload));
            if (!decrypted.accepted()) return false;
            return messages.receive(decrypted.envelope());
        } catch (IOException | RuntimeException failure) {
            if (invalidWarning.compareAndSet(false, true)) LOGGER.warning("Retained invalid MySQL proxy envelope");
            return false;
        }
    }

    private void send(JsonEnvelope envelope) {
        Queue current = queue.get();
        if (current == null) return;
        var builder = JsonEnvelope.builder(envelope.getSubChannel()).schema(envelope.getSchema());
        envelope.getFields().forEach((key, value) -> {
            if (!VotingPluginWire.K_VOTE_DELAY_REJECTION_ACK_VERSION.equals(key)) builder.put(key, value);
        });
        try { current.publish(encryption.encrypt(builder.build())); }
        catch (Exception failure) { throw new IllegalStateException("MySQL proxy publication failed"); }
    }

    @Override public void playerOnline(SharedVoteIdentity identity) { messages.playerOnline(identity); }
    @Override public void playerOffline(SharedVoteIdentity identity) { messages.playerOffline(identity); }

    @Override public void close() {
        synchronized (lifecycle) {
            if (!open.compareAndSet(true, false)) return;
        }
        messages.close();
        synchronized (lifecycle) {
            Queue retired = queue.getAndSet(null);
            worker.execute(() -> { if (retired != null) closeQueue(retired); });
            worker.shutdown();
        }
    }

    private static void closeQueue(Queue queue) {
        try { queue.close(); }
        catch (Exception failure) { LOGGER.warning("MySQL transport cleanup failed"); }
    }
}
