package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.logging.Logger;


import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Shared broker authentication, durable dispatch and owned connection lifecycle. */
final class NeoForgeProxyBrokerService implements NeoForgeProxyTransport {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeProxyBrokerService.class.getName());

    interface Connection extends AutoCloseable {
        void subscribe(String topic, BiConsumer<String, byte[]> receiver) throws Exception;
        void publish(String topic, JsonEnvelope envelope) throws Exception;
        @Override void close() throws Exception;
    }

    interface ConnectionFactory { Connection open() throws Exception; }

    private final NeoForgeProxyTransportConfiguration configuration;
    private final SharedTransportEnvelopeAuthenticator authenticator;
    private final TransportEnvelopeEncryption encryption;
    private final ConnectionFactory factory;
    private final Domain domain;
    private final String inboundTopic;
    private final String outboundTopic;
    private final NeoForgeProxyMessageService messages;
    private final AtomicReference<Connection> connection = new AtomicReference<>();
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final AtomicBoolean invalidWarning = new AtomicBoolean();
    private final Object lifecycle = new Object();
    private final ScheduledThreadPoolExecutor worker;
    private long retrySeconds = 1;

    NeoForgeProxyBrokerService(NeoForgeProxyTransportConfiguration configuration, NeoForgeVoteProcessor processor,
            NeoForgePlayerDirectory players, SharedTransportEnvelopeAuthenticator authenticator,
            TransportEnvelopeEncryption encryption, ConnectionFactory factory, boolean scheduleHeartbeat) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        if (configuration.method() != NeoForgeProxyTransportConfiguration.Method.MQTT
                && configuration.method() != NeoForgeProxyTransportConfiguration.Method.REDIS) {
            throw new IllegalArgumentException("Broker settings required");
        }
        this.authenticator = Objects.requireNonNull(authenticator, "authenticator");
        this.encryption = Objects.requireNonNull(encryption, "encryption");
        this.factory = Objects.requireNonNull(factory, "factory");
        boolean redis = configuration.method() == NeoForgeProxyTransportConfiguration.Method.REDIS;
        domain = redis ? Domain.REDIS_PROXY_BACKEND : Domain.MQTT_PROXY_BACKEND;
        inboundTopic = redis ? com.bencodez.votingplugin.proxy.redis.VotingPluginRedisChannels.backend(
                configuration.redis().prefix(), configuration.server())
                : configuration.mqtt().prefix() + "votingplugin/servers/" + configuration.server();
        outboundTopic = redis ? com.bencodez.votingplugin.proxy.redis.VotingPluginRedisChannels.proxy(
                configuration.redis().prefix()) : configuration.mqtt().prefix() + "votingplugin/servers/proxy";
        if (authenticator.mode() == Mode.COMPATIBILITY) {
            LOGGER.warning("Proxy broker compatibility mode accepts legacy unsigned traffic; use REQUIRED after upgrading peers");
        }
        worker = new ScheduledThreadPoolExecutor(1, task -> {
            Thread thread = new Thread(task, "VotingPlugin-NeoForge-" + configuration.method());
            thread.setDaemon(true);
            return thread;
        });
        worker.setRemoveOnCancelPolicy(true);
        worker.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        messages = new NeoForgeProxyMessageService(configuration.server(), processor, players,
                this::send, () -> { }, scheduleHeartbeat);
        worker.execute(this::connect);
    }

    private void connect() {
        if (!open.get()) return;
        Connection candidate = null;
        boolean adopted = false;
        try {
            candidate = factory.open();
            if (!open.get()) return;
            candidate.subscribe(inboundTopic, this::receive);
            synchronized (lifecycle) {
                if (!open.get()) return;
                connection.set(candidate);
                adopted = true;
            }
            messages.transportReady();
        } catch (Exception failure) {
            if (open.get() && !adopted) {
                LOGGER.warning("Proxy broker connection unavailable; retrying without acknowledging unretained votes");
                synchronized (lifecycle) {
                    if (open.get()) {
                        worker.schedule(this::connect, retrySeconds, TimeUnit.SECONDS);
                        retrySeconds = Math.min(60, retrySeconds * 2);
                    }
                }
            }
        } finally {
            if (!adopted && candidate != null) closeConnection(candidate);
        }
    }

    void receive(String topic, byte[] payload) {
        if (!open.get() || !inboundTopic.equals(topic)) return;
        try {
            JsonEnvelope envelope = NeoForgeProxyEnvelopeCodec.decode(payload);
            String sender = envelope.getFields().get(SharedTransportEnvelopeAuthenticator.K_SENDER);
            var verified = authenticator.verify(envelope, domain, inboundTopic);
            if (!verified.accepted() || (!verified.unsignedCompatibility()
                    && !configuration.proxyName().equals(sender))) {
                warnInvalid();
                return;
            }
            var decrypted = encryption.decrypt(verified.envelope());
            if (!decrypted.accepted()) {
                warnInvalid();
                return;
            }
            messages.receive(decrypted.envelope());
        } catch (IOException | RuntimeException invalid) {
            warnInvalid();
        }
    }

    private void warnInvalid() {
        if (invalidWarning.compareAndSet(false, true)) LOGGER.warning("Rejected invalid broker proxy message");
    }

    private void send(JsonEnvelope envelope) {
        Connection current = connection.get();
        // Initial announcements precede connection admission; transportReady republishes them.
        if (current == null) return;
        JsonEnvelope.Builder builder = JsonEnvelope.builder(envelope.getSubChannel()).schema(envelope.getSchema());
        envelope.getFields().forEach((key, value) -> {
            // NeoForge cannot complete delayed-vote rejection; never negotiate that capability.
            if (!VotingPluginWire.K_VOTE_DELAY_REJECTION_ACK_VERSION.equals(key)) builder.put(key, value);
        });
        try {
            current.publish(outboundTopic, authenticator.sign(prepareOutbound(builder.build()),
                    domain, configuration.server(), outboundTopic));
        } catch (Exception failure) {
            throw new IllegalStateException("Proxy broker publication failed");
        }
    }

    private JsonEnvelope prepareOutbound(JsonEnvelope envelope) {
        JsonEnvelope encrypted = encryption.encrypt(envelope);
        return domain == Domain.REDIS_PROXY_BACKEND ? VotingPluginWire.withRedisDeliveryId(encrypted) : encrypted;
    }

    void transportReady() { messages.transportReady(); }

    @Override public void playerOnline(SharedVoteIdentity identity) { messages.playerOnline(identity); }
    @Override public void playerOffline(SharedVoteIdentity identity) { messages.playerOffline(identity); }

    @Override public void close() {
        synchronized (lifecycle) {
            if (!open.compareAndSet(true, false)) return;
        }
        messages.close();
        synchronized (lifecycle) {
            Connection retired = connection.getAndSet(null);
            worker.execute(() -> { if (retired != null) closeConnection(retired); });
            worker.shutdown();
        }
    }

    private static void closeConnection(Connection retired) {
        try { retired.close(); }
        catch (Exception failure) { LOGGER.warning("Proxy broker transport cleanup failed"); }
    }
}
