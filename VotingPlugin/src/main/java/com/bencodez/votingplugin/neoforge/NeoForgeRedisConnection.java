package com.bencodez.votingplugin.neoforge;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.logging.Logger;

import javax.net.ssl.SSLParameters;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;

import redis.clients.jedis.BinaryJedisPubSub;
import redis.clients.jedis.DefaultJedisClientConfig;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;

/** One subscriber and one bounded publisher connection; retries retain the same application occurrence. */
final class NeoForgeRedisConnection implements NeoForgeProxyRedisService.Connection {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeRedisConnection.class.getName());
    private final JedisPool publishers;
    private final Supplier<Jedis> subscribers;
    private final Runnable reconnected;
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final Object lifecycle = new Object();
    private final CountDownLatch ready = new CountDownLatch(1);
    private final CountDownLatch stopped = new CountDownLatch(1);
    private Jedis subscriber;
    private Thread listener;

    NeoForgeRedisConnection(NeoForgeProxyTransportConfiguration.Redis settings, Runnable reconnected) {
        this(pool(settings), () -> new Jedis(new HostAndPort(settings.host(), settings.port()),
                clientConfig(settings)), reconnected);
    }

    NeoForgeRedisConnection(JedisPool publishers, Supplier<Jedis> subscribers, Runnable reconnected) {
        this.publishers = Objects.requireNonNull(publishers, "publishers");
        this.subscribers = Objects.requireNonNull(subscribers, "subscribers");
        this.reconnected = Objects.requireNonNull(reconnected, "reconnected");
    }

    private static JedisPool pool(NeoForgeProxyTransportConfiguration.Redis settings) {
        var limits = new GenericObjectPoolConfig<Jedis>();
        limits.setMaxTotal(1);
        limits.setMaxIdle(1);
        limits.setMinIdle(0);
        limits.setMaxWait(Duration.ofSeconds(2));
        return new JedisPool(limits, new HostAndPort(settings.host(), settings.port()), clientConfig(settings));
    }

    static DefaultJedisClientConfig clientConfig(NeoForgeProxyTransportConfiguration.Redis settings) {
        var config = DefaultJedisClientConfig.builder().database(settings.database()).ssl(settings.ssl())
                .connectionTimeoutMillis(2000).socketTimeoutMillis(2000).blockingSocketTimeoutMillis(0);
        if (!settings.username().isEmpty()) config.user(settings.username());
        if (!settings.password().isEmpty()) config.password(settings.password());
        if (settings.ssl()) {
            var ssl = new SSLParameters();
            ssl.setEndpointIdentificationAlgorithm("HTTPS");
            config.sslParameters(ssl);
        }
        return config.build();
    }

    @Override public void subscribe(String channel, BiConsumer<String, byte[]> receiver) throws Exception {
        Objects.requireNonNull(receiver, "receiver");
        byte[] encoded = channel.getBytes(StandardCharsets.UTF_8);
        synchronized (lifecycle) {
            if (!open.get() || listener != null) throw new IllegalStateException("Redis subscription not available");
            listener = new Thread(() -> listen(channel, encoded, receiver), "VotingPlugin-NeoForge-Redis-Receive");
            listener.setDaemon(true);
            listener.start();
        }
        if (!ready.await(3, TimeUnit.SECONDS) || !open.get()) {
            throw new IllegalStateException("Redis subscription did not become ready");
        }
    }

    private void listen(String channel, byte[] encoded, BiConsumer<String, byte[]> receiver) {
        long retrySeconds = 1;
        while (open.get()) {
            Jedis candidate = null;
            boolean adopted = false;
            AtomicBoolean subscribed = new AtomicBoolean();
            try {
                candidate = subscribers.get();
                synchronized (lifecycle) {
                    if (!open.get()) return;
                    subscriber = candidate;
                    adopted = true;
                }
                candidate.subscribe(new BinaryJedisPubSub() {
                    @Override public void onSubscribe(byte[] actual, int count) {
                        if (!open.get() || !Arrays.equals(encoded, actual)) return;
                        subscribed.set(true);
                        ready.countDown();
                        reconnected.run();
                    }
                    @Override public void onMessage(byte[] actual, byte[] payload) {
                        if (open.get() && Arrays.equals(encoded, actual)) receiver.accept(channel, payload);
                    }
                }, encoded);
            } catch (RuntimeException unavailable) {
                if (open.get()) LOGGER.warning("Redis subscription unavailable; retrying");
            } finally {
                boolean release;
                synchronized (lifecycle) {
                    release = !adopted || subscriber == candidate;
                    if (subscriber == candidate) subscriber = null;
                }
                // Shutdown may already own an adopted subscriber; late unadopted clients remain ours.
                if (release && candidate != null) closeSubscriber(candidate);
            }
            if (subscribed.get()) retrySeconds = 1;
            try {
                if (stopped.await(retrySeconds, TimeUnit.SECONDS)) return;
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                return;
            }
            retrySeconds = Math.min(60, retrySeconds * 2);
        }
    }

    @Override public void publish(String channel, JsonEnvelope envelope) {
        if (!open.get()) throw new IllegalStateException("Redis transport is closed");
        try (Jedis publisher = publishers.getResource()) {
            publisher.publish(channel.getBytes(StandardCharsets.UTF_8),
                    JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8));
        }
    }

    @Override public void close() {
        Jedis retired;
        Thread thread;
        synchronized (lifecycle) {
            if (!open.compareAndSet(true, false)) return;
            retired = subscriber;
            subscriber = null;
            thread = listener;
        }
        stopped.countDown();
        ready.countDown();
        try {
            if (retired != null) closeSubscriber(retired);
        } finally {
            try {
                publishers.close();
            } finally {
                if (thread != null) {
                    thread.interrupt();
                    try { thread.join(1000); }
                    catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
                    if (thread.isAlive()) LOGGER.warning("Redis subscriber cleanup is still pending");
                }
            }
        }
    }

    private static void closeSubscriber(Jedis client) {
        try { client.close(); }
        catch (RuntimeException failure) { LOGGER.warning("Redis subscriber cleanup failed"); }
    }
}
