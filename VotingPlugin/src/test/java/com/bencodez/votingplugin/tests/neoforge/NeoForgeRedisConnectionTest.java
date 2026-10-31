package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import redis.clients.jedis.BinaryJedisPubSub;
import redis.clients.jedis.DefaultJedisClientConfig;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;

class NeoForgeRedisConnectionTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeRedisConnection";
    private static final String CHANNEL = "fixture/VotingPlugin_backend-test";
    private static final byte[] CHANNEL_BYTES = CHANNEL.getBytes(StandardCharsets.UTF_8);

    @Test void binarySubscriberFiltersChannelsAndPublisherUsesASeparatePooledConnection() throws Exception {
        JedisPool pool = mock(JedisPool.class);
        Jedis subscriber = mock(Jedis.class);
        Jedis publisher = mock(Jedis.class);
        when(pool.getResource()).thenReturn(publisher);
        var callback = new AtomicReference<BinaryJedisPubSub>();
        var release = new CountDownLatch(1);
        prepareSubscriber(subscriber, callback, release);
        var reconnects = new AtomicInteger();
        var refreshed = new CountDownLatch(1);
        Object connection = create(pool, () -> subscriber, () -> {
            reconnects.incrementAndGet();
            refreshed.countDown();
        });
        List<byte[]> received = new CopyOnWriteArrayList<>();
        try {
            subscribe(connection, (channel, payload) -> {
                assertEquals(CHANNEL, channel);
                received.add(payload);
            });
            assertTrue(refreshed.await(3, TimeUnit.SECONDS));
            assertEquals(1, reconnects.get());
            byte[] completeWrapper = "{\"t\":\"fixture\",\"v\":1,\"f\":{\"name\":\"雪\"}}"
                    .getBytes(StandardCharsets.UTF_8);
            callback.get().onMessage("other/channel".getBytes(StandardCharsets.UTF_8), completeWrapper);
            callback.get().onMessage(CHANNEL_BYTES, completeWrapper);
            assertEquals(1, received.size());
            assertArrayEquals(completeWrapper, received.getFirst());
            verify(pool, never()).getResource();
            JsonEnvelope envelope = JsonEnvelope.builder("fixture").put("name", "雪").build();
            call(connection, "publish", new String[] {"String", "JsonEnvelope"}, "fixture/VotingPlugin", envelope);
            verify(publisher).publish(eq("fixture/VotingPlugin".getBytes(StandardCharsets.UTF_8)),
                    eq(JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8)));
            verify(publisher).close();
            verify(subscriber, never()).publish(any(byte[].class), any(byte[].class));
        } finally {
            close(connection);
        }
        close(connection);
        callback.get().onMessage(CHANNEL_BYTES, new byte[] {1});
        assertEquals(1, received.size());
        verify(subscriber, times(1)).close();
        verify(pool, times(1)).close();
    }

    @Test void subscribedDisconnectClosesItsClientAndResubscribesBeforeRefreshingPresence() throws Exception {
        JedisPool pool = mock(JedisPool.class);
        Jedis first = mock(Jedis.class);
        Jedis second = mock(Jedis.class);
        doAnswer(invocation -> {
            BinaryJedisPubSub callback = invocation.getArgument(0);
            callback.onSubscribe(CHANNEL_BYTES, 1);
            throw new IllegalStateException("synthetic disconnect");
        }).when(first).subscribe(any(BinaryJedisPubSub.class), any(byte[][].class));
        var secondCallback = new AtomicReference<BinaryJedisPubSub>();
        var release = new CountDownLatch(1);
        prepareSubscriber(second, secondCallback, release);
        var opened = new AtomicInteger();
        var refreshed = new CountDownLatch(2);
        Object connection = create(pool, () -> opened.getAndIncrement() == 0 ? first : second, refreshed::countDown);
        try {
            subscribe(connection, (channel, payload) -> { });
            assertTrue(refreshed.await(4, TimeUnit.SECONDS));
            assertEquals(2, opened.get());
            assertNotNull(secondCallback.get());
            verify(first, times(1)).close();
        } finally {
            close(connection);
        }
        verify(second, times(1)).close();
        verify(pool, times(1)).close();
    }

    @Test void shutdownOwnsLateSubscriberWithoutSubscribingOrClosingItTwice() throws Exception {
        JedisPool pool = mock(JedisPool.class);
        Jedis late = mock(Jedis.class);
        var entered = new CountDownLatch(1);
        var release = new CountDownLatch(1);
        var closed = new CountDownLatch(1);
        doAnswer(invocation -> { closed.countDown(); return null; }).when(late).close();
        Object connection = create(pool, () -> {
            entered.countDown();
            boolean interrupted = false;
            for (;;) {
                try { release.await(); break; }
                catch (InterruptedException expected) { interrupted = true; }
            }
            if (interrupted) Thread.currentThread().interrupt();
            return late;
        }, () -> fail("A retired subscriber must not refresh presence"));
        try (var caller = Executors.newSingleThreadExecutor()) {
            var startup = caller.submit(() -> subscribe(connection, (channel, payload) -> { }));
            assertTrue(entered.await(3, TimeUnit.SECONDS));
            try {
                close(connection);
                var failed = assertThrows(java.util.concurrent.ExecutionException.class,
                        () -> startup.get(3, TimeUnit.SECONDS));
                assertInstanceOf(IllegalStateException.class, failed.getCause());
            } finally {
                release.countDown();
            }
            assertTrue(closed.await(3, TimeUnit.SECONDS));
            Thread listener = (Thread) ReflectiveAccess.field(OWNER, connection, "listener");
            listener.join(3000);
            assertFalse(listener.isAlive());
        } finally {
            release.countDown();
            close(connection);
        }
        verify(late, never()).subscribe(any(BinaryJedisPubSub.class), any(byte[][].class));
        verify(late, times(1)).close();
        verify(pool, times(1)).close();
    }

    @Test void poolCloseFailureStillInterruptsAndJoinsTheOwnedListener() throws Exception {
        JedisPool pool = mock(JedisPool.class);
        doThrow(new IllegalStateException("synthetic pool close failure")).when(pool).close();
        Jedis subscriber = mock(Jedis.class);
        var interrupted = new CountDownLatch(1);
        doAnswer(invocation -> {
            BinaryJedisPubSub callback = invocation.getArgument(0);
            callback.onSubscribe(CHANNEL_BYTES, 1);
            try { new CountDownLatch(1).await(); }
            catch (InterruptedException shutdown) { interrupted.countDown(); }
            return null;
        }).when(subscriber).subscribe(any(BinaryJedisPubSub.class), any(byte[][].class));
        Object connection = create(pool, () -> subscriber, () -> { });
        subscribe(connection, (channel, payload) -> { });
        assertThrows(IllegalStateException.class, () -> close(connection));
        assertEquals(0, interrupted.getCount());
        assertFalse(((Thread) ReflectiveAccess.field(OWNER, connection, "listener")).isAlive());
        close(connection);
        verify(subscriber, times(1)).close();
        verify(pool, times(1)).close();
    }

    @Test void tlsCredentialsDatabaseAndTimeoutsReachTheActualJedisClientConfiguration() {
        Object settings = ReflectiveAccess.construct(
                "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration$Redis",
                new String[] {"String", "int", "String", "String", "int", "boolean", "String"},
                new Object[] {"redis.example", 6380, "fixture-user", " synthetic-only ", 3, true, "fixture/"});
        var config = (DefaultJedisClientConfig) ReflectiveAccess.call(OWNER, null, "clientConfig",
                new String[] {"Redis"}, new Object[] {settings});
        assertTrue(config.isSsl());
        assertEquals("HTTPS", config.getSslParameters().getEndpointIdentificationAlgorithm());
        assertEquals("fixture-user", config.getUser());
        assertEquals(" synthetic-only ", config.getPassword());
        assertEquals(3, config.getDatabase());
        assertEquals(2000, config.getConnectionTimeoutMillis());
        assertEquals(2000, config.getSocketTimeoutMillis());
        assertEquals(0, config.getBlockingSocketTimeoutMillis());
    }

    private static void prepareSubscriber(Jedis subscriber, AtomicReference<BinaryJedisPubSub> reference,
            CountDownLatch release) {
        doAnswer(invocation -> {
            BinaryJedisPubSub callback = invocation.getArgument(0);
            byte[][] channels = (byte[][]) invocation.getRawArguments()[1];
            assertEquals(1, channels.length);
            assertTrue(Arrays.equals(CHANNEL_BYTES, channels[0]));
            reference.set(callback);
            callback.onSubscribe(CHANNEL_BYTES, 1);
            release.await(5, TimeUnit.SECONDS);
            return null;
        }).when(subscriber).subscribe(any(BinaryJedisPubSub.class), any(byte[][].class));
        doAnswer(invocation -> { release.countDown(); return null; }).when(subscriber).close();
    }

    private static Object create(JedisPool pool, Supplier<Jedis> subscribers, Runnable reconnected) {
        return ReflectiveAccess.construct(OWNER, new String[] {"JedisPool", "Supplier", "Runnable"},
                new Object[] {pool, subscribers, reconnected});
    }
    private static void subscribe(Object connection, BiConsumer<String, byte[]> receiver) {
        call(connection, "subscribe", new String[] {"String", "BiConsumer"}, CHANNEL, receiver);
    }
    private static void close(Object connection) { call(connection, "close", new String[0]); }
    private static Object call(Object receiver, String method, String[] signature, Object... values) {
        return ReflectiveAccess.call(OWNER, receiver, method, signature, values);
    }
}
