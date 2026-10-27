package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

/** A bounded in-process RESP peer, not acceptance against an installed Redis broker. */
class NeoForgeRedisWireTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeRedisConnection";

    @Test void actualJedisClientSubscribesPublishesUtf8AndClosesItsBlockedSocket() throws Exception {
        try (var peer = new RespPeer()) {
            Object settings = ReflectiveAccess.construct(
                    "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration$Redis",
                    new String[] {"String", "int", "String", "String", "int", "boolean", "String"},
                    new Object[] {"127.0.0.1", peer.port(), "", "", 0, false, "fixture/"});
            Object connection = ReflectiveAccess.construct(OWNER, new String[] {"Redis", "Runnable"},
                    new Object[] {settings, (Runnable) () -> { }});
            String channel = "fixture/VotingPlugin_backend-test";
            var received = new CompletableFuture<byte[]>();
            JsonEnvelope envelope = JsonEnvelope.builder("fixture").put("name", "雪\r\nline").build();
            byte[] expected = JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8);
            try {
                ReflectiveAccess.call(OWNER, connection, "subscribe", new String[] {"String", "BiConsumer"},
                        new Object[] {channel, (java.util.function.BiConsumer<String, byte[]>) (actual, payload) -> {
                            assertEquals(channel, actual);
                            received.complete(payload);
                        }});
                assertEquals(channel, peer.subscription.get(3, TimeUnit.SECONDS));
                ReflectiveAccess.call(OWNER, connection, "publish", new String[] {"String", "JsonEnvelope"},
                        new Object[] {channel, envelope});
                assertArrayEquals(expected, peer.published.get(3, TimeUnit.SECONDS));
                assertArrayEquals(expected, received.get(3, TimeUnit.SECONDS));
                assertEquals(2, peer.connections.size());
                assertFalse(peer.failure.isCompletedExceptionally());
            } finally {
                ReflectiveAccess.call(OWNER, connection, "close", new String[0], new Object[0]);
            }
            assertFalse(((Thread) ReflectiveAccess.field(OWNER, connection, "listener")).isAlive());
        }
    }

    private static final class RespPeer implements AutoCloseable {
        final ServerSocket listener = new ServerSocket(0, 2, InetAddress.getLoopbackAddress());
        final java.util.concurrent.ExecutorService workers = Executors.newFixedThreadPool(3);
        final List<Socket> connections = new CopyOnWriteArrayList<>();
        final CompletableFuture<String> subscription = new CompletableFuture<>();
        final CompletableFuture<byte[]> published = new CompletableFuture<>();
        final CompletableFuture<Void> failure = new CompletableFuture<>();
        volatile OutputStream subscribed;

        RespPeer() throws IOException {
            workers.submit(() -> {
                try {
                    // Production owns exactly one subscriber and one publisher in this fixture.
                    for (int count = 0; count < 2; count++) {
                        Socket socket = listener.accept();
                        socket.setSoTimeout(5000);
                        connections.add(socket);
                        workers.submit(() -> serve(socket));
                    }
                } catch (IOException problem) {
                    if (!listener.isClosed()) failPeer(problem);
                }
            });
        }

        int port() { return listener.getLocalPort(); }

        void serve(Socket socket) {
            try (socket) {
                InputStream input = new BufferedInputStream(socket.getInputStream());
                OutputStream output = socket.getOutputStream();
                for (;;) {
                    List<byte[]> args = command(input);
                    if (args == null) return;
                    String command = new String(args.getFirst(), StandardCharsets.UTF_8);
                    switch (command.toUpperCase(java.util.Locale.ROOT)) {
                        case "CLIENT", "AUTH", "SELECT" -> write(output, "+OK\r\n");
                        case "SUBSCRIBE" -> {
                            subscribed = output;
                            array(output, "subscribe".getBytes(StandardCharsets.UTF_8), args.get(1), 1);
                            subscription.complete(new String(args.get(1), StandardCharsets.UTF_8));
                        }
                        case "PUBLISH" -> {
                            published.complete(args.get(2));
                            array(subscribed, "message".getBytes(StandardCharsets.UTF_8), args.get(1), args.get(2));
                            write(output, ":1\r\n");
                        }
                        default -> throw new IOException("Unexpected synthetic RESP command");
                    }
                }
            } catch (IOException problem) {
                if (!listener.isClosed()) failPeer(problem);
            } catch (RuntimeException problem) {
                failPeer(problem);
            }
        }

        void failPeer(Exception failure) {
            this.failure.completeExceptionally(failure);
            subscription.completeExceptionally(failure);
            published.completeExceptionally(failure);
        }

        static List<byte[]> command(InputStream input) throws IOException {
            int marker = input.read();
            if (marker == -1) return null;
            if (marker != '*') throw new IOException("Expected RESP array");
            int count = Integer.parseInt(line(input));
            if (count < 1 || count > 16) throw new IOException("Synthetic array bound exceeded");
            var result = new ArrayList<byte[]>(count);
            for (int index = 0; index < count; index++) {
                if (input.read() != '$') throw new IOException("Expected RESP bulk string");
                int length = Integer.parseInt(line(input));
                if (length < 0 || length > 4096) throw new IOException("Synthetic bulk bound exceeded");
                byte[] value = input.readNBytes(length);
                if (value.length != length || input.read() != '\r' || input.read() != '\n') throw new EOFException();
                result.add(value);
            }
            return result;
        }

        static String line(InputStream input) throws IOException {
            StringBuilder value = new StringBuilder();
            for (int index = 0; index < 32; index++) {
                int next = input.read();
                if (next == '\r') {
                    if (input.read() != '\n') throw new EOFException();
                    return value.toString();
                }
                if (next < 0) throw new EOFException();
                value.append((char) next);
            }
            throw new IOException("Synthetic RESP header bound exceeded");
        }

        static void array(OutputStream output, Object... values) throws IOException {
            synchronized (output) {
                write(output, "*" + values.length + "\r\n");
                for (Object value : values) {
                    if (value instanceof Integer number) write(output, ":" + number + "\r\n");
                    else {
                        byte[] bytes = (byte[]) value;
                        write(output, "$" + bytes.length + "\r\n");
                        output.write(bytes);
                        write(output, "\r\n");
                    }
                }
                output.flush();
            }
        }

        static void write(OutputStream output, String value) throws IOException {
            output.write(value.getBytes(StandardCharsets.UTF_8));
            output.flush();
        }

        @Override public void close() throws Exception {
            listener.close();
            for (Socket socket : connections) socket.close();
            workers.shutdownNow();
            assertTrue(workers.awaitTermination(3, TimeUnit.SECONDS));
        }
    }
}
