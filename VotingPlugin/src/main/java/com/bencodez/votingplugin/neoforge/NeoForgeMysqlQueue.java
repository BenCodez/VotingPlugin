package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

import javax.sql.DataSource;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;

/** Uses the existing SimpleAPI queue schema, retaining a row until application acceptance. */
final class NeoForgeMysqlQueue implements NeoForgeProxyMysqlService.Queue {
    static final int PAGE_SIZE = 50;
    private static final int QUERY_SECONDS = 2;
    private static final Logger LOGGER = Logger.getLogger(NeoForgeMysqlQueue.class.getName());
    private static final String TABLE = "VotingPlugin_message_queue";
    private final DataSource source;
    private final AutoCloseable closeSource;
    private final String server;
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final AtomicBoolean invalidWarning = new AtomicBoolean();
    private long cursor;

    NeoForgeMysqlQueue(DataSource source, AutoCloseable closeSource, String server) throws SQLException {
        this.source = Objects.requireNonNull(source, "source");
        this.closeSource = Objects.requireNonNull(closeSource, "closeSource");
        this.server = Objects.requireNonNull(server, "server");
        try (Connection connection = source.getConnection(); var statement = connection.createStatement()) {
            statement.setQueryTimeout(QUERY_SECONDS);
            statement.execute("CREATE TABLE IF NOT EXISTS " + TABLE
                    + " (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT, source VARCHAR(36) NOT NULL,"
                    + " destination VARCHAR(36) NOT NULL, created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,"
                    + " payload LONGTEXT NOT NULL, PRIMARY KEY (id), INDEX idx_dest_id (destination, id)) ENGINE=InnoDB");
        }
    }

    @Override public void poll(NeoForgeProxyMysqlService.Receiver receiver) throws SQLException {
        if (!open.get()) return;
        List<Row> rows = readPage();
        // No pool connection remains checked out during storage work or acknowledgement publication.
        for (Row row : rows) {
            if (!open.get()) return;
            if (row.payload == null) {
                if (invalidWarning.compareAndSet(false, true)) LOGGER.warning("Retained invalid MySQL queue payload");
                continue;
            }
            if (receiver.receive(row.source, row.destination, row.payload) && open.get()) remove(row);
        }
    }

    private List<Row> readPage() throws SQLException {
        ArrayList<Row> rows = new ArrayList<>(PAGE_SIZE);
        try (Connection connection = source.getConnection()) {
            String lock = "backend-channel-" + server;
            if (!acquire(connection, lock)) return rows;
            try {
                // Bound the actual server response too: a large LONGTEXT never crosses the JDBC boundary.
                try (PreparedStatement select = connection.prepareStatement("SELECT id, source, destination,"
                        + " CASE WHEN OCTET_LENGTH(payload) <= " + NeoForgeProxyEnvelopeCodec.MAX_BYTES
                        + " THEN payload ELSE NULL END AS payload FROM " + TABLE
                        + " WHERE destination = ? AND id > ? ORDER BY id LIMIT " + PAGE_SIZE)) {
                    select.setQueryTimeout(QUERY_SECONDS);
                    select.setMaxRows(PAGE_SIZE);
                    select.setString(1, server);
                    select.setLong(2, cursor);
                    try (var result = select.executeQuery()) {
                        while (rows.size() < PAGE_SIZE && result.next()) {
                            long id = result.getLong("id");
                            if (id <= cursor) throw new SQLException("Invalid MySQL queue ordering");
                            byte[] payload;
                            try (Reader reader = result.getCharacterStream("payload")) {
                                payload = boundedPayload(reader);
                            } catch (IOException failure) {
                                throw new SQLException("MySQL queue read failed");
                            }
                            rows.add(new Row(id, result.getString("source"), result.getString("destination"), payload));
                            cursor = id;
                        }
                    }
                }
                // Revisit refused rows, while full pages can advance to other players rather than starve them.
                if (rows.size() < PAGE_SIZE) cursor = 0;
            } finally {
                try (PreparedStatement release = connection.prepareStatement("SELECT RELEASE_LOCK(?)")) {
                    release.setQueryTimeout(QUERY_SECONDS);
                    release.setString(1, lock);
                    try (var result = release.executeQuery()) {
                        if (!result.next() || result.getInt(1) != 1) throw new SQLException("MySQL queue lock not released");
                    }
                } catch (SQLException failure) {
                    // Returning a connection with a named lock to the pool can strand peer consumers.
                    try { connection.abort(Runnable::run); }
                    catch (SQLException cleanup) { failure.addSuppressed(cleanup); }
                    throw failure;
                }
            }
        }
        return rows;
    }

    private static byte[] boundedPayload(Reader reader) throws IOException {
        if (reader == null) return null;
        StringBuilder text = new StringBuilder();
        char[] buffer = new char[4096];
        int count;
        while ((count = reader.read(buffer)) != -1) {
            if (text.length() + count > NeoForgeProxyEnvelopeCodec.MAX_BYTES) return null;
            text.append(buffer, 0, count);
        }
        try {
            var encoded = StandardCharsets.UTF_8.newEncoder().onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT).encode(CharBuffer.wrap(text));
            if (encoded.remaining() > NeoForgeProxyEnvelopeCodec.MAX_BYTES) return null;
            byte[] bytes = new byte[encoded.remaining()];
            encoded.get(bytes);
            return bytes;
        } catch (CharacterCodingException invalid) { return null; }
    }

    private static boolean acquire(Connection connection, String lock) throws SQLException {
        try (PreparedStatement statement = connection.prepareStatement("SELECT GET_LOCK(?, 0)")) {
            statement.setQueryTimeout(QUERY_SECONDS);
            statement.setString(1, lock);
            try (var result = statement.executeQuery()) { return result.next() && result.getInt(1) == 1; }
        }
    }

    private void remove(Row row) throws SQLException {
        // Another consumer may have already removed it; a missing row is an idempotent outcome.
        try (Connection connection = source.getConnection(); PreparedStatement remove = connection.prepareStatement(
                "DELETE FROM " + TABLE + " WHERE id = ? AND source = 'proxy' AND destination = ?")) {
            remove.setQueryTimeout(QUERY_SECONDS);
            remove.setLong(1, row.id);
            remove.setString(2, server);
            remove.executeUpdate();
        }
    }

    @Override public void publish(JsonEnvelope envelope) throws SQLException {
        if (!open.get()) throw new SQLException("MySQL transport is closed");
        String payload = JsonEnvelopeCodec.encode(envelope);
        if (payload.getBytes(StandardCharsets.UTF_8).length > NeoForgeProxyEnvelopeCodec.MAX_BYTES) {
            throw new SQLException("MySQL envelope exceeds transport limit");
        }
        try (Connection connection = source.getConnection(); PreparedStatement insert = connection.prepareStatement(
                "INSERT INTO " + TABLE + " (source, destination, payload) VALUES (?, 'proxy', ?)")) {
            insert.setQueryTimeout(QUERY_SECONDS);
            insert.setString(1, server);
            insert.setString(2, payload);
            insert.executeUpdate();
        }
    }

    @Override public void close() throws Exception {
        if (open.compareAndSet(true, false)) closeSource.close();
    }

    private record Row(long id, String source, String destination, byte[] payload) { }
}
