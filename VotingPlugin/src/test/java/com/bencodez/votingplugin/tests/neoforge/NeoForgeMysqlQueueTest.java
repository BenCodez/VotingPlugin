package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicInteger;

import javax.sql.DataSource;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

class NeoForgeMysqlQueueTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeMysqlQueue";
    private static final String SERVER = "backend-test";
    private static final String WRAPPER = "{\"t\":\"fixture\",\"v\":1,\"f\":{\"name\":\"雪\\r\\nline\"}}";

    @Test void deletesOnlyConfirmedRowsAfterReleasingTheSqlConnection() throws Exception {
        Jdbc jdbc = new Jdbc();
        jdbc.rows.addAll(List.of(new Row(1, WRAPPER), new Row(2, WRAPPER)));
        Object queue = create(jdbc);
        try {
            poll(queue, new Receiver() {
                int calls;
                public boolean receive(String source, String destination, byte[] payload) {
                    assertEquals(0, jdbc.activeConnections.get(), "Acceptance must not wait while owning the SQL pool slot");
                    assertEquals("proxy", source);
                    assertEquals(SERVER, destination);
                    assertArrayEquals(WRAPPER.getBytes(StandardCharsets.UTF_8), payload);
                    return ++calls == 2;
                }
            });
            assertEquals(List.of(1L), jdbc.rows.stream().map(Row::id).toList());
            assertEquals(List.of(2L), jdbc.deleted);
            verify(jdbc.select).setQueryTimeout(2);
            verify(jdbc.select).setMaxRows(50);
            verify(jdbc.acquire).setString(1, "backend-channel-" + SERVER);
            assertTrue(jdbc.selectSql.contains("OCTET_LENGTH(payload) <= 262144"));
            assertTrue(jdbc.selectSql.endsWith("ORDER BY id LIMIT 50"));
            assertTrue(jdbc.deleteSql.contains("source = 'proxy' AND destination = ?"));
        } finally { close(queue); }
    }

    @Test void deleteFailureLeavesTheOriginalOccurrenceAvailableForRetry() throws Exception {
        Jdbc jdbc = new Jdbc();
        jdbc.rows.add(new Row(7, WRAPPER));
        Object queue = create(jdbc);
        try {
            jdbc.failDelete = true;
            assertThrows(SQLException.class, () -> poll(queue, new Receiver()));
            assertEquals(1, jdbc.rows.size());
            jdbc.failDelete = false;
            poll(queue, new Receiver());
            assertTrue(jdbc.rows.isEmpty());
            assertEquals(List.of(7L), jdbc.deleted);
        } finally { close(queue); }
    }

    @Test void invalidOrUnconfirmedRowsRemainWithoutStarvingTheNextPage() throws Exception {
        Jdbc jdbc = new Jdbc();
        for (int id = 1; id <= 51; id++) jdbc.rows.add(new Row(id, id == 51 ? WRAPPER : null));
        Object queue = create(jdbc);
        try {
            AtomicInteger accepted = new AtomicInteger();
            Receiver receiver = new Receiver() {
                public boolean receive(String source, String destination, byte[] payload) {
                    accepted.incrementAndGet();
                    return true;
                }
            };
            poll(queue, receiver);
            assertEquals(0, accepted.get());
            assertEquals(51, jdbc.rows.size());
            poll(queue, receiver);
            assertEquals(1, accepted.get());
            assertEquals(50, jdbc.rows.size());
            assertEquals(List.of(51L), jdbc.deleted);
        } finally { close(queue); }
    }

    @Test void boundedReaderRejectsByteOverflowAndUnpairedUnicodeWithoutRewritingIt() {
        Object exact = bounded("x".repeat(262144));
        assertEquals(262144, ((byte[]) exact).length);
        assertNull(bounded("x".repeat(262145)));
        assertNull(bounded("雪".repeat(87382)));
        assertNull(bounded("\uD800"));
        assertArrayEquals(WRAPPER.getBytes(StandardCharsets.UTF_8), (byte[]) bounded(WRAPPER));
    }

    @Test void releaseFailureAbortsTheBorrowedConnectionAndDoesNotClaimAnyRows() throws Exception {
        Jdbc jdbc = new Jdbc();
        jdbc.rows.add(new Row(1, WRAPPER));
        when(jdbc.releaseResult.getInt(1)).thenReturn(0);
        Object queue = create(jdbc);
        try {
            assertThrows(SQLException.class, () -> poll(queue, new Receiver() {
                public boolean receive(String source, String destination, byte[] payload) {
                    fail("A failed queue read must not dispatch the batch");
                    return false;
                }
            }));
            verify(jdbc.connection).abort(any(Executor.class));
            assertEquals(1, jdbc.rows.size());
            assertTrue(jdbc.deleted.isEmpty());
        } finally { close(queue); }
    }

    @Test void publisherUsesTheExistingProxyRoleAndParametersAndCloseIsIdempotent() throws Exception {
        Jdbc jdbc = new Jdbc();
        Object queue = create(jdbc);
        JsonEnvelope envelope = JsonEnvelope.builder("fixture").put("name", "雪\r\nline").build();
        ReflectiveAccess.call(OWNER, queue, "publish", new String[] {"JsonEnvelope"}, new Object[] {envelope});
        verify(jdbc.insert).setString(1, SERVER);
        verify(jdbc.insert).setString(2, JsonEnvelopeCodec.encode(envelope));
        verify(jdbc.insert).setQueryTimeout(2);
        assertTrue(jdbc.insertSql.contains("(source, destination, payload) VALUES (?, 'proxy', ?)"));
        close(queue);
        close(queue);
        assertEquals(1, jdbc.closed.get());
        assertThrows(SQLException.class, () -> ReflectiveAccess.call(OWNER, queue, "publish",
                new String[] {"JsonEnvelope"}, new Object[] {envelope}));
        poll(queue, new Receiver() {
            public boolean receive(String source, String destination, byte[] payload) {
                fail("Closed queue must not dispatch");
                return false;
            }
        });
    }

    private static Object bounded(String text) {
        return ReflectiveAccess.call(OWNER, null, "boundedPayload", new String[] {"Reader"},
                new Object[] {new StringReader(text)});
    }
    private static Object create(Jdbc jdbc) {
        return ReflectiveAccess.construct(OWNER, new String[] {"DataSource", "AutoCloseable", "String"},
                new Object[] {jdbc.source, (AutoCloseable) jdbc.closed::incrementAndGet, SERVER});
    }
    private static void poll(Object queue, Receiver receiver) {
        ReflectiveAccess.call(OWNER, queue, "poll", new String[] {"Receiver"}, new Object[] {receiver});
    }
    private static void close(Object queue) {
        ReflectiveAccess.call(OWNER, queue, "close", new String[0], new Object[0]);
    }
    public static class Receiver {
        public boolean receive(String source, String destination, byte[] payload) { return true; }
    }
    private record Row(long id, String payload) { }

    private static final class Jdbc {
        final DataSource source = mock(DataSource.class);
        final Connection connection = mock(Connection.class);
        final Statement schema = mock(Statement.class);
        final PreparedStatement acquire = mock(PreparedStatement.class);
        final PreparedStatement release = mock(PreparedStatement.class);
        final PreparedStatement select = mock(PreparedStatement.class);
        final PreparedStatement delete = mock(PreparedStatement.class);
        final PreparedStatement insert = mock(PreparedStatement.class);
        final ResultSet releaseResult = mock(ResultSet.class);
        final List<Row> rows = new ArrayList<>();
        final List<Long> deleted = new ArrayList<>();
        final AtomicInteger activeConnections = new AtomicInteger();
        final AtomicInteger closed = new AtomicInteger();
        boolean failDelete;
        long cursor;
        long deleteId;
        String selectSql;
        String deleteSql;
        String insertSql;

        Jdbc() throws Exception {
            when(source.getConnection()).thenAnswer(invocation -> { activeConnections.incrementAndGet(); return connection; });
            doAnswer(invocation -> { activeConnections.decrementAndGet(); return null; }).when(connection).close();
            when(connection.createStatement()).thenReturn(schema);
            when(connection.prepareStatement(anyString())).thenAnswer(invocation -> {
                String sql = invocation.getArgument(0);
                if (sql.startsWith("SELECT GET_LOCK")) return acquire;
                if (sql.startsWith("SELECT RELEASE_LOCK")) return release;
                if (sql.startsWith("SELECT id")) { selectSql = sql; return select; }
                if (sql.startsWith("DELETE")) { deleteSql = sql; return delete; }
                if (sql.startsWith("INSERT")) { insertSql = sql; return insert; }
                throw new AssertionError("Unexpected SQL shape");
            });
            ResultSet acquired = mock(ResultSet.class);
            when(acquired.next()).thenReturn(true);
            when(acquired.getInt(1)).thenReturn(1);
            when(acquire.executeQuery()).thenReturn(acquired);
            when(releaseResult.next()).thenReturn(true);
            when(releaseResult.getInt(1)).thenReturn(1);
            when(release.executeQuery()).thenReturn(releaseResult);
            doAnswer(invocation -> { cursor = invocation.getArgument(1); return null; }).when(select).setLong(eq(2), anyLong());
            when(select.executeQuery()).thenAnswer(invocation -> {
                List<Row> page = rows.stream().filter(row -> row.id > cursor).limit(50).toList();
                AtomicInteger index = new AtomicInteger(-1);
                ResultSet result = mock(ResultSet.class);
                when(result.next()).thenAnswer(unused -> index.incrementAndGet() < page.size());
                when(result.getLong("id")).thenAnswer(unused -> page.get(index.get()).id);
                when(result.getString("source")).thenReturn("proxy");
                when(result.getString("destination")).thenReturn(SERVER);
                when(result.getCharacterStream("payload")).thenAnswer(unused -> {
                    String payload = page.get(index.get()).payload;
                    return payload == null ? null : new StringReader(payload);
                });
                return result;
            });
            doAnswer(invocation -> { deleteId = invocation.getArgument(1); return null; }).when(delete).setLong(eq(1), anyLong());
            when(delete.executeUpdate()).thenAnswer(invocation -> {
                if (failDelete) throw new SQLException("synthetic delete failure");
                deleted.add(deleteId);
                rows.removeIf(row -> row.id == deleteId);
                return 1;
            });
        }
    }
}
