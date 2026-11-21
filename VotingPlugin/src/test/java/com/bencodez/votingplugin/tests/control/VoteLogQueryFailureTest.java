package com.bencodez.votingplugin.tests.control;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;
import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.control.ControlInspectionService;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogReadScope;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

class VoteLogQueryFailureTest {
    @Test void searchFailureAndRecovery() throws Exception { actualQueryFailureAfterSuccessfulProbeIsUnavailableAndRecoveryRetainsRows("vote-log-search"); }
    @Test void traceFailureAndRecovery() throws Exception { actualQueryFailureAfterSuccessfulProbeIsUnavailableAndRecoveryRetainsRows("vote-trace"); }

    void actualQueryFailureAfterSuccessfulProbeIsUnavailableAndRecoveryRetainsRows(String kind) throws Exception {
        Fixture f = new Fixture();
        JsonObject query = query(kind);
        assertEquals(1, rows(f.service.inspect(query)).size());
        f.failAt = 1;
        var failure = assertThrows(ControlInspectionService.InspectionUnavailableException.class,
                () -> f.service.inspect(query));
        assertEquals("vote log query is unavailable", failure.getMessage());
        assertNull(failure.getCause());
        assertEquals(2, f.probes.get());
        assertEquals(1, f.queries.get());
        f.failAt = 0;
        assertEquals("Example", rows(f.service.inspect(query)).get(0).getAsJsonObject().get("playerName").getAsString());
        f.empty = true;
        assertEquals(0, rows(f.service.inspect(query)).size());
    }

    @Test void summaryFailures() throws Exception {
        for (int stage = 1; stage <= 4; stage++) summaryRejectsFailureOfEachDependentAggregate(stage);
    }

    void summaryRejectsFailureOfEachDependentAggregate(int failedQuery) throws Exception {
        Fixture f = new Fixture();
        f.failAt = failedQuery;
        assertThrows(ControlInspectionService.InspectionUnavailableException.class,
                () -> f.service.inspect(query("vote-log-summary")));
        assertEquals(failedQuery, f.queries.get());
        f.failAt = 0;
        assertEquals(1, f.service.inspect(query("vote-log-summary")).getAsJsonObject("result").get("total").getAsLong());
    }

    @Test void failedProbeIsUnavailableAndDoesNotRunDependentQuery() throws Exception {
        Fixture f = new Fixture();
        f.failProbe = true;
        assertThrows(ControlInspectionService.InspectionUnavailableException.class,
                () -> f.service.inspect(query("vote-log-search")));
        assertEquals(0, f.queries.get());
    }

    @Test void healthAndLegacyReadsHaveDistinctFailureSemantics() throws Exception {
        Fixture f = new Fixture();
        f.failAt = 1;
        for (Runnable read : List.<Runnable>of(() -> f.table.getServiceHealth(30, 10),
                () -> f.table.getServiceHealthForServices(30, List.of("example")))) {
            f.queries.set(0);
            assertThrows(IllegalStateException.class, () -> VoteLogReadScope.read(() -> {
                read.run(); return null;
            }, () -> { throw new IllegalStateException("unavailable"); }));
        }
        f.queries.set(0);
        assertTrue(f.table.getRecent(30, null, 10).isEmpty());
        f.queries.set(0);
        assertEquals(0, f.table.getCounts(30).total);
    }

    @Test void nestedScopesRestoreAndConcurrentLegacyReaderIsUnaffected() throws Exception {
        Fixture f = new Fixture();
        f.failAt = 1;
        var worker = Executors.newSingleThreadExecutor();
        try {
            VoteLogReadScope.read(() -> {
                assertThrows(IllegalArgumentException.class, () -> VoteLogReadScope.read(() -> {
                    f.table.getRecent(30, null, 10); return null;
                }, () -> { throw new IllegalArgumentException("inner"); }));
                f.queries.set(0);
                assertThrows(IllegalStateException.class, () -> f.table.getRecent(30, null, 10));
                f.queries.set(0);
                try {
                    assertTrue(worker.submit(() -> f.table.getRecent(30, null, 10).isEmpty()).get(5, TimeUnit.SECONDS));
                } catch (Exception failure) { throw new AssertionError(failure); }
                return null;
            }, () -> { throw new IllegalStateException("outer"); });
        } finally {
            worker.shutdownNow();
            assertTrue(worker.awaitTermination(5, TimeUnit.SECONDS));
        }
        f.queries.set(0);
        assertTrue(f.table.getRecent(30, null, 10).isEmpty());
    }

    private static com.google.gson.JsonArray rows(JsonObject envelope) {
        JsonObject result = envelope.getAsJsonObject("result");
        return result.has("entries") ? result.getAsJsonArray("entries") : result.getAsJsonArray("events");
    }

    private static JsonObject query(String kind) {
        JsonObject query = new JsonObject();
        query.addProperty("kind", kind);
        query.add("filters", JsonParser.parseString(kind.equals("vote-trace")
                ? "{\"voteId\":\"00000000-0000-0000-0000-000000000001\"}" : "{}"));
        return query;
    }

    private static final class Fixture {
        final VoteLogMysqlTable table = mock(VoteLogMysqlTable.class, CALLS_REAL_METHODS);
        final ControlInspectionService service;
        final AtomicInteger probes = new AtomicInteger();
        final AtomicInteger queries = new AtomicInteger();
        int failAt;
        boolean failProbe;
        boolean empty;

        Fixture() throws Exception {
            MySQL sql = mock(MySQL.class, RETURNS_DEEP_STUBS);
            Field mysql = com.bencodez.simpleapi.sql.mysql.AbstractSqlTable.class.getDeclaredField("mysql");
            mysql.setAccessible(true);
            mysql.set(table, sql);
            doReturn("VoteLog").when(table).getTableName();
            Connection connection = mock(Connection.class);
            when(sql.getConnectionManager().getConnection()).thenReturn(connection);
            when(connection.prepareStatement(anyString())).thenAnswer(call -> {
                boolean probe = call.<String>getArgument(0).contains("WHERE 1=0");
                PreparedStatement statement = mock(PreparedStatement.class);
                when(statement.executeQuery()).thenAnswer(ignored -> {
                    if (probe) {
                        probes.incrementAndGet();
                        queries.set(0);
                        if (failProbe) throw new SQLException("synthetic unavailable");
                    } else if (queries.incrementAndGet() == failAt) throw new SQLException("synthetic unavailable");
                    ResultSet result = mock(ResultSet.class);
                    when(result.next()).thenReturn(!empty, false);
                    when(result.getString(anyString())).thenReturn("Example");
                    when(result.getString("vote_id")).thenReturn("00000000-0000-0000-0000-000000000001");
                    when(result.getLong(anyString())).thenReturn(1L);
                    return result;
                });
                return statement;
            });
            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            when(plugin.getConfigFile().isVoteLoggingEnabled()).thenReturn(true);
            when(plugin.getVoteLogMysqlTable()).thenReturn(table);
            service = new ControlInspectionService(plugin);
        }
    }
}
