package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mariadb.jdbc.Configuration;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.zaxxer.hikari.HikariDataSource;

class NeoForgeMysqlDataSourceTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeMysqlDataSource";

    @Test void programmaticDriverFieldsCannotBecomeUrlOptionsAndTlsVerifiesIdentity() {
        String opaque = " " + UUID.randomUUID() + " ";
        Object settings = settings(opaque, true);
        Configuration configuration = (Configuration) ReflectiveAccess.call(OWNER, null, "configuration",
                new String[] {"Mysql"}, new Object[] {settings});
        assertEquals("fixture?sslMode=disable", configuration.database());
        assertEquals(opaque, configuration.user());
        assertEquals(opaque, configuration.password());
        assertEquals("VERIFY_FULL", configuration.sslMode().name());
        assertEquals(2000, configuration.connectTimeout());
        assertEquals(2000, configuration.socketTimeout());
        assertFalse(configuration.allowLocalInfile());
        assertFalse(configuration.allowMultiQueries());
        assertFalse(configuration.dumpQueriesOnException());
        assertFalse(configuration.allowPublicKeyRetrieval());
    }

    @Test void poolStartsWithoutNetworkHasBoundedResourcesAndDoesNotExposeCredentials() {
        String opaque = UUID.randomUUID().toString();
        try (HikariDataSource pool = (HikariDataSource) ReflectiveAccess.call(OWNER, null, "open",
                new String[] {"Mysql"}, new Object[] {settings(opaque, false)})) {
            assertEquals(1, pool.getMaximumPoolSize());
            assertEquals(0, pool.getMinimumIdle());
            assertEquals(2000, pool.getConnectionTimeout());
            assertEquals(1000, pool.getValidationTimeout());
            assertEquals(-1, pool.getInitializationFailTimeout());
            assertEquals(2, pool.getDataSource().getLoginTimeout());
            assertFalse(pool.getDataSource().toString().contains(opaque));
            assertFalse(pool.getDataSource().toString().contains("fixture?sslMode"));
            assertNull(pool.getJdbcUrl());
        } catch (java.sql.SQLException failure) { throw new AssertionError(failure); }
    }

    private static Object settings(String opaque, boolean ssl) {
        return ReflectiveAccess.construct(
                "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration$Mysql",
                new String[] {"String", "int", "String", "String", "String", "boolean", "boolean", "int"},
                new Object[] {"db.example", 3306, "fixture?sslMode=disable", opaque, opaque, ssl, false, 1});
    }
}
