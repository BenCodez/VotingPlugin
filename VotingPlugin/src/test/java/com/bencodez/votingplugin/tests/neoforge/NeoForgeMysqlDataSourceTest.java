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

    @Test void openingAndClosingPrivatePoolCannotRetainItsDriverOrRemoveUnrelatedDrivers() throws Exception {
        var privateRegistration = new org.mariadb.jdbc.Driver();
        java.sql.Driver unrelated = new UnrelatedDriver();
        java.sql.DriverManager.registerDriver(privateRegistration);
        java.sql.DriverManager.registerDriver(unrelated);
        try {
            for (int attempt = 0; attempt < 2; attempt++) {
                try (HikariDataSource pool = (HikariDataSource) ReflectiveAccess.call(OWNER, null, "open",
                        new String[] {"Mysql"}, new Object[] {settings("", false)})) {
                    assertFalse(java.sql.DriverManager.drivers().anyMatch(
                            driver -> driver.getClass() == org.mariadb.jdbc.Driver.class),
                            "the directly selected private driver must not be retained globally");
                    assertTrue(java.sql.DriverManager.drivers().anyMatch(driver -> driver == unrelated));
                    assertFalse(pool.isClosed());
                }
                assertFalse(java.sql.DriverManager.drivers().anyMatch(driver -> driver == privateRegistration));
                assertTrue(java.sql.DriverManager.drivers().anyMatch(driver -> driver == unrelated));
            }
        } finally {
            java.sql.DriverManager.deregisterDriver(privateRegistration);
            java.sql.DriverManager.deregisterDriver(unrelated);
        }
    }

    private static final class UnrelatedDriver implements java.sql.Driver {
        @Override public boolean acceptsURL(String url) { return false; }
        @Override public java.sql.Connection connect(String url, java.util.Properties info) { return null; }
        @Override public java.sql.DriverPropertyInfo[] getPropertyInfo(String url, java.util.Properties info) {
            return new java.sql.DriverPropertyInfo[0];
        }
        @Override public int getMajorVersion() { return 1; }
        @Override public int getMinorVersion() { return 0; }
        @Override public boolean jdbcCompliant() { return false; }
        @Override public java.util.logging.Logger getParentLogger() {
            return java.util.logging.Logger.getLogger("synthetic-driver-test");
        }
    }

    private static Object settings(String opaque, boolean ssl) {
        return ReflectiveAccess.construct(
                "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration$Mysql",
                new String[] {"String", "int", "String", "String", "String", "boolean", "boolean", "int"},
                new Object[] {"db.example", 3306, "fixture?sslMode=disable", opaque, opaque, ssl, false, 1});
    }
}
