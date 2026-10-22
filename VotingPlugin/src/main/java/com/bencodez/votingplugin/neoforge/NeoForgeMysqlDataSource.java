package com.bencodez.votingplugin.neoforge;

import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.logging.Logger;

import javax.sql.DataSource;

import org.mariadb.jdbc.Configuration;
import org.mariadb.jdbc.Driver;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

/** The bundled, privately relocated driver is selected directly, without global JDBC registration lookup. */
final class NeoForgeMysqlDataSource {
    private NeoForgeMysqlDataSource() { }

    static Configuration configuration(NeoForgeProxyTransportConfiguration.Mysql settings) {
        // Programmatic fields keep database names and opaque credentials out of URL option parsing.
        return new Configuration.Builder().addHost(settings.host(), settings.port()).database(settings.database())
                .user(settings.username()).password(settings.password()).connectTimeout(2000).socketTimeout(2000)
                .sslMode(settings.ssl() ? "verify-full" : "disable")
                .allowPublicKeyRetrieval(settings.publicKeyRetrieval()).allowLocalInfile(false)
                .allowMultiQueries(false).dumpQueriesOnException(false).build();
    }

    static HikariDataSource open(NeoForgeProxyTransportConfiguration.Mysql settings) {
        HikariConfig pool = new HikariConfig();
        pool.setPoolName("VotingPlugin-NeoForge-MySQL");
        pool.setDataSource(new Source(configuration(settings)));
        pool.setMaximumPoolSize(settings.maxConnections());
        pool.setMinimumIdle(0);
        pool.setConnectionTimeout(2000);
        pool.setValidationTimeout(1000);
        pool.setInitializationFailTimeout(-1);
        return new HikariDataSource(pool);
    }

    private static final class Source implements DataSource {
        private final Configuration configuration;
        Source(Configuration configuration) {
            // Direct use still initializes Connector/J's global registration.
            // Remove only this private driver class before admitting pool work,
            // including startup/connection failures that never reach shutdown.
            new Driver();
            var registered = java.sql.DriverManager.getDrivers();
            while (registered.hasMoreElements()) {
                java.sql.Driver driver = registered.nextElement();
                if (driver.getClass() == Driver.class) {
                    try { java.sql.DriverManager.deregisterDriver(driver); }
                    catch (SQLException failure) {
                        throw new IllegalStateException("Unable to isolate transport driver");
                    }
                }
            }
            this.configuration = configuration;
        }
        @Override public Connection getConnection() throws SQLException { return Driver.connect(configuration); }
        @Override public Connection getConnection(String user, String password) throws SQLException {
            throw new SQLFeatureNotSupportedException("Use the configured transport credentials");
        }
        @Override public PrintWriter getLogWriter() { return null; }
        @Override public void setLogWriter(PrintWriter writer) throws SQLException {
            if (writer != null) throw new SQLFeatureNotSupportedException("Transport diagnostics are redacted");
        }
        @Override public int getLoginTimeout() { return 2; }
        @Override public void setLoginTimeout(int seconds) throws SQLException {
            if (seconds < 0) throw new SQLException("Invalid login timeout");
            // The immutable driver configuration retains the stricter two-second connection bound.
        }
        @Override public Logger getParentLogger() { return Logger.getLogger("VotingPlugin-NeoForge-MySQL"); }
        @Override public <T> T unwrap(Class<T> type) throws SQLException {
            if (type.isInstance(this)) return type.cast(this);
            throw new SQLException("Unsupported datasource wrapper");
        }
        @Override public boolean isWrapperFor(Class<?> type) { return type.isInstance(this); }
    }
}
