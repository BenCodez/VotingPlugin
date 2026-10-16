package com.bencodez.votingplugin.tests.proxy.velocity;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.velocity.*;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;
import com.google.gson.JsonObject;

/** Test-only forwarding access to the non-public production VelocityDatabaseDriverInstaller. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VelocityDatabaseDriverInstallerAccess implements ReflectiveAccess.View {
    public static final URI LATEST_RELEASE = (URI) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", "LATEST_RELEASE");
    public static final String MYSQL = (String) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", "MYSQL");
    public static final String MARIADB = (String) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", "MARIADB");
    public static final String POSTGRESQL = (String) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", "POSTGRESQL");
    private final Object delegate;
    private VelocityDatabaseDriverInstallerAccess(Object delegate) { this.delegate = delegate; }
    public static VelocityDatabaseDriverInstallerAccess view(Object delegate) { return delegate == null ? null : new VelocityDatabaseDriverInstallerAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof VelocityDatabaseDriverInstallerAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static VelocityDatabaseDriverInstallerAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller"))); }
    public VelocityDatabaseDriverInstallerAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public VelocityDatabaseDriverInstallerAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public VelocityDatabaseDriverInstallerAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public VelocityDatabaseDriverInstallerAccess(DriverProbe probe, Downloader downloader) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", new String[]{"DriverProbe", "Downloader"}, new Object[]{probe, downloader})); }
    public VelocityDatabaseDriverInstallerAccess(DriverProbe probe, Downloader downloader, Set<String> approvedDigests) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", new String[]{"DriverProbe", "Downloader", "Set"}, new Object[]{probe, downloader, approvedDigests})); }
    public boolean ready(List<MysqlConfig> configurations, boolean automatic, Path dataDirectory, Consumer<String> info, Consumer<String> warning) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", delegate, "ready", new String[]{"List", "boolean", "Path", "Consumer", "Consumer"}, new Object[]{configurations, automatic, dataDirectory, info, warning}); }

    public static String sha256(Path file) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", null, "sha256", new String[]{"Path"}, new Object[]{file}); }
    public static void validate(Path jar, String requiredDriver) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", null, "validate", new String[]{"Path", "String"}, new Object[]{jar, requiredDriver}); }
    public static URI releaseArtifact(JsonObject release) throws IOException { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", null, "releaseArtifact", new String[]{"JsonObject"}, new Object[]{release}); }
    public static InputStream request(URI uri, ConnectionFactory connections) throws IOException { return (InputStream) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.velocity.VelocityDatabaseDriverInstaller", null, "request", new String[]{"URI", "ConnectionFactory"}, new Object[]{uri, connections}); }
    @FunctionalInterface
    public interface DriverProbe {
        boolean available(String driver);
    }

    @FunctionalInterface
    public interface Downloader {
        void download(Path destination) throws IOException;
    }

    @FunctionalInterface
    public interface ConnectionFactory {
        HttpURLConnection open(URI uri) throws IOException;
    }

}
