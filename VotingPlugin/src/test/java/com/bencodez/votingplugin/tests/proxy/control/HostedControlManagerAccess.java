package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.control.*;
import com.bencodez.votingplugin.proxy.control.HostedControlManager.*;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import com.bencodez.votingplugin.util.ControlCredentialFile.PendingAutoEnrollment;

/** Typed test-only access to package-private HostedControlManager seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class HostedControlManagerAccess {
    private HostedControlManagerAccess() { }
    public static HostedControlManager create(HostedControlManagerAccess.Settings settings, ScheduledExecutorService executor, HostedControlManagerAccess.ArtifactDownloader downloader, HostedControlManagerAccess.ProcessLauncher launcher, HostedControlManagerAccess.HealthProbe healthProbe, HostedControlManagerAccess.Sleeper sleeper, LongSupplier nanoTime, Consumer<String> logger) { return (HostedControlManager) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager", new String[]{"Settings", "ScheduledExecutorService", "ArtifactDownloader", "ProcessLauncher", "HealthProbe", "Sleeper", "LongSupplier", "Consumer"}, new Object[]{settings, executor, downloader, launcher, healthProbe, sleeper, nanoTime, logger}); }
    public static HostedControlManager create(HostedControlManagerAccess.Settings settings, ScheduledExecutorService executor, HostedControlManagerAccess.ArtifactDownloader downloader, HostedControlManagerAccess.ProcessLauncher launcher, HostedControlManagerAccess.VerifierInstaller verifierInstaller, HostedControlManagerAccess.HealthProbe healthProbe, HostedControlManagerAccess.Sleeper sleeper, LongSupplier nanoTime, PendingAutoEnrollment localAutoEnrollment, Consumer<String> logger) { return (HostedControlManager) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager", new String[]{"Settings", "ScheduledExecutorService", "ArtifactDownloader", "ProcessLauncher", "VerifierInstaller", "HealthProbe", "Sleeper", "LongSupplier", "PendingAutoEnrollment", "Consumer"}, new Object[]{settings, executor, downloader, launcher, verifierInstaller, healthProbe, sleeper, nanoTime, localAutoEnrollment, logger}); }
    public static HostedControlManager create(HostedControlManagerAccess.Settings settings, ScheduledExecutorService executor, HostedControlManagerAccess.ArtifactDownloader downloader, HostedControlManagerAccess.ReleaseResolver releaseResolver, HostedControlManagerAccess.ProcessLauncher launcher, HostedControlManagerAccess.VerifierInstaller verifierInstaller, HostedControlManagerAccess.HealthProbe healthProbe, HostedControlManagerAccess.Sleeper sleeper, LongSupplier nanoTime, PendingAutoEnrollment localAutoEnrollment, Consumer<String> logger) { return (HostedControlManager) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager", new String[]{"Settings", "ScheduledExecutorService", "ArtifactDownloader", "ReleaseResolver", "ProcessLauncher", "VerifierInstaller", "HealthProbe", "Sleeper", "LongSupplier", "PendingAutoEnrollment", "Consumer"}, new Object[]{settings, executor, downloader, releaseResolver, launcher, verifierInstaller, healthProbe, sleeper, nanoTime, localAutoEnrollment, logger}); }
    public static void runOnce(HostedControlManager target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", target, "runOnce", new String[]{}, new Object[]{}); }
    public static void checkForLatestUpdate(HostedControlManager target, HostedControlManagerAccess.ManagedProcess process) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", target, "checkForLatestUpdate", new String[]{"ManagedProcess"}, new Object[]{process}); }
    public static void stopProcess(HostedControlManagerAccess.ManagedProcess process, boolean wait) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", null, "stopProcess", new String[]{"ManagedProcess", "boolean"}, new Object[]{process, wait}); }
    public static String sha256(Path file) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", null, "sha256", new String[]{"Path"}, new Object[]{file}); }
    public static Path resolveInside(Path root, String configured, String name) { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", null, "resolveInside", new String[]{"Path", "String", "String"}, new Object[]{root, configured, name}); }
    public static URI parseDownloadUri(String configured) { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager", null, "parseDownloadUri", new String[]{"String"}, new Object[]{configured}); }
    public static final class ArtifactSpec implements ReflectiveAccess.View {
        public ArtifactSpec() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$ArtifactSpec", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private ArtifactSpec(Object delegate) { this.delegate = delegate; }
        public static ArtifactSpec view(Object delegate) { return delegate == null ? null : new ArtifactSpec(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof ArtifactSpec value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public ArtifactSpec(URI downloadUri, String sha256, String version) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$ArtifactSpec", new String[]{"URI", "String", "String"}, new Object[]{downloadUri, sha256, version})); }
        public URI downloadUri() { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$ArtifactSpec", delegate, "downloadUri", new String[]{}, new Object[]{}); }
        public String sha256() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$ArtifactSpec", delegate, "sha256", new String[]{}, new Object[]{}); }
        public String version() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$ArtifactSpec", delegate, "version", new String[]{}, new Object[]{}); }
    }

    public static final class Settings implements ReflectiveAccess.View {
        public Settings() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private Settings(Object delegate) { this.delegate = delegate; }
        public static Settings view(Object delegate) { return delegate == null ? null : new Settings(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Settings value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public Settings(Path rootDirectory, Path jarFile, Path dataDirectory, boolean autoDownload, boolean autoUpdate, URI downloadUri, String sha256, String host, int port, int startupTimeoutSeconds, int downloadTimeoutSeconds) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", new String[]{"Path", "Path", "Path", "boolean", "boolean", "URI", "String", "String", "int", "int", "int"}, new Object[]{rootDirectory, jarFile, dataDirectory, autoDownload, autoUpdate, downloadUri, sha256, host, port, startupTimeoutSeconds, downloadTimeoutSeconds})); }
        public Path rootDirectory() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "rootDirectory", new String[]{}, new Object[]{}); }
        public Path jarFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "jarFile", new String[]{}, new Object[]{}); }
        public Path dataDirectory() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "dataDirectory", new String[]{}, new Object[]{}); }
        public boolean autoDownload() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "autoDownload", new String[]{}, new Object[]{}); }
        public boolean autoUpdate() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "autoUpdate", new String[]{}, new Object[]{}); }
        public URI downloadUri() { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "downloadUri", new String[]{}, new Object[]{}); }
        public String sha256() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "sha256", new String[]{}, new Object[]{}); }
        public String host() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "host", new String[]{}, new Object[]{}); }
        public int port() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "port", new String[]{}, new Object[]{}); }
        public int startupTimeoutSeconds() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "startupTimeoutSeconds", new String[]{}, new Object[]{}); }
        public int downloadTimeoutSeconds() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "downloadTimeoutSeconds", new String[]{}, new Object[]{}); }
        public Path failedFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "failedFile", new String[]{}, new Object[]{}); }
        public Path rollbackPendingFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "rollbackPendingFile", new String[]{}, new Object[]{}); }
        public Path quarantineFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "quarantineFile", new String[]{}, new Object[]{}); }
        public Path healthCheckingFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "healthCheckingFile", new String[]{}, new Object[]{}); }
        public Path releaseStateFile() { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "releaseStateFile", new String[]{}, new Object[]{}); }
        public boolean usesLatestRelease() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "usesLatestRelease", new String[]{}, new Object[]{}); }
        public URI endpoint() { return (URI) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$Settings", delegate, "endpoint", new String[]{}, new Object[]{}); }
    }

    @FunctionalInterface
    public interface ArtifactDownloader {
        void download(URI source, Path target, long maximumBytes, Duration timeout) throws IOException, InterruptedException;
    }

    @FunctionalInterface
    public interface ReleaseResolver {
        HostedControlManagerAccess.ArtifactSpec resolve(Duration timeout) throws IOException, InterruptedException;
    }

    @FunctionalInterface
    public interface ProcessLauncher {
        HostedControlManagerAccess.ManagedProcess launch(HostedControlManagerAccess.Settings settings, Path artifact, String launchId) throws IOException;
    }

    @FunctionalInterface
    public interface VerifierInstaller {
        void install(HostedControlManagerAccess.Settings settings, Path artifact, String nodeId, String verifier, Duration timeout) throws IOException, InterruptedException;
    }

    @FunctionalInterface
    public interface HealthProbe {
        boolean isHealthy(URI endpoint, Duration timeout, String launchId);
    }

    @FunctionalInterface
    public interface Sleeper {
        void sleep(long millis) throws InterruptedException;
    }

        public interface ManagedProcess {
        boolean isAlive();
        void destroy();
        void destroyForcibly();
        boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException;
        CompletableFuture<Void> onExit();
    }

    public static final class JdkGithubReleaseResolver implements ReflectiveAccess.View, com.bencodez.votingplugin.tests.proxy.control.HostedControlManagerAccess.ReleaseResolver {

        private final Object delegate;
        private JdkGithubReleaseResolver(Object delegate) { this.delegate = delegate; }
        public static JdkGithubReleaseResolver view(Object delegate) { return delegate == null ? null : new JdkGithubReleaseResolver(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof JdkGithubReleaseResolver value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public HostedControlManagerAccess.ArtifactSpec resolve(Duration timeout) throws IOException, InterruptedException { return HostedControlManagerAccess.ArtifactSpec.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkGithubReleaseResolver", delegate, "resolve", new String[]{"Duration"}, new Object[]{timeout})); }
        public static HostedControlManagerAccess.ArtifactSpec parseLatestRelease(byte[] body) throws IOException { return HostedControlManagerAccess.ArtifactSpec.view(ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkGithubReleaseResolver", null, "parseLatestRelease", new String[]{"byte[]"}, new Object[]{body})); }
    }

    public static final class JdkArtifactDownloader implements ReflectiveAccess.View, com.bencodez.votingplugin.tests.proxy.control.HostedControlManagerAccess.ArtifactDownloader {
        public JdkArtifactDownloader() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkArtifactDownloader", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private JdkArtifactDownloader(Object delegate) { this.delegate = delegate; }
        public static JdkArtifactDownloader view(Object delegate) { return delegate == null ? null : new JdkArtifactDownloader(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof JdkArtifactDownloader value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public void download(URI source, Path target, long maximumBytes, Duration timeout) throws IOException, InterruptedException { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkArtifactDownloader", delegate, "download", new String[]{"URI", "Path", "long", "Duration"}, new Object[]{source, target, maximumBytes, timeout}); }
    }

    public static final class JdkHealthProbe implements ReflectiveAccess.View, com.bencodez.votingplugin.tests.proxy.control.HostedControlManagerAccess.HealthProbe {
        public JdkHealthProbe() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkHealthProbe", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private JdkHealthProbe(Object delegate) { this.delegate = delegate; }
        public static JdkHealthProbe view(Object delegate) { return delegate == null ? null : new JdkHealthProbe(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof JdkHealthProbe value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public boolean isHealthy(URI endpoint, Duration timeout, String launchId) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.HostedControlManager$JdkHealthProbe", delegate, "isHealthy", new String[]{"URI", "Duration", "String"}, new Object[]{endpoint, timeout, launchId}); }
    }

}
