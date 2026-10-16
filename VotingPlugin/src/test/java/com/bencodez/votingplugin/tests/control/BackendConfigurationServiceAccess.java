package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import com.bencodez.votingplugin.control.BackendConfigurationService.*;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.channels.FileChannel;
import java.util.List;
import java.util.Map;

/** Typed test-only access to package-private BackendConfigurationService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendConfigurationServiceAccess {
    public static final int MAX_REWARD_DIRECTORY_ENTRIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.control.BackendConfigurationService", "MAX_REWARD_DIRECTORY_ENTRIES");
    private BackendConfigurationServiceAccess() { }
    public static BackendConfigurationService create(Path dataDirectory, ReloadAction reload) { return (BackendConfigurationService) ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendConfigurationService", new String[]{"Path", "ReloadAction"}, new Object[]{dataDirectory, reload}); }
    public static BackendConfigurationService create(Path dataDirectory, ApplyAction reload) { return (BackendConfigurationService) ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendConfigurationService", new String[]{"Path", "ApplyAction"}, new Object[]{dataDirectory, reload}); }
    public static BackendConfigurationService create(Path dataDirectory, ApplyAction reload, BackendConfigurationServiceAccess.MoveAction mover) { return (BackendConfigurationService) ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendConfigurationService", new String[]{"Path", "ApplyAction", "MoveAction"}, new Object[]{dataDirectory, reload, mover}); }
    public static boolean supportsNamedRewardFiles(BackendConfigurationService target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "supportsNamedRewardFiles", new String[]{}, new Object[]{}); }
    public static boolean supportsNamedRewardFiles(BackendConfigurationService target, BackendConfigurationServiceAccess.DirectoryStagingSupport staging) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "supportsNamedRewardFiles", new String[]{"DirectoryStagingSupport"}, new Object[]{staging}); }
    public static boolean supportsNamedRewardFiles(Path directory) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "supportsNamedRewardFiles", new String[]{"Path"}, new Object[]{directory}); }
    public static boolean supportsNamedRewardFiles(Path directory, BackendConfigurationServiceAccess.DirectoryForcer force) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "supportsNamedRewardFiles", new String[]{"Path", "DirectoryForcer"}, new Object[]{directory, force}); }
    public static boolean supportsNamedRewardFiles(Path directory, BackendConfigurationServiceAccess.DirectoryForcer force, BackendConfigurationServiceAccess.DirectoryStagingSupport staging) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "supportsNamedRewardFiles", new String[]{"Path", "DirectoryForcer", "DirectoryStagingSupport"}, new Object[]{directory, force, staging}); }
    public static List<String> rewardFileInventory(Path dataDirectory) throws IOException { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "rewardFileInventory", new String[]{"Path"}, new Object[]{dataDirectory}); }
    public static List<String> rewardFileInventory(Path dataDirectory, BackendConfigurationServiceAccess.RewardDirectoryAction<List<String>> scan) throws IOException { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "rewardFileInventory", new String[]{"Path", "RewardDirectoryAction"}, new Object[]{dataDirectory, scan}); }
    public static String readRaw(java.nio.file.SecureDirectoryStream<Path> directory, String name, boolean allowMissing) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "readRaw", new String[]{"SecureDirectoryStream", "String", "boolean"}, new Object[]{directory, name, allowMissing}); }
    public static boolean namedRewardCannotBeConfirmed(IOException failure) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "namedRewardCannotBeConfirmed", new String[]{"IOException"}, new Object[]{failure}); }
    public static <T> T retryRead(BackendConfigurationServiceAccess.ReadAction<T> read) throws IOException { return (T) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "retryRead", new String[]{"ReadAction"}, new Object[]{read}); }
    public static String proposedQuickSetupRevision(BackendConfigurationService target, String preset, QuickPreview preview) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "proposedQuickSetupRevision", new String[]{"String", "QuickPreview"}, new Object[]{preset, preview}); }
    public static String currentQuickSetupRevision(BackendConfigurationService target, String preset) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "currentQuickSetupRevision", new String[]{"String"}, new Object[]{preset}); }
    public static String currentQuickSetupRevision(BackendConfigurationService target, String preset, Map<String, String> options) throws IOException { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "currentQuickSetupRevision", new String[]{"String", "Map"}, new Object[]{preset, options}); }
    public static ApplyResult applyQuickSetup(BackendConfigurationService target, String preset, Map<String, String> options, String expectedRevision) throws IOException { return (ApplyResult) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "applyQuickSetup", new String[]{"String", "Map", "String"}, new Object[]{preset, options, expectedRevision}); }
    public static ApplyResult applyQuickSetup(BackendConfigurationService target, String preset, Map<String, String> options, String expectedRevision, ApplyAction applyAction) throws IOException { return (ApplyResult) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", target, "applyQuickSetup", new String[]{"String", "Map", "String", "ApplyAction"}, new Object[]{preset, options, expectedRevision, applyAction}); }
    public static List<String> sanitizeComments(List<String> comments, List<String> secretValues, boolean secretPath) { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendConfigurationService", null, "sanitizeComments", new String[]{"List", "List", "boolean"}, new Object[]{comments, secretValues, secretPath}); }
    @FunctionalInterface
    public interface ReadAction<T> {
        T run() throws IOException;
    }

    @FunctionalInterface
    public interface DirectoryForcer {
        void force(FileChannel channel) throws IOException;
    }

    @FunctionalInterface
    public interface DirectoryStagingSupport {
        boolean supports(java.nio.file.SecureDirectoryStream<Path> directory) throws IOException;
    }

    @FunctionalInterface
    public interface RewardDirectoryAction<T> {
        T run(java.nio.file.SecureDirectoryStream<Path> directory) throws IOException;
    }

    @FunctionalInterface
    public interface MoveAction {
        void move(Path source, Path target) throws IOException;
    }

}
