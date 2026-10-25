package com.bencodez.votingplugin.tests.util;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.util.*;
import com.bencodez.votingplugin.util.SqliteNativeLibrary.*;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;

/** Typed test-only access to package-private SqliteNativeLibrary seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SqliteNativeLibraryAccess {
    public static final String DRIVER_FILE = (String) ReflectiveAccess.field("com.bencodez.votingplugin.util.SqliteNativeLibrary", "DRIVER_FILE");
    private SqliteNativeLibraryAccess() { }
    public static Path prepareNative(Path directory, String folder, ClassLoader resourceLoader, SqliteNativeLibraryAccess.ArtifactFetcher fetcher) throws IOException { return (Path) ReflectiveAccess.call("com.bencodez.votingplugin.util.SqliteNativeLibrary", null, "prepareNative", new String[]{"Path", "String", "ClassLoader", "ArtifactFetcher"}, new Object[]{directory, folder, resourceLoader, fetcher}); }
    @FunctionalInterface
    public interface ArtifactFetcher {
        void fetch(URI source, Path target) throws IOException;
    }

}
