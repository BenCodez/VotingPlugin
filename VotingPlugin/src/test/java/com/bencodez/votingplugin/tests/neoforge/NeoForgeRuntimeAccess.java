package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeRuntime.*;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Clock;

/** Typed test-only access to package-private NeoForgeRuntime seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeRuntimeAccess {
    public static final String USER_TABLE_NAME = (String) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", "USER_TABLE_NAME");
    private NeoForgeRuntimeAccess() { }
    public static NeoForgeRuntime start(Path directory) throws IOException { return (NeoForgeRuntime) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", null, "start", new String[]{"Path"}, new Object[]{directory}); }
    public static NeoForgeRuntime start(Path directory, Clock clock) throws IOException { return (NeoForgeRuntime) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", null, "start", new String[]{"Path", "Clock"}, new Object[]{directory, clock}); }
    public static NeoForgeRuntime start(Path directory, Object server) throws IOException { return (NeoForgeRuntime) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", null, "start", new String[]{"Path", "Object"}, new Object[]{directory, server}); }
    public static RuntimeException closeAll(Runnable... steps) { return (RuntimeException) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRuntime", null, "closeAll", new String[]{"Runnable[]"}, new Object[]{steps}); }
}
