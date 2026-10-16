package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import com.bencodez.votingplugin.control.PluginDeploymentService.*;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.function.BooleanSupplier;

/** Typed test-only access to package-private PluginDeploymentService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class PluginDeploymentServiceAccess {
    private PluginDeploymentServiceAccess() { }
    public static Result stage(PluginDeploymentService target, Task task, InputStream body, BooleanSupplier active) throws IOException { return (Result) ReflectiveAccess.call("com.bencodez.votingplugin.control.PluginDeploymentService", target, "stage", new String[]{"Task", "InputStream", "BooleanSupplier"}, new Object[]{task, body, active}); }
    public static void move(Path source, Path destination) throws IOException { ReflectiveAccess.call("com.bencodez.votingplugin.control.PluginDeploymentService", null, "move", new String[]{"Path", "Path"}, new Object[]{source, destination}); }
}
