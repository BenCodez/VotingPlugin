package com.bencodez.votingplugin.tests.proxy.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.control.*;
import com.bencodez.votingplugin.proxy.control.ControlConnector.*;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.function.Supplier;
import com.bencodez.votingplugin.control.PluginDeploymentService;
import com.bencodez.votingplugin.tests.proxy.control.ProxyControlResultStoreAccess.StoredResult;
import com.google.gson.JsonObject;

/** Typed test-only access to package-private ControlConnector seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ControlConnectorAccess {
    public static final int MAX_RESPONSE_BYTES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.control.ControlConnector", "MAX_RESPONSE_BYTES");
    private ControlConnectorAccess() { }
    public static ControlConnector create(Settings settings, ScheduledExecutorService scheduler, Transport transport, Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId, LongSupplier jitterSource) { return (ControlConnector) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ControlConnector", new String[]{"Settings", "ScheduledExecutorService", "Transport", "Supplier", "Consumer", "UUID", "LongSupplier"}, new Object[]{settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource}); }
    public static ControlConnector create(Settings settings, ScheduledExecutorService scheduler, Transport transport, Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId, LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService) { return (ControlConnector) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ControlConnector", new String[]{"Settings", "ScheduledExecutorService", "Transport", "Supplier", "Consumer", "UUID", "LongSupplier", "ProxyRoutingConfigurationService"}, new Object[]{settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, configurationService}); }
    public static ControlConnector create(Settings settings, ScheduledExecutorService scheduler, Transport transport, Supplier<List<ObservedBackend>> snapshotSource, Consumer<String> logger, UUID sessionId, LongSupplier jitterSource, ProxyRoutingConfigurationService configurationService, Map<UUID, StoredResult> recoveredTasks) { return (ControlConnector) ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.control.ControlConnector", new String[]{"Settings", "ScheduledExecutorService", "Transport", "Supplier", "Consumer", "UUID", "LongSupplier", "ProxyRoutingConfigurationService", "Map"}, new Object[]{settings, scheduler, transport, snapshotSource, logger, sessionId, jitterSource, configurationService, recoveredTasks}); }
    public static PluginDeploymentService.Task deploymentTask(JsonObject task) { return (PluginDeploymentService.Task) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "deploymentTask", new String[]{"JsonObject"}, new Object[]{task}); }
    public static boolean proxyDeploymentSupported(String osName) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "proxyDeploymentSupported", new String[]{"String"}, new Object[]{osName}); }
    public static void pollOperations(ControlConnector target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", target, "pollOperations", new String[]{}, new Object[]{}); }
    public static void cycle(ControlConnector target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", target, "cycle", new String[]{}, new Object[]{}); }
    public static boolean hasActiveOperation(ControlConnector target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", target, "hasActiveOperation", new String[]{}, new Object[]{}); }
    public static boolean requiresRuntimeReplacement(StoredResult result) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "requiresRuntimeReplacement", new String[]{"StoredResult"}, new Object[]{result}); }

    public static List<String> boundedResultChanges(List<String> changes) { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "boundedResultChanges", new String[]{"List"}, new Object[]{changes}); }
    public static void addCapabilities(JsonObject body, boolean configurationReady, boolean communicationReady, boolean methodReady, boolean fileReady, boolean deploymentReady) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "addCapabilities", new String[]{"JsonObject", "boolean", "boolean", "boolean", "boolean", "boolean"}, new Object[]{body, configurationReady, communicationReady, methodReady, fileReady, deploymentReady}); }
    public static long backoffMillis(int failures, long jitterValue) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "backoffMillis", new String[]{"int", "long"}, new Object[]{failures, jitterValue}); }
    public static String stableBackendId(String serverName) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.control.ControlConnector", null, "stableBackendId", new String[]{"String"}, new Object[]{serverName}); }
}
