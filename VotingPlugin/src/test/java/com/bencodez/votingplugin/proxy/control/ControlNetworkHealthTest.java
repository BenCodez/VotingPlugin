package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.google.gson.*;
import java.net.URI;
import java.util.*;
import java.util.concurrent.*;
import java.lang.reflect.Field;
import org.junit.jupiter.api.Test;

class ControlNetworkHealthTest {
    private static final UUID SESSION = UUID.randomUUID();
    private static final String ID = UUID.randomUUID().toString(), ATTEMPT = UUID.randomUUID().toString();
    private static void field(ControlConnector connector, String name, Object value) throws Exception {
        Field field = ControlConnector.class.getDeclaredField(name); field.setAccessible(true); field.set(connector, value);
    }
    private static void poll(ControlConnector connector) throws Exception {
        var method = ControlConnector.class.getDeclaredMethod("pollNetworkHealth"); method.setAccessible(true); method.invoke(connector);
    }
    private ControlConnector connector(String platform, ControlConnector.Transport transport, ScheduledExecutorService executor) throws Exception {
        var settings = new ControlConnector.Settings("proxy-a", "a", platform, "1", URI.create("http://localhost:8080"), 10, 1000, 1000);
        var connector = new ControlConnector(settings, executor, transport, List::of, ignored -> {}, SESSION, () -> 0);
        var proxy = mock(VotingPluginProxy.class); var config = mock(VotingPluginProxyConfig.class);
        when(proxy.getConfig()).thenReturn(config);
        when(proxy.getInstalledPluginNames()).thenReturn(List.of("VotingPlugin", "VotifierPlus"));
        when(proxy.getAllConfiguredServers()).thenReturn(Set.of("backend-a"));
        when(config.getBungeeMethod()).thenReturn("PLUGINMESSAGING");
        when(config.getSharedTransportAuthentication()).thenReturn("COMPATIBILITY");
        when(config.getMultiProxyMethod()).thenReturn("SOCKET");
        field(connector, "proxy", proxy);
        field(connector, "registered", true); field(connector, "status", ControlConnector.Status.CONNECTED);
        field(connector, "acceptedCapabilities", Set.of("data.inspect.v1", "data.network-health.v1"));
        return connector;
    }
    private static String task(String kind, String filters) {
        return "{\"inspectionId\":\"" + ID + "\",\"attemptId\":\"" + ATTEMPT + "\",\"query\":{\"kind\":\"" + kind + "\",\"filters\":" + filters + "}}";
    }
    @Test void bothProxyPlatformsReportInventoryAndCompleteATypedInspection() throws Exception {
        for (String platform : List.of("BUNGEECORD", "VELOCITY")) {
            var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
            ControlConnector.Transport transport = request -> { requests.add(request); return CompletableFuture.completedFuture(new ControlConnector.Response(200, requests.size() == 1 ? task("network-health", "{}") : "{}")); };
            try (var connector = connector(platform, transport, executor)) {
                var registration = ControlConnector.class.getDeclaredMethod("registrationRequest"); registration.setAccessible(true);
                var body = JsonParser.parseString(((ControlConnector.Request)registration.invoke(connector)).body()).getAsJsonObject();
                assertEquals(platform, body.get("platform").getAsString());
                assertEquals(List.of("VotifierPlus", "VotingPlugin"), body.getAsJsonArray("detectedPlugins").asList().stream().map(JsonElement::getAsString).toList());
                poll(connector);
                assertEquals(2, requests.size());
                var result = JsonParser.parseString(requests.get(1).body()).getAsJsonObject();
                assertTrue(result.get("success").getAsBoolean()); assertEquals("OK", result.get("code").getAsString());
                assertFalse(result.get("message").getAsString().isBlank()); assertEquals(ATTEMPT, result.get("attemptId").getAsString());
                var evidence = result.getAsJsonObject("data").getAsJsonObject("result");
                assertEquals("PROXY", evidence.get("role").getAsString());
                assertTrue(evidence.get("topologyComplete").getAsBoolean());
                assertEquals("backend-a", evidence.getAsJsonArray("backendNames").get(0).getAsString());
                assertFalse(evidence.has("activeMethod"), "Configured method must not fabricate active runtime evidence");
                assertFalse(result.toString().contains("Password"));
                String fixtureDir = System.getProperty("network.health.fixture.dir");
                if (fixtureDir != null) {
                    java.nio.file.Path dir = java.nio.file.Path.of(fixtureDir);
                    java.nio.file.Files.createDirectories(dir);
                    java.nio.file.Files.writeString(dir.resolve(platform + ".json"), evidence.toString());
                }
            } finally { executor.shutdownNow(); }
        }
    }
    @Test void producerUsesRuntimeAliasesAndVotePartyListApplicability() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor();
        try (var connector = connector("VELOCITY", request -> CompletableFuture.completedFuture(new ControlConnector.Response(204, "")), executor)) {
            var proxyField = ControlConnector.class.getDeclaredField("proxy"); proxyField.setAccessible(true);
            var config = ((VotingPluginProxy)proxyField.get(connector)).getConfig();
            when(config.getSharedTransportAuthentication()).thenReturn(" REQUIRED ");
            when(config.getProxyBroadcastEnabled()).thenReturn(true);
            when(config.getProxyBroadcastScopeMode()).thenReturn(" PLAYER ");
            when(config.getProxyBroadcastOfflineMode()).thenReturn(" FORWARD ");
            when(config.getVotePartyEnabled()).thenReturn(true); when(config.getVotePartySendToAllServers()).thenReturn(true);
            var method = ControlConnector.class.getDeclaredMethod("proxyNetworkHealth"); method.setAccessible(true);
            var data = (JsonObject)method.invoke(connector);
            assertEquals("REQUIRED", data.get("sharedAuthentication").getAsString());
            assertTrue(data.get("offlineForwardServersApplicable").getAsBoolean());
            assertFalse(data.get("broadcastServersApplicable").getAsBoolean());
            assertFalse(data.get("votePartyServersApplicable").getAsBoolean());
            when(config.getProxyBroadcastScopeMode()).thenReturn(" SERVERS ");
            when(config.getVotePartySendToAllServers()).thenReturn(false);
            data = (JsonObject)method.invoke(connector);
            assertTrue(data.get("broadcastServersApplicable").getAsBoolean());
            assertFalse(data.get("offlineForwardServersApplicable").getAsBoolean());
            assertTrue(data.get("votePartyServersApplicable").getAsBoolean());
        } finally { executor.shutdownNow(); }
    }
    @Test void oldControlCannotActivateHealthPollingAndFiltersAreRejected() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
        try (var connector = connector("VELOCITY", request -> { requests.add(request); return CompletableFuture.completedFuture(new ControlConnector.Response(200, task("network-health", "{\"path\":\"secret\"}"))); }, executor)) {
            field(connector, "acceptedCapabilities", Set.of("data.inspect.v1")); poll(connector); assertTrue(requests.isEmpty());
            field(connector, "acceptedCapabilities", Set.of("data.inspect.v1", "data.network-health.v1")); poll(connector);
            assertEquals(1, requests.size(), "Invalid filters must never trigger a result snapshot");
        } finally { executor.shutdownNow(); }
    }

    @Test void recoveryConnectorDoesNotClaimNewDiagnosticsOrExposeInventory() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
        try (var connector = connector("VELOCITY", request -> { requests.add(request); return CompletableFuture.completedFuture(new ControlConnector.Response(204, "")); }, executor)) {
            field(connector, "recovering", true);
            poll(connector);
            assertTrue(requests.isEmpty());
            var method = ControlConnector.class.getDeclaredMethod("registrationRequest"); method.setAccessible(true);
            var registration = (ControlConnector.Request) method.invoke(connector);
            assertFalse(JsonParser.parseString(registration.body()).getAsJsonObject().has("detectedPlugins"));
        } finally { executor.shutdownNow(); }
    }

    @Test void collectionFailureSettlesClaimWithGenericInspectionFailure() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
        try (var connector = connector("VELOCITY", request -> {
            requests.add(request);
            if (requests.size() == 1) return CompletableFuture.completedFuture(new ControlConnector.Response(200, task("network-health", "{}")));
            return CompletableFuture.completedFuture(new ControlConnector.Response(200, "{}"));
        }, executor)) {
            var proxyField = ControlConnector.class.getDeclaredField("proxy"); proxyField.setAccessible(true);
            when(((VotingPluginProxy) proxyField.get(connector)).getAllConfiguredServers()).thenThrow(new IllegalStateException("secret details"));
            poll(connector);
            assertEquals(2, requests.size());
            assertTrue(requests.get(1).body().contains("\"code\":\"INSPECTION_FAILED\""));
            assertFalse(requests.get(1).body().contains("secret details"));
        } finally { executor.shutdownNow(); }
    }

    @Test void inspectionEchoesOpaqueAttemptIdsAndRejectsMalformedBounds() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
        String opaque = "attempt-from-control-7f3a";
        try (var connector = connector("VELOCITY", request -> {
            requests.add(request);
            if (requests.size() == 1) return CompletableFuture.completedFuture(new ControlConnector.Response(200,
                    taskWithAttempt("network-health", "{}", opaque)));
            return CompletableFuture.completedFuture(new ControlConnector.Response(200, "{}"));
        }, executor)) {
            poll(connector);
            assertEquals(opaque, JsonParser.parseString(requests.get(1).body()).getAsJsonObject().get("attemptId").getAsString());
        } finally { executor.shutdownNow(); }

        requests.clear();
        try (var connector = connector("VELOCITY", request -> {
            requests.add(request);
            return CompletableFuture.completedFuture(new ControlConnector.Response(200,
                    taskWithAttempt("network-health", "{}", "x".repeat(257))));
        }, executor)) {
            poll(connector);
            assertEquals(1, requests.size());
        } finally { executor.shutdownNow(); }
    }

    private static String taskWithAttempt(String kind, String filters, String attempt) {
        return "{\"inspectionId\":\"" + ID + "\",\"attemptId\":\"" + attempt
                + "\",\"query\":{\"kind\":\"" + kind + "\",\"filters\":" + filters + "}}";
    }
    @Test void shutdownCancelsThePendingClaimAndCannotPostItsLateResult() throws Exception {
        var executor = Executors.newSingleThreadScheduledExecutor(); List<ControlConnector.Request> requests = new ArrayList<>();
        var pending = new CompletableFuture<ControlConnector.Response>();
        try (var connector = connector("VELOCITY", request -> { requests.add(request); return pending; }, executor)) {
            poll(connector); connector.close();
            assertTrue(pending.isCancelled());
            pending.complete(new ControlConnector.Response(200, task("network-health", "{}")));
            assertEquals(1, requests.size());
        } finally { executor.shutdownNow(); }
    }
}
