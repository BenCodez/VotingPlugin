package com.bencodez.votingplugin.tests.control;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.control.*;
import com.bencodez.votingplugin.control.BackendControlConnector.*;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import com.bencodez.votingplugin.tests.control.BackendControlResultStoreAccess.StoredResult;
import com.google.gson.JsonObject;

/** Typed test-only access to package-private BackendControlConnector seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendControlConnectorAccess {
    private BackendControlConnectorAccess() { }
    public static PluginDeploymentService.Task deploymentTask(JsonObject task) { return (PluginDeploymentService.Task) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "deploymentTask", new String[]{"JsonObject"}, new Object[]{task}); }
    public static BackendControlConnectorAccess.InspectionTaskResult executeInspection(BackendControlConnector target, JsonObject query) { return BackendControlConnectorAccess.InspectionTaskResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", target, "executeInspection", new String[]{"JsonObject"}, new Object[]{query})); }
    public static String inspectionFailureMessage(Throwable ignored) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "inspectionFailureMessage", new String[]{"Throwable"}, new Object[]{ignored}); }
    public static boolean taskLeaseExpired(BackendControlConnectorAccess.Response response) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "taskLeaseExpired", new String[]{"Response"}, new Object[]{response}); }
    public static boolean operationNotFound(BackendControlConnectorAccess.Response response) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "operationNotFound", new String[]{"Response"}, new Object[]{response}); }
    public static StoredResult abortedIntent(StoredResult pending) { return com.bencodez.votingplugin.tests.control.BackendControlResultStoreAccess.StoredResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "abortedIntent", new String[]{"StoredResult"}, new Object[]{pending})); }
    public static StoredResult committedInstalledForAttempt(BackendConfigurationService configurations, StoredResult pending, String attemptId) throws IOException { return com.bencodez.votingplugin.tests.control.BackendControlResultStoreAccess.StoredResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "committedInstalledForAttempt", new String[]{"BackendConfigurationService", "StoredResult", "String"}, new Object[]{configurations, pending, attemptId})); }
    public static void afterResultAcknowledged(BackendControlConnectorAccess.ResultSubmission submission, BackendControlConnectorAccess.ResultAcknowledgement acknowledged) throws Exception { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "afterResultAcknowledged", new String[]{"ResultSubmission", "ResultAcknowledgement"}, new Object[]{submission, acknowledged}); }
    public static String operationFailureMessage(String type, Throwable failure) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "operationFailureMessage", new String[]{"String", "Throwable"}, new Object[]{type, failure}); }
    public static String operationFailureCode(String type) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "operationFailureCode", new String[]{"String"}, new Object[]{type}); }
    public static String operationFailureCode(String type, Throwable failure) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "operationFailureCode", new String[]{"String", "Throwable"}, new Object[]{type, failure}); }
    public static String boundedResultMessage(String message) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "boundedResultMessage", new String[]{"String"}, new Object[]{message}); }
    public static List<String> boundedResultChanges(List<String> changes) { return (List<String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "boundedResultChanges", new String[]{"List"}, new Object[]{changes}); }
    public static boolean quickSetupCapabilityAccepted(String preset, boolean quickSetupsAccepted, boolean votePartySetupsAccepted, boolean voteSitesSyncAccepted) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "quickSetupCapabilityAccepted", new String[]{"String", "boolean", "boolean", "boolean"}, new Object[]{preset, quickSetupsAccepted, votePartySetupsAccepted, voteSitesSyncAccepted}); }
    public static boolean quickSetupCapabilityAccepted(String preset, boolean quickSetupsAccepted, boolean votePartySetupsAccepted, boolean voteSitesSyncAccepted, Map<String, String> options) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "quickSetupCapabilityAccepted", new String[]{"String", "boolean", "boolean", "boolean", "Map"}, new Object[]{preset, quickSetupsAccepted, votePartySetupsAccepted, voteSitesSyncAccepted, options}); }
    public static boolean proxyMethodApplyCapabilityAccepted(String method, boolean proxyMethodV2Accepted) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "proxyMethodApplyCapabilityAccepted", new String[]{"String", "boolean"}, new Object[]{method, proxyMethodV2Accepted}); }
    public static void addCapabilities(JsonObject body) { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "addCapabilities", new String[]{"JsonObject"}, new Object[]{body}); }
    public static void addCapabilities(JsonObject body, boolean rewardFilesSupported, boolean deploymentReady) { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "addCapabilities", new String[]{"JsonObject", "boolean", "boolean"}, new Object[]{body, rewardFilesSupported, deploymentReady}); }
    public static boolean negotiatedCapability(JsonObject node, String capability, boolean current) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "negotiatedCapability", new String[]{"JsonObject", "String", "boolean"}, new Object[]{node, capability, current}); }
    public static long inspectionRetryDelayMillis(int failures) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "inspectionRetryDelayMillis", new String[]{"int"}, new Object[]{failures}); }
    public static void requireFileCapability(boolean accepted) { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "requireFileCapability", new String[]{"boolean"}, new Object[]{accepted}); }
    public static Map<String, String> resultQuickOptions(String preset, Map<String, String> options) { return (Map<String, String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "resultQuickOptions", new String[]{"String", "Map"}, new Object[]{preset, options}); }
    public static Map<String, String> resultQuickReadOptions(String preset, Map<String, String> state, Map<String, String> requestOptions) { return (Map<String, String>) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "resultQuickReadOptions", new String[]{"String", "Map", "Map"}, new Object[]{preset, state, requestOptions}); }
    public static boolean awaitInspectionShutdown(ScheduledExecutorService executor) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "awaitInspectionShutdown", new String[]{"ScheduledExecutorService"}, new Object[]{executor}); }
    public static void awaitShutdown(ScheduledExecutorService executor, CompletableFuture<Void> operation) { ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector", null, "awaitShutdown", new String[]{"ScheduledExecutorService", "CompletableFuture"}, new Object[]{executor, operation}); }
    public static final class Response implements ReflectiveAccess.View {
        public Response() { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlConnector$Response", new String[]{}, new Object[]{})); }
        private final Object delegate;
        private Response(Object delegate) { this.delegate = delegate; }
        public static Response view(Object delegate) { return delegate == null ? null : new Response(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Response value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public Response(int status, String body) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.control.BackendControlConnector$Response", new String[]{"int", "String"}, new Object[]{status, body})); }
        public int status() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$Response", delegate, "status", new String[]{}, new Object[]{}); }
        public String body() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$Response", delegate, "body", new String[]{}, new Object[]{}); }
    }

    @FunctionalInterface
    public interface ResultSubmission {
        void submit() throws Exception;
    }

    @FunctionalInterface
    public interface ResultAcknowledgement {
        void acknowledge() throws Exception;
    }

    public static final class InspectionTaskResult implements ReflectiveAccess.View {

        private final Object delegate;
        private InspectionTaskResult(Object delegate) { this.delegate = delegate; }
        public static InspectionTaskResult view(Object delegate) { return delegate == null ? null : new InspectionTaskResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof InspectionTaskResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public boolean success() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$InspectionTaskResult", delegate, "success", new String[]{}, new Object[]{}); }
        public String code() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$InspectionTaskResult", delegate, "code", new String[]{}, new Object[]{}); }
        public String message() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$InspectionTaskResult", delegate, "message", new String[]{}, new Object[]{}); }
        public JsonObject data() { return (JsonObject) ReflectiveAccess.call("com.bencodez.votingplugin.control.BackendControlConnector$InspectionTaskResult", delegate, "data", new String[]{}, new Object[]{}); }
    }

}
