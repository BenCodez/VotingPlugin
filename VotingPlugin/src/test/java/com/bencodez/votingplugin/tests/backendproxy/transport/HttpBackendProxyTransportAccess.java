package com.bencodez.votingplugin.tests.backendproxy.transport;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.transport.*;
import com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport.*;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Semaphore;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.http.HttpBackendTransportConnector;
import com.bencodez.simpleapi.servercomm.http.HttpConnectionCode;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Typed test-only access to package-private HttpBackendProxyTransport seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class HttpBackendProxyTransportAccess {
    public static void validate(HttpBackendProxyTransport target) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "validate", new String[]{}, new Object[]{}); }
    public static void validate(HttpBackendProxyTransport target, long deadlineNanos) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "validate", new String[]{"long"}, new Object[]{deadlineNanos}); }
    public static void activateRestoredHandoffDrain(HttpBackendProxyTransport target, int managerMessages) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "activateRestoredHandoffDrain", new String[]{"int"}, new Object[]{managerMessages}); }
    public static void appendShutdownHandoffMessages(HttpBackendProxyTransport target, java.util.List<JsonEnvelope> messages) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "appendShutdownHandoffMessages", new String[]{"List"}, new Object[]{messages}); }
    public static void acceptHandoffMessages(HttpBackendProxyTransport target, java.util.List<JsonEnvelope> messages) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "acceptHandoffMessages", new String[]{"List"}, new Object[]{messages}); }
    public static void beginPreparedHandoff(HttpBackendProxyTransport target) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "beginPreparedHandoff", new String[]{}, new Object[]{}); }
    public static void flushForReplacement(HttpBackendProxyTransport target, HttpBackendTransportConnector connector, long deadlineNanos) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "flushForReplacement", new String[]{"HttpBackendTransportConnector", "long"}, new Object[]{connector, deadlineNanos}); }
    public static java.util.List<JsonEnvelope> drainPreparedMessages(HttpBackendProxyTransport target) { return (java.util.List<JsonEnvelope>) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "drainPreparedMessages", new String[]{}, new Object[]{}); }
    public static boolean enrollForStartup(HttpBackendProxyTransport target, HttpConnectionCode code, String serverId, Path directory, boolean retryEnrollment, java.util.function.LongPredicate waitForRetry) throws Exception { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "enrollForStartup", new String[]{"HttpConnectionCode", "String", "Path", "boolean", "LongPredicate"}, new Object[]{code, serverId, directory, retryEnrollment, waitForRetry}); }
    public static void dispatchAfterPublication(HttpBackendProxyTransport target, GlobalMessageHandler messageHandler, JsonEnvelope envelope) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "dispatchAfterPublication", new String[]{"GlobalMessageHandler", "JsonEnvelope"}, new Object[]{messageHandler, envelope}); }
    private HttpBackendProxyTransportAccess() { }
    public static HttpBackendProxyTransport create(VotingPluginMain plugin) { return (HttpBackendProxyTransport) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static HttpBackendProxyTransport create(VotingPluginMain plugin, TransportEnvelopeEncryption encryption) { return (HttpBackendProxyTransport) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", new String[]{"VotingPluginMain", "TransportEnvelopeEncryption"}, new Object[]{plugin, encryption}); }
    public static HttpBackendProxyTransport recreatePrepared(HttpBackendProxyTransport target) { return (HttpBackendProxyTransport) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "recreatePrepared", new String[]{}, new Object[]{}); }
    public static boolean isClosedForReplacement(HttpBackendProxyTransport target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "isClosedForReplacement", new String[]{}, new Object[]{}); }

    public static int preparedMessageCount(HttpBackendProxyTransport target) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "preparedMessageCount", new String[]{}, new Object[]{}); }
    public static java.util.List<JsonEnvelope> preparedMessagesSnapshot(HttpBackendProxyTransport target) { return (java.util.List<JsonEnvelope>) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "preparedMessagesSnapshot", new String[]{}, new Object[]{}); }

    public static void finishInitialization(boolean installed, HttpBackendTransportConnector replacement, boolean acquired, Semaphore owner, CountDownLatch completion) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", null, "finishInitialization", new String[]{"boolean", "HttpBackendTransportConnector", "boolean", "Semaphore", "CountDownLatch"}, new Object[]{installed, replacement, acquired, owner, completion}); }
    public static void transferStartupQueue(ArrayDeque<JsonEnvelope> queue, HttpBackendTransportConnector connector) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", null, "transferStartupQueue", new String[]{"ArrayDeque", "HttpBackendTransportConnector"}, new Object[]{queue, connector}); }

    public static void restoreUnenrolledCredentialState(Path directory, String serverId, String configuredCode) throws Exception { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", null, "restoreUnenrolledCredentialState", new String[]{"Path", "String", "String"}, new Object[]{directory, serverId, configuredCode}); }

    public static void dispatchIncoming(HttpBackendProxyTransport target, GlobalMessageHandler messageHandler, JsonEnvelope envelope, long deadlineNanos) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "dispatchIncoming", new String[]{"GlobalMessageHandler", "JsonEnvelope", "long"}, new Object[]{messageHandler, envelope, deadlineNanos}); }




    public static java.util.List<JsonEnvelope> handoffMessagesSnapshot(HttpBackendProxyTransport target) { return (java.util.List<JsonEnvelope>) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", target, "handoffMessagesSnapshot", new String[]{}, new Object[]{}); }


    public static HttpConnectionCode enrollmentCode(Path directory, String serverId, String configuredCode) { return (HttpConnectionCode) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", null, "enrollmentCode", new String[]{"Path", "String", "String"}, new Object[]{directory, serverId, configuredCode}); }
    public static boolean flushHandoffForShutdown(HttpBackendTransportConnector active, java.util.List<JsonEnvelope> finalHandoff, long deadlineNanos) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.HttpBackendProxyTransport", null, "flushHandoffForShutdown", new String[]{"HttpBackendTransportConnector", "List", "long"}, new Object[]{active, finalHandoff, deadlineNanos}); }
}
