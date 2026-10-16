package com.bencodez.votingplugin.tests.backendproxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.*;
import com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue.*;
import java.util.function.Consumer;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Typed test-only access to package-private BackendOrderedVoteOverflowQueue seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class BackendOrderedVoteOverflowQueueAccess {
    public static final int MAX_NORMAL_ENTRIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", "MAX_NORMAL_ENTRIES");
    public static final int MAX_FAILED_ENTRIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", "MAX_FAILED_ENTRIES");
    private BackendOrderedVoteOverflowQueueAccess() { }
    public static boolean enqueue(BackendOrderedVoteOverflowQueue target, JsonEnvelope envelope) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "enqueue", new String[]{"JsonEnvelope"}, new Object[]{envelope}); }
    public static boolean enqueue(BackendOrderedVoteOverflowQueue target, JsonEnvelope envelope, int reservedPrefixEntries) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "enqueue", new String[]{"JsonEnvelope", "int"}, new Object[]{envelope, reservedPrefixEntries}); }
    public static int failedSize(BackendOrderedVoteOverflowQueue target) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "failedSize", new String[]{}, new Object[]{}); }
    public static boolean isQuarantineCapacityExhausted(BackendOrderedVoteOverflowQueue target) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "isQuarantineCapacityExhausted", new String[]{}, new Object[]{}); }
    public static boolean quarantineForShutdown(BackendOrderedVoteOverflowQueue target, BackendOrderedVoteOverflowQueueAccess.PendingEnvelope expected, JsonEnvelope envelope) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "quarantineForShutdown", new String[]{"PendingEnvelope", "JsonEnvelope"}, new Object[]{expected, envelope}); }
    public static void quarantineAsync(BackendOrderedVoteOverflowQueue target, BackendOrderedVoteOverflowQueueAccess.PendingEnvelope expected, JsonEnvelope envelope, Consumer<Boolean> completion) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "quarantineAsync", new String[]{"PendingEnvelope", "JsonEnvelope", "Consumer"}, new Object[]{expected, envelope, completion}); }
    public static BackendOrderedVoteOverflowQueueAccess.PendingEnvelope peekDurable(BackendOrderedVoteOverflowQueue target) { return BackendOrderedVoteOverflowQueueAccess.PendingEnvelope.view(ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "peekDurable", new String[]{}, new Object[]{})); }
    public static boolean awaitClose(BackendOrderedVoteOverflowQueue target, long timeoutMillis) throws InterruptedException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "awaitClose", new String[]{"long"}, new Object[]{timeoutMillis}); }
    public static BackendOrderedVoteOverflowQueueAccess.CloseState closeState(BackendOrderedVoteOverflowQueue target) { return ReflectiveAccess.enumView(BackendOrderedVoteOverflowQueueAccess.CloseState.class, ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue", target, "closeState", new String[]{}, new Object[]{})); }
    public enum CloseState { OPEN, CLOSING, WAITING_TO_PERSIST, SNAPSHOT_REPLACED, COMPLETE, FAILED }

    public static final class PendingEnvelope implements ReflectiveAccess.View {
        private final Object delegate;
        private PendingEnvelope(Object delegate) { this.delegate = delegate; }
        public static PendingEnvelope view(Object delegate) { return delegate == null ? null : new PendingEnvelope(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof PendingEnvelope value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }
        public JsonEnvelope envelope() { return (JsonEnvelope) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.BackendOrderedVoteOverflowQueue$PendingEnvelope", delegate, "envelope", new String[]{}, new Object[]{}); }
    }

}
