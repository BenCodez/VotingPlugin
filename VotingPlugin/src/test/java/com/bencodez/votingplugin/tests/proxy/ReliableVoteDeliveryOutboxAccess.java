package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;

/** Test-only forwarding access to the non-public production ReliableVoteDeliveryOutbox. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ReliableVoteDeliveryOutboxAccess implements ReflectiveAccess.View {
    public static final int MAX_UPDATE_ENTRIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", "MAX_UPDATE_ENTRIES");
    public static final int MAX_DEFERRED_UPDATE_ENTRIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", "MAX_DEFERRED_UPDATE_ENTRIES");
    private final Object delegate;
    private ReliableVoteDeliveryOutboxAccess(Object delegate) { this.delegate = delegate; }
    public static ReliableVoteDeliveryOutboxAccess view(Object delegate) { return delegate == null ? null : new ReliableVoteDeliveryOutboxAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof ReliableVoteDeliveryOutboxAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static ReliableVoteDeliveryOutboxAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox"))); }
    public ReliableVoteDeliveryOutboxAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public ReliableVoteDeliveryOutboxAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public ReliableVoteDeliveryOutboxAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public ReliableVoteDeliveryOutboxAccess(Path file) throws IOException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", new String[]{"Path"}, new Object[]{file})); }
    public boolean offer(String server, JsonEnvelope envelope) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "offer", new String[]{"String", "JsonEnvelope"}, new Object[]{server, envelope}); }
    public boolean retainUpdate(String server, JsonEnvelope envelope, boolean carrierAvailable) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "retainUpdate", new String[]{"String", "JsonEnvelope", "boolean"}, new Object[]{server, envelope, carrierAvailable}); }
    public boolean setUpdateReady(String server, UUID voteId, boolean ready) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "setUpdateReady", new String[]{"String", "UUID", "boolean"}, new Object[]{server, voteId, ready}); }
    public boolean isFirstPendingUpdate(String server, UUID voteId) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "isFirstPendingUpdate", new String[]{"String", "UUID"}, new Object[]{server, voteId}); }
    public boolean acknowledgeCompletion(String server, UUID voteId, String subChannel) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "acknowledgeCompletion", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); }
    public boolean beginLegacyDelivery(String server, UUID voteId, String subChannel) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "beginLegacyDelivery", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); }
    public boolean rejectLegacyDelivery(String server, UUID voteId, String subChannel) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "rejectLegacyDelivery", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); }
    public boolean acknowledgeReceiptRelease(String server, UUID voteId, String subChannel) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "acknowledgeReceiptRelease", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); }
    public boolean retireLegacyDelivery(String server, UUID voteId, String subChannel) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "retireLegacyDelivery", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); }
    public List<Entry> snapshot() { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "snapshot", new String[]{}, new Object[]{}); return rawResult == null ? null : ((java.util.Collection<?>) rawResult).stream().map(value -> ReliableVoteDeliveryOutboxAccess.Entry.view(value)).toList(); }
    public Optional<Entry> find(String server, UUID voteId, String subChannel) { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "find", new String[]{"String", "UUID", "String"}, new Object[]{server, voteId, subChannel}); return rawResult == null ? null : ((java.util.Optional<?>) rawResult).map(value -> ReliableVoteDeliveryOutboxAccess.Entry.view(value)); }
    public int size() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox", delegate, "size", new String[]{}, new Object[]{}); }
    public static final class Entry implements ReflectiveAccess.View {
        private final Object delegate;
        private Entry(Object delegate) { this.delegate = delegate; }
        public static Entry view(Object delegate) { return delegate == null ? null : new Entry(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof Entry value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String server() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "server", new String[]{}, new Object[]{}); }
        public JsonEnvelope envelope() { return (JsonEnvelope) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "envelope", new String[]{}, new Object[]{}); }
        public boolean awaitingReceiptRelease() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "awaitingReceiptRelease", new String[]{}, new Object[]{}); }
        public boolean legacyDeliveryFenced() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "legacyDeliveryFenced", new String[]{}, new Object[]{}); }
        public boolean legacyDeliveryRejected() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "legacyDeliveryRejected", new String[]{}, new Object[]{}); }
        public boolean deferredUpdate() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.ReliableVoteDeliveryOutbox$Entry", delegate, "deferredUpdate", new String[]{}, new Object[]{}); }

    }

}
