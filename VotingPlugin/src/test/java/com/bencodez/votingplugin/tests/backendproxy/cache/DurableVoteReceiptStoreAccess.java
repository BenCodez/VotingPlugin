package com.bencodez.votingplugin.tests.backendproxy.cache;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.cache.*;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import java.util.UUID;

/** Test-only forwarding access to the non-public production DurableVoteReceiptStore. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class DurableVoteReceiptStoreAccess implements ReflectiveAccess.View {
    public static final int MAX_ACTIVE_RECEIPTS = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "MAX_ACTIVE_RECEIPTS");
    public static final int COMPLETION_HEADROOM = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "COMPLETION_HEADROOM");
    public static final int MAX_RELEASE_TOMBSTONES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "MAX_RELEASE_TOMBSTONES");
    public static final long MAX_FILE_BYTES = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "MAX_FILE_BYTES");
    public static final int MAX_RECORD_BYTES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "MAX_RECORD_BYTES");
    public static final int MAX_TOTAL_RECEIPTS = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", "MAX_TOTAL_RECEIPTS");
    private final Object delegate;
    private DurableVoteReceiptStoreAccess(Object delegate) { this.delegate = delegate; }
    public static DurableVoteReceiptStoreAccess view(Object delegate) { return delegate == null ? null : new DurableVoteReceiptStoreAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof DurableVoteReceiptStoreAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static DurableVoteReceiptStoreAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore"))); }
    public DurableVoteReceiptStoreAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public DurableVoteReceiptStoreAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public DurableVoteReceiptStoreAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public DurableVoteReceiptStoreAccess(Path file) throws IOException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", new String[]{"Path"}, new Object[]{file})); }
    public DurableVoteReceiptStoreAccess(Path file, int maxActiveReceipts, int completionHeadroom, int maxReleaseTombstones) throws IOException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", new String[]{"Path", "int", "int", "int"}, new Object[]{file, maxActiveReceipts, completionHeadroom, maxReleaseTombstones})); }
    public DurableVoteReceiptStoreAccess(Path file, int maxActiveReceipts, int completionHeadroom, int maxReleaseTombstones, int maxTotalReceipts) throws IOException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", new String[]{"Path", "int", "int", "int", "int"}, new Object[]{file, maxActiveReceipts, completionHeadroom, maxReleaseTombstones, maxTotalReceipts})); }
    public Map<UUID, Long> snapshot() { return (Map<UUID, Long>) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", delegate, "snapshot", new String[]{}, new Object[]{}); }
    public boolean contains(UUID voteId) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", delegate, "contains", new String[]{"UUID"}, new Object[]{voteId}); }
    public long complete(UUID voteId) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", delegate, "complete", new String[]{"UUID"}, new Object[]{voteId}); }
    public long release(UUID voteId) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.cache.DurableVoteReceiptStore", delegate, "release", new String[]{"UUID"}, new Object[]{voteId}); }
}
