package com.bencodez.votingplugin.tests.backendproxy.cache;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.cache.*;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache.*;
import java.nio.file.Path;

/** Typed test-only access to package-private ProcessedVoteCache seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class ProcessedVoteCacheAccess {
    public static final int DEFAULT_MAX_TRACKED_VOTES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", "DEFAULT_MAX_TRACKED_VOTES");
    private ProcessedVoteCacheAccess() { }
    public static ProcessedVoteCache create() { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{}, new Object[]{}); }
    public static ProcessedVoteCache create(long ttlMillis) { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{"long"}, new Object[]{ttlMillis}); }
    public static ProcessedVoteCache create(Path receiptFile) { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{"Path"}, new Object[]{receiptFile}); }
    public static ProcessedVoteCache create(long ttlMillis, Path receiptFile) { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{"long", "Path"}, new Object[]{ttlMillis, receiptFile}); }
    public static ProcessedVoteCache create(long ttlMillis, int maxTrackedVotes) { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{"long", "int"}, new Object[]{ttlMillis, maxTrackedVotes}); }
    public static ProcessedVoteCache create(long ttlMillis, int maxTrackedVotes, DurableVoteReceiptStoreAccess durableReceipts) { return (ProcessedVoteCache) ReflectiveAccess.construct("com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache", new String[]{"long", "int", "DurableVoteReceiptStore"}, new Object[]{ttlMillis, maxTrackedVotes, durableReceipts}); }
}
