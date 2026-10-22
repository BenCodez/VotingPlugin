package com.bencodez.votingplugin.neoforge;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Lifecycle and presence hooks shared by the native proxy adapters. */
public interface NeoForgeProxyTransport extends AutoCloseable {
    void playerOnline(SharedVoteIdentity identity);
    void playerOffline(SharedVoteIdentity identity);
    @Override void close();
}
