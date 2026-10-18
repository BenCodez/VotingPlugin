package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore.*;
import java.util.List;
import java.util.UUID;
import com.bencodez.advancedcore.core.user.storage.sql.SqlUserBackend;
import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;

/** Typed test-only access to package-private NeoForgeDeferredVoteStore seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeDeferredVoteStoreAccess {
    public static FenceResult fenceExternalEffects(NeoForgeDeferredVoteStore.Claim target) {
        return ReflectiveAccess.enumView(FenceResult.class, ReflectiveAccess.call(
                "com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore$Claim", target,
                "fenceExternalEffects", new String[]{}, new Object[]{}));
    }

    public static final String DEFERRED_VOTES = (String) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", "DEFERRED_VOTES");
    public static final String COMPLETED_DEFERRED_VOTES = (String) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", "COMPLETED_DEFERRED_VOTES");
    public static final int MAX_DEFERRED_PER_USER = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", "MAX_DEFERRED_PER_USER");
    private NeoForgeDeferredVoteStoreAccess() { }
    public static NeoForgeDeferredVoteStore create(SqlUserBackend backend) { return (NeoForgeDeferredVoteStore) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", new String[]{"SqlUserBackend"}, new Object[]{backend}); }
    public static NeoForgeDeferredVoteStore create(SqlUserBackend backend, int perUserLimit, int totalLimit) { return (NeoForgeDeferredVoteStore) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", new String[]{"SqlUserBackend", "int", "int"}, new Object[]{backend, perUserLimit, totalLimit}); }
    public static NeoForgeDeferredVoteStore create(SqlUserBackend backend, int perUserLimit, int totalLimit, int completedPerUserLimit, int completedTotalLimit) { return (NeoForgeDeferredVoteStore) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", new String[]{"SqlUserBackend", "int", "int", "int", "int"}, new Object[]{backend, perUserLimit, totalLimit, completedPerUserLimit, completedTotalLimit}); }
    public static NeoForgeDeferredVoteStore create(SqlUserBackend backend, int perUserLimit, int totalLimit, int completedPerUserLimit, int completedTotalLimit, int releasedPerUserLimit) { return (NeoForgeDeferredVoteStore) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", new String[]{"SqlUserBackend", "int", "int", "int", "int", "int"}, new Object[]{backend, perUserLimit, totalLimit, completedPerUserLimit, completedTotalLimit, releasedPerUserLimit}); }
    public static NeoForgeDeferredVoteStore create(SqlUserBackend backend, int perUserLimit, int totalLimit, int completedPerUserLimit, int completedTotalLimit, int releasedPerUserLimit, int releasedTotalLimit) { return (NeoForgeDeferredVoteStore) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", new String[]{"SqlUserBackend", "int", "int", "int", "int", "int", "int"}, new Object[]{backend, perUserLimit, totalLimit, completedPerUserLimit, completedTotalLimit, releasedPerUserLimit, releasedTotalLimit}); }
    public static NeoForgeDeferredVoteStoreAccess.DeferralResult defer(NeoForgeDeferredVoteStore target, SharedVoteIdentity identity, SharedVoteInput input, NeoForgeVoteSite site, NeoForgeVoteAccountingDecision accountingDecision) { return NeoForgeDeferredVoteStoreAccess.DeferralResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", target, "defer", new String[]{"SharedVoteIdentity", "SharedVoteInput", "NeoForgeVoteSite", "NeoForgeVoteAccountingDecision"}, new Object[]{identity, input, site, accountingDecision})); }
    public static NeoForgeDeferredVoteStoreAccess.ClaimAttempt claimForReplay(NeoForgeDeferredVoteStore target, UUID playerId, UUID voteId) { return NeoForgeDeferredVoteStoreAccess.ClaimAttempt.view(ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", target, "claimForReplay", new String[]{"UUID", "UUID"}, new Object[]{playerId, voteId})); }
    public static NeoForgeDeferredVoteStoreAccess.QuarantineResult quarantine(NeoForgeDeferredVoteStore target, UUID playerId, UUID voteId) { return ReflectiveAccess.enumView(NeoForgeDeferredVoteStoreAccess.QuarantineResult.class, ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", target, "quarantine", new String[]{"UUID", "UUID"}, new Object[]{playerId, voteId})); }
    public static NeoForgeDeferredVoteStoreAccess.ReleaseResult release(NeoForgeDeferredVoteStore target, UUID playerId, UUID voteId) { return ReflectiveAccess.enumView(NeoForgeDeferredVoteStoreAccess.ReleaseResult.class, ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore", target, "release", new String[]{"UUID", "UUID"}, new Object[]{playerId, voteId})); }
    public enum QuarantineResult { QUARANTINED, ALREADY_QUARANTINED, ALREADY_COMPLETED, NOT_PENDING }

    public enum FenceResult { FENCED, ALREADY_FENCED, REPLAYABLE, ALREADY_REPLAYABLE, ALREADY_COMPLETED, NOT_PENDING }

    public enum Status { RETAINED, ALREADY_RETAINED, ALREADY_COMPLETED, CAPACITY_REACHED }

    public enum ReleaseResult { RELEASED, ALREADY_RELEASED, NOT_COMPLETED, RELEASE_CAPACITY_REACHED }

    public enum ClaimStatus { CLAIMED, ACTIVE, ALREADY_COMPLETED, NOT_PENDING, QUARANTINED, RECEIPT_CAPACITY_REACHED }

    public static final class ClaimAttempt implements ReflectiveAccess.View {

        private final Object delegate;
        private ClaimAttempt(Object delegate) { this.delegate = delegate; }
        public static ClaimAttempt view(Object delegate) { return delegate == null ? null : new ClaimAttempt(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof ClaimAttempt value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public NeoForgeDeferredVoteStoreAccess.ClaimStatus status() { return ReflectiveAccess.enumView(NeoForgeDeferredVoteStoreAccess.ClaimStatus.class, ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore$ClaimAttempt", delegate, "status", new String[]{}, new Object[]{})); }
        public Claim claim() { return (Claim) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore$ClaimAttempt", delegate, "claim", new String[]{}, new Object[]{}); }
    }

    public static final class DeferralResult implements ReflectiveAccess.View {

        private final Object delegate;
        private DeferralResult(Object delegate) { this.delegate = delegate; }
        public static DeferralResult view(Object delegate) { return delegate == null ? null : new DeferralResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof DeferralResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public NeoForgeDeferredVoteStoreAccess.Status status() { return ReflectiveAccess.enumView(NeoForgeDeferredVoteStoreAccess.Status.class, ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore$DeferralResult", delegate, "status", new String[]{}, new Object[]{})); }
        public List<NeoForgeDeferredVote> pending() { return (List<NeoForgeDeferredVote>) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeDeferredVoteStore$DeferralResult", delegate, "pending", new String[]{}, new Object[]{}); }
    }

}
