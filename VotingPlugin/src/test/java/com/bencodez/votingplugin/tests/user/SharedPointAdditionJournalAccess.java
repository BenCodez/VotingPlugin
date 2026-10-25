package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import java.sql.SQLException;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

/** Test-only forwarding access to the non-public production SharedPointAdditionJournal. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedPointAdditionJournalAccess implements ReflectiveAccess.View {
    public static final long COMPLETED_RETENTION_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.user.SharedPointAdditionJournal", "COMPLETED_RETENTION_MILLIS");
    public static final long HOOK_RECOVERY_LEASE_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.user.SharedPointAdditionJournal", "HOOK_RECOVERY_LEASE_MILLIS");
    private final Object delegate;
    private SharedPointAdditionJournalAccess(Object delegate) { this.delegate = delegate; }
    public static SharedPointAdditionJournalAccess view(Object delegate) { return delegate == null ? null : new SharedPointAdditionJournalAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedPointAdditionJournalAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedPointAdditionJournalAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.user.SharedPointAdditionJournal"))); }
    public SharedPointAdditionJournalAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedPointAdditionJournalAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedPointAdditionJournalAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedPointAdditionJournalAccess(MySQL table, boolean initializeSchema) throws SQLException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.user.SharedPointAdditionJournal", new String[]{"MySQL", "boolean"}, new Object[]{table, initializeSchema})); }
    public static String journalTableName(String sourceTable) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", null, "journalTableName", new String[]{"String"}, new Object[]{sourceTable}); }

    public AdditionResult add(String operationId, String uuid, String pointsColumn, int amount, long now) throws SQLException { return SharedPointAdditionJournalAccess.AdditionResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "add", new String[]{"String", "String", "String", "int", "long"}, new Object[]{operationId, uuid, pointsColumn, amount, now})); }
    public AdditionResult subtract(String operationId, String uuid, String pointsColumn, int amount, long now) throws SQLException { return SharedPointAdditionJournalAccess.AdditionResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "subtract", new String[]{"String", "String", "String", "int", "long"}, new Object[]{operationId, uuid, pointsColumn, amount, now})); }
    public HookClaim claimHook(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner, long now) throws SQLException { return SharedPointAdditionJournalAccess.HookClaim.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "claimHook", new String[]{"String", "String", "String", "int", "String", "long"}, new Object[]{operationId, uuid, pointsColumn, requestedAmount, owner, now})); }
    public AdditionResult settleClaim(String operationId, String uuid, String journalPointsColumn, String creditPointsColumn, int requestedAmount, String owner, Integer adjustedAmount) throws SQLException { return SharedPointAdditionJournalAccess.AdditionResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "settleClaim", new String[]{"String", "String", "String", "String", "int", "String", "Integer"}, new Object[]{operationId, uuid, journalPointsColumn, creditPointsColumn, requestedAmount, owner, adjustedAmount})); }
    public void markIndeterminate(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "markIndeterminate", new String[]{"String", "String", "String", "int", "String"}, new Object[]{operationId, uuid, pointsColumn, requestedAmount, owner}); }
    public void releaseUnstartedHook(String operationId, String uuid, String pointsColumn, int requestedAmount, String owner) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "releaseUnstartedHook", new String[]{"String", "String", "String", "int", "String"}, new Object[]{operationId, uuid, pointsColumn, requestedAmount, owner}); }
    public AdditionResult findCompleted(String operationId, String uuid, String pointsColumn) throws SQLException { return SharedPointAdditionJournalAccess.AdditionResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "findCompleted", new String[]{"String", "String", "String"}, new Object[]{operationId, uuid, pointsColumn})); }
    public void acknowledge(String operationId, long now) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "acknowledge", new String[]{"String", "long"}, new Object[]{operationId, now}); }
    public void cleanupAcknowledged(long now) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal", delegate, "cleanupAcknowledged", new String[]{"long"}, new Object[]{now}); }
    public static final class AdditionResult implements ReflectiveAccess.View {

        private final Object delegate;
        private AdditionResult(Object delegate) { this.delegate = delegate; }
        public static AdditionResult view(Object delegate) { return delegate == null ? null : new AdditionResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof AdditionResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public int total() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$AdditionResult", delegate, "total", new String[]{}, new Object[]{}); }
    }

    public static final class HookClaim implements ReflectiveAccess.View {

        private final Object delegate;
        private HookClaim(Object delegate) { this.delegate = delegate; }
        public static HookClaim view(Object delegate) { return delegate == null ? null : new HookClaim(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof HookClaim value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public boolean claimed() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$HookClaim", delegate, "claimed", new String[]{}, new Object[]{}); }
        public boolean completed() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$HookClaim", delegate, "completed", new String[]{}, new Object[]{}); }
        public boolean requiresReconciliation() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$HookClaim", delegate, "requiresReconciliation", new String[]{}, new Object[]{}); }
        public int total() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$HookClaim", delegate, "total", new String[]{}, new Object[]{}); }
        public static HookClaim completed(int total) { return SharedPointAdditionJournalAccess.HookClaim.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointAdditionJournal$HookClaim", null, "completed", new String[]{"int"}, new Object[]{total})); }
    }

}
