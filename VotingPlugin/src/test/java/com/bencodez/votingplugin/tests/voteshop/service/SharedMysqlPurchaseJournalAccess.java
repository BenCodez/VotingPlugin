package com.bencodez.votingplugin.tests.voteshop.service;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.voteshop.service.*;
import java.sql.SQLException;
import java.util.List;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

/** Test-only forwarding access to the non-public production SharedMysqlPurchaseJournal. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedMysqlPurchaseJournalAccess implements ReflectiveAccess.View {
    public static final String NO_LIMIT_RESET_GENERATION = (String) ReflectiveAccess.field("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", "NO_LIMIT_RESET_GENERATION");
    public static final long PENDING_RECOVERY_AGE_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", "PENDING_RECOVERY_AGE_MILLIS");
    private final Object delegate;
    private SharedMysqlPurchaseJournalAccess(Object delegate) { this.delegate = delegate; }
    public static SharedMysqlPurchaseJournalAccess view(Object delegate) { return delegate == null ? null : new SharedMysqlPurchaseJournalAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedMysqlPurchaseJournalAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedMysqlPurchaseJournalAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal"))); }
    public SharedMysqlPurchaseJournalAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedMysqlPurchaseJournalAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedMysqlPurchaseJournalAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedMysqlPurchaseJournalAccess(MySQL table, boolean initializeSchema) throws SQLException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", new String[]{"MySQL", "boolean"}, new Object[]{table, initializeSchema})); }
    public static String journalTableName(String sourceTable) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", null, "journalTableName", new String[]{"String"}, new Object[]{sourceTable}); }
    public static String epochTableName(String sourceTable) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", null, "epochTableName", new String[]{"String"}, new Object[]{sourceTable}); }

    public boolean reserve(String purchaseId, String uuid, String pointsColumn, String limitColumn, int cost, int limit, String limitGeneration, long limitGenerationExpiresAt, long now) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "reserve", new String[]{"String", "String", "String", "String", "int", "int", "String", "long", "long"}, new Object[]{purchaseId, uuid, pointsColumn, limitColumn, cost, limit, limitGeneration, limitGenerationExpiresAt, now}); }
    public void resetLimit(String limitColumn, String resetGeneration) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "resetLimit", new String[]{"String", "String"}, new Object[]{limitColumn, resetGeneration}); }
    public ClaimOutcome claimReward(String purchaseId, long startedAt) throws SQLException { return ReflectiveAccess.enumView(SharedMysqlPurchaseJournalAccess.ClaimOutcome.class, ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "claimReward", new String[]{"String", "long"}, new Object[]{purchaseId, startedAt})); }
    public boolean complete(String purchaseId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "complete", new String[]{"String"}, new Object[]{purchaseId}); }
    public boolean refundPending(String purchaseId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "refundPending", new String[]{"String"}, new Object[]{purchaseId}); }
    public boolean refundPending(String purchaseId, long now) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "refundPending", new String[]{"String", "long"}, new Object[]{purchaseId, now}); }
    public boolean refundUnstartedReward(String purchaseId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "refundUnstartedReward", new String[]{"String"}, new Object[]{purchaseId}); }
    public boolean markCompensating(String purchaseId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "markCompensating", new String[]{"String"}, new Object[]{purchaseId}); }
    public boolean refundCompensatingReward(String purchaseId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "refundCompensatingReward", new String[]{"String"}, new Object[]{purchaseId}); }
    public List<RefundedPurchase> recoverAndCleanup(long now) throws SQLException { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal", delegate, "recoverAndCleanup", new String[]{"long"}, new Object[]{now}); return rawResult == null ? null : ((java.util.Collection<?>) rawResult).stream().map(value -> SharedMysqlPurchaseJournalAccess.RefundedPurchase.view(value)).toList(); }
    public static final class RefundedPurchase implements ReflectiveAccess.View {

        private final Object delegate;
        private RefundedPurchase(Object delegate) { this.delegate = delegate; }
        public static RefundedPurchase view(Object delegate) { return delegate == null ? null : new RefundedPurchase(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof RefundedPurchase value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String uuid() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal$RefundedPurchase", delegate, "uuid", new String[]{}, new Object[]{}); }
        public String pointsColumn() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal$RefundedPurchase", delegate, "pointsColumn", new String[]{}, new Object[]{}); }
        public String limitColumn() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.voteshop.service.SharedMysqlPurchaseJournal$RefundedPurchase", delegate, "limitColumn", new String[]{}, new Object[]{}); }
    }

    public enum ClaimOutcome { CLAIMED, NOT_CLAIMED, INDETERMINATE }

}
