package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import java.sql.SQLException;
import java.util.List;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;

/** Test-only forwarding access to the non-public production SharedPointTransferJournal. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedPointTransferJournalAccess implements ReflectiveAccess.View {
    public static final long RESERVED_RECOVERY_AGE_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.user.SharedPointTransferJournal", "RESERVED_RECOVERY_AGE_MILLIS");
    public static final long TERMINAL_RETENTION_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.user.SharedPointTransferJournal", "TERMINAL_RETENTION_MILLIS");
    private final Object delegate;
    private SharedPointTransferJournalAccess(Object delegate) { this.delegate = delegate; }
    public static SharedPointTransferJournalAccess view(Object delegate) { return delegate == null ? null : new SharedPointTransferJournalAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedPointTransferJournalAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedPointTransferJournalAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.user.SharedPointTransferJournal"))); }
    public SharedPointTransferJournalAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedPointTransferJournalAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedPointTransferJournalAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedPointTransferJournalAccess(MySQL table) throws SQLException { this(ReflectiveAccess.construct("com.bencodez.votingplugin.user.SharedPointTransferJournal", new String[]{"MySQL"}, new Object[]{table})); }
    public static String journalTableName(String sourceTable) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", null, "journalTableName", new String[]{"String"}, new Object[]{sourceTable}); }
    public static SharedPointTransferJournalAccess forTable(MySQL table) throws SQLException { return SharedPointTransferJournalAccess.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", null, "forTable", new String[]{"MySQL"}, new Object[]{table})); }
    public boolean reserve(String transferId, String sourceUuid, String sourcePointsColumn, int debitPoints, String targetUuid, int requestedCreditPoints, long now) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "reserve", new String[]{"String", "String", "String", "int", "String", "int", "long"}, new Object[]{transferId, sourceUuid, sourcePointsColumn, debitPoints, targetUuid, requestedCreditPoints, now}); }
    public ClaimOutcome claimHookWithConfirmation(String transferId, String owner, long startedAt) { return ReflectiveAccess.enumView(SharedPointTransferJournalAccess.ClaimOutcome.class, ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "claimHookWithConfirmation", new String[]{"String", "String", "long"}, new Object[]{transferId, owner, startedAt})); }
    public boolean markCompensating(String transferId) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "markCompensating", new String[]{"String"}, new Object[]{transferId}); }
    public boolean refundHookStarted(String transferId, String sourceUuid, String sourcePointsColumn, int debitPoints) throws SQLException { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "refundHookStarted", new String[]{"String", "String", "String", "int"}, new Object[]{transferId, sourceUuid, sourcePointsColumn, debitPoints}); }
    public SettlementOutcome settleWithConfirmation(String transferId, String owner, String sourceUuid, String sourcePointsColumn, String targetUuid, String targetPointsColumn, int debitPoints, Integer adjustedCreditPoints) { return ReflectiveAccess.enumView(SharedPointTransferJournalAccess.SettlementOutcome.class, ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "settleWithConfirmation", new String[]{"String", "String", "String", "String", "String", "String", "int", "Integer"}, new Object[]{transferId, owner, sourceUuid, sourcePointsColumn, targetUuid, targetPointsColumn, debitPoints, adjustedCreditPoints})); }
    public boolean settle(String transferId, String owner, String sourceUuid, String sourcePointsColumn, String targetUuid, String targetPointsColumn, int debitPoints, Integer adjustedCreditPoints) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "settle", new String[]{"String", "String", "String", "String", "String", "String", "int", "Integer"}, new Object[]{transferId, owner, sourceUuid, sourcePointsColumn, targetUuid, targetPointsColumn, debitPoints, adjustedCreditPoints}); }
    public List<RefundedTransfer> recoverAndCleanup(long now) throws SQLException { Object rawResult = ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal", delegate, "recoverAndCleanup", new String[]{"long"}, new Object[]{now}); return rawResult == null ? null : ((java.util.Collection<?>) rawResult).stream().map(value -> SharedPointTransferJournalAccess.RefundedTransfer.view(value)).toList(); }
    public enum ClaimOutcome { CLAIMED, NOT_CLAIMED, INDETERMINATE }

    public enum SettlementOutcome { COMPLETED, REFUNDED, INDETERMINATE }

    public static final class RefundedTransfer implements ReflectiveAccess.View {

        private final Object delegate;
        private RefundedTransfer(Object delegate) { this.delegate = delegate; }
        public static RefundedTransfer view(Object delegate) { return delegate == null ? null : new RefundedTransfer(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof RefundedTransfer value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public String uuid() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal$RefundedTransfer", delegate, "uuid", new String[]{}, new Object[]{}); }
        public String pointsColumn() { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedPointTransferJournal$RefundedTransfer", delegate, "pointsColumn", new String[]{}, new Object[]{}); }
    }

}
