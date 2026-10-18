package com.bencodez.votingplugin.tests.user;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.user.*;
import java.sql.SQLException;
import java.util.concurrent.CompletionStage;
import java.util.function.Consumer;
import java.util.function.IntFunction;
import com.bencodez.votingplugin.VotingPluginMain;

/** Test-only forwarding access to the non-public production SharedMysqlPointMutator. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class SharedMysqlPointMutatorAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private SharedMysqlPointMutatorAccess(Object delegate) { this.delegate = delegate; }
    public static SharedMysqlPointMutatorAccess view(Object delegate) { return delegate == null ? null : new SharedMysqlPointMutatorAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof SharedMysqlPointMutatorAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static SharedMysqlPointMutatorAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.user.SharedMysqlPointMutator"))); }
    public SharedMysqlPointMutatorAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public SharedMysqlPointMutatorAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public SharedMysqlPointMutatorAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public SharedMysqlPointMutatorAccess(VotingPluginMain plugin) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.user.SharedMysqlPointMutator", new String[]{"VotingPluginMain"}, new Object[]{plugin})); }
    public boolean applies() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "applies", new String[]{}, new Object[]{}); }
    public static boolean usesSharedMysqlPoints(VotingPluginMain plugin) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", null, "usesSharedMysqlPoints", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static boolean canRecoverSharedMysqlPointJournals(VotingPluginMain plugin) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", null, "canRecoverSharedMysqlPointJournals", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static void scheduleTransferRecovery(VotingPluginMain plugin) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", null, "scheduleTransferRecovery", new String[]{"VotingPluginMain"}, new Object[]{plugin}); }
    public static void recoverTransfers(VotingPluginMain plugin, SharedPointTransferJournalAccess journal) throws SQLException { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", null, "recoverTransfers", new String[]{"VotingPluginMain", "SharedPointTransferJournal"}, new Object[]{plugin, journal}); }
    public int add(VotingPluginUser user, int amount, boolean async) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "add", new String[]{"VotingPluginUser", "int", "boolean"}, new Object[]{user, amount, async}); }
    public int add(VotingPluginUser user, int amount, boolean async, boolean failOnRejectedAdmission) { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "add", new String[]{"VotingPluginUser", "int", "boolean", "boolean"}, new Object[]{user, amount, async, failOnRejectedAdmission}); }
    public AddResult addCommitted(VotingPluginUser user, int amount) { return SharedMysqlPointMutatorAccess.AddResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "addCommitted", new String[]{"VotingPluginUser", "int"}, new Object[]{user, amount})); }
    public AddResult addCommitted(VotingPluginUser user, int amount, String operationId) { return SharedMysqlPointMutatorAccess.AddResult.view(ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "addCommitted", new String[]{"VotingPluginUser", "int", "String"}, new Object[]{user, amount, operationId})); }
    public CompletionStage<Void> acknowledgePointAddition(String operationId) { return (CompletionStage<Void>) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "acknowledgePointAddition", new String[]{"String"}, new Object[]{operationId}); }
    public boolean set(VotingPluginUser user, int value, boolean async) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "set", new String[]{"VotingPluginUser", "int", "boolean"}, new Object[]{user, value, async}); }
    public boolean setCommitted(VotingPluginUser user, int value) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "setCommitted", new String[]{"VotingPluginUser", "int"}, new Object[]{user, value}); }
    public void cap(VotingPluginUser user, int maximum, boolean async) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "cap", new String[]{"VotingPluginUser", "int", "boolean"}, new Object[]{user, maximum, async}); }
    public void addAndCap(VotingPluginUser user, int amount, int maximum, boolean async) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "addAndCap", new String[]{"VotingPluginUser", "int", "int", "boolean"}, new Object[]{user, amount, maximum, async}); }
    public boolean remove(VotingPluginUser user, int amount) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "remove", new String[]{"VotingPluginUser", "int"}, new Object[]{user, amount}); }
    public boolean remove(VotingPluginUser user, int amount, boolean async) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "remove", new String[]{"VotingPluginUser", "int", "boolean"}, new Object[]{user, amount, async}); }
    public boolean transfer(VotingPluginUser source, VotingPluginUser target, int amount) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "transfer", new String[]{"VotingPluginUser", "VotingPluginUser", "int"}, new Object[]{source, target, amount}); }
    public boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount, int creditAmount) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "transfer", new String[]{"VotingPluginUser", "VotingPluginUser", "int", "int"}, new Object[]{source, target, debitAmount, creditAmount}); }
    public boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount, IntFunction<Integer> creditAmountProvider) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "transfer", new String[]{"VotingPluginUser", "VotingPluginUser", "int", "IntFunction"}, new Object[]{source, target, debitAmount, creditAmountProvider}); }
    public void transferWithBukkitApproval(VotingPluginUser source, VotingPluginUser target, int debitAmount, IntFunction<Integer> creditAmountProvider, Consumer<PointTransferResult> completion) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "transferWithBukkitApproval", new String[]{"VotingPluginUser", "VotingPluginUser", "int", "IntFunction", "Consumer"}, new Object[]{source, target, debitAmount, creditAmountProvider, completion}); }
    public void refundClaimedAfterSchedulingFailure(VotingPluginUser source, org.bukkit.entity.Player sourcePlayer, Consumer<PointTransferResult> completion, SharedPointTransferJournalAccess journal, String transferId, String sourcePoints, int debitAmount, RuntimeException failure) { ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator", delegate, "refundClaimedAfterSchedulingFailure", new String[]{"VotingPluginUser", "Player", "Consumer", "SharedPointTransferJournal", "String", "String", "int", "RuntimeException"}, new Object[]{source, sourcePlayer, completion, journal, transferId, sourcePoints, debitAmount, failure}); }
    public enum MutationOutcome { CONFIRMED, REJECTED, INDETERMINATE }

    public static final class AddResult implements ReflectiveAccess.View {
        private final Object delegate;
        private AddResult(Object delegate) { this.delegate = delegate; }
        public static AddResult view(Object delegate) { return delegate == null ? null : new AddResult(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof AddResult value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public MutationOutcome outcome() { return ReflectiveAccess.enumView(SharedMysqlPointMutatorAccess.MutationOutcome.class, ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator$AddResult", delegate, "outcome", new String[]{}, new Object[]{})); }
        public int total() { return (Integer) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator$AddResult", delegate, "total", new String[]{}, new Object[]{}); }

        public boolean success() { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.user.SharedMysqlPointMutator$AddResult", delegate, "success", new String[]{}, new Object[]{}); }
    }

}
