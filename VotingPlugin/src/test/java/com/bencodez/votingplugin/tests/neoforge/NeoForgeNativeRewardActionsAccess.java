package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import java.util.concurrent.CompletableFuture;

/** Test-only forwarding access to the non-public production NeoForgeNativeRewardActions. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeNativeRewardActionsAccess implements ReflectiveAccess.View, NeoForgeRewardActionsAccess {
    private final Object delegate;
    private NeoForgeNativeRewardActionsAccess(Object delegate) { this.delegate = delegate; }
    public static NeoForgeNativeRewardActionsAccess view(Object delegate) { return delegate == null ? null : new NeoForgeNativeRewardActionsAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof NeoForgeNativeRewardActionsAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static NeoForgeNativeRewardActionsAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.neoforge.NeoForgeNativeRewardActions"))); }
    public NeoForgeNativeRewardActionsAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public NeoForgeNativeRewardActionsAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public NeoForgeNativeRewardActionsAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public NeoForgeNativeRewardActionsAccess(Object server, NeoForgeServerScheduler scheduler, NeoForgePlayerDirectory players) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeNativeRewardActions", new String[]{"Object", "NeoForgeServerScheduler", "NeoForgePlayerDirectory"}, new Object[]{server, scheduler, players})); }
    public CompletableFuture<Void> execute(NeoForgeDeferredVote vote, NeoForgeRewardPlan plan) { return (CompletableFuture<Void>) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeNativeRewardActions", delegate, "execute", new String[]{"NeoForgeDeferredVote", "NeoForgeRewardPlan"}, new Object[]{vote, plan}); }
}
