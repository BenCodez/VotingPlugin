package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import org.spongepowered.configurate.ConfigurationNode;

/** Test-only forwarding access to the non-public production NeoForgeRewardConfiguration. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeRewardConfigurationAccess implements ReflectiveAccess.View {
    private final Object delegate;
    private NeoForgeRewardConfigurationAccess(Object delegate) { this.delegate = delegate; }
    public static NeoForgeRewardConfigurationAccess view(Object delegate) { return delegate == null ? null : new NeoForgeRewardConfigurationAccess(delegate); }
    @Override public Object delegate() { return delegate; }
    @Override public boolean equals(Object other) { return other instanceof NeoForgeRewardConfigurationAccess value && delegate.equals(value.delegate); }
    @Override public int hashCode() { return delegate.hashCode(); }
    public static NeoForgeRewardConfigurationAccess mock() { return view(org.mockito.Mockito.mock(ReflectiveAccess.type("com.bencodez.votingplugin.neoforge.NeoForgeRewardConfiguration"))); }
    public NeoForgeRewardConfigurationAccess verify() { return view(org.mockito.Mockito.verify(delegate)); }
    public NeoForgeRewardConfigurationAccess verify(org.mockito.verification.VerificationMode mode) { return view(org.mockito.Mockito.verify(delegate, mode)); }
    public NeoForgeRewardConfigurationAccess stub(org.mockito.stubbing.Stubber stubber) { return view(stubber.when(delegate)); }
    public NeoForgeRewardConfigurationAccess(ConfigurationNode config, ConfigurationNode voteSites, ConfigurationNode specialRewards) { this(ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeRewardConfiguration", new String[]{"ConfigurationNode", "ConfigurationNode", "ConfigurationNode"}, new Object[]{config, voteSites, specialRewards})); }
    public NeoForgeRewardPlan plan(NeoForgeDeferredVote vote, NeoForgeVoteSite site, boolean online) { return (NeoForgeRewardPlan) ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeRewardConfiguration", delegate, "plan", new String[]{"NeoForgeDeferredVote", "NeoForgeVoteSite", "boolean"}, new Object[]{vote, site, online}); }
}
