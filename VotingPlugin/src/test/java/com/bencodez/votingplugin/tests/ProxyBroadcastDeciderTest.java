package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.VotingPluginProxyConfig;
import com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider;

public class ProxyBroadcastDeciderTest {

	@Test
	void forwardModeUsesImmediateDeliveryWhenTheVotingPlayerIsOffline() {
		VotingPluginProxyConfig config = mock(VotingPluginProxyConfig.class);
		when(config.getProxyBroadcastEnabled()).thenReturn(true);
		when(config.getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
		when(config.getProxyBroadcastScopeMode()).thenReturn("ALL_SERVERS");

		Set<String> servers = new LinkedHashSet<>(Arrays.asList("Server1", "Server2"));
		ProxyBroadcastDecider decider = new ProxyBroadcastDecider(() -> config, () -> servers, server -> true,
				server -> false);

		assertTrue(decider.usesImmediateForwarding(false));
		assertFalse(decider.usesImmediateForwarding(true));
		assertEquals(servers, decider.resolveTargets(false, null));
	}

	@Test
	void queuedAndDisabledBroadcastsDoNotUseImmediateDelivery() {
		VotingPluginProxyConfig config = mock(VotingPluginProxyConfig.class);
		when(config.getProxyBroadcastEnabled()).thenReturn(true);
		when(config.getProxyBroadcastOfflineMode()).thenReturn("QUEUE");

		ProxyBroadcastDecider decider = new ProxyBroadcastDecider(() -> config, LinkedHashSet::new, server -> true,
				server -> false);

		assertFalse(decider.usesImmediateForwarding(false));

		when(config.getProxyBroadcastEnabled()).thenReturn(false);
		when(config.getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
		assertFalse(decider.usesImmediateForwarding(false));
	}
}
