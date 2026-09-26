package com.bencodez.votingplugin.proxy.redis;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.junit.jupiter.api.Test;

class VotingPluginRedisChannelsTest {
	@Test
	void separatesMultiProxyNetworksByConfiguredPrefix() {
		String networkA = VotingPluginRedisChannels.multiProxy("network-a:", "proxy-1");
		String networkB = VotingPluginRedisChannels.multiProxy("network-b:", "proxy-1");

		assertEquals("network-a:VotingPluginProxy_proxy-1", networkA);
		assertNotEquals(networkA, networkB);
	}

	@Test
	void appliesPrefixExactlyOnceForEveryChannelRole() {
		assertEquals("vp:VotingPlugin", VotingPluginRedisChannels.proxy("vp:"));
		assertEquals("vp:VotingPlugin_backend-a", VotingPluginRedisChannels.backend("vp:", "backend-a"));
		assertEquals("vp:VotingPluginProxy_proxy-a", VotingPluginRedisChannels.multiProxy("vp:", "proxy-a"));
	}
}
