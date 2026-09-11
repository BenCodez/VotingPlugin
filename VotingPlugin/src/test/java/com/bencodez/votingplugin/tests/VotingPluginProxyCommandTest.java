package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyCommand;

public class VotingPluginProxyCommandTest {
	@Test
	void manualVoteReportsRetryableDurabilityFailure() {
		VotingPluginProxy proxy = mock(VotingPluginProxy.class);
		doThrow(VotingPluginProxy.VoteRetryException.class).when(proxy)
				.vote("Player", "Service", false, true, 0, null, null);

		String result = new VotingPluginProxyCommand(proxy)
				.execute(new String[] { "vote", "Player", "Service" });

		assertEquals("&cVote could not be stored safely. Please retry shortly.", result);
	}
}
