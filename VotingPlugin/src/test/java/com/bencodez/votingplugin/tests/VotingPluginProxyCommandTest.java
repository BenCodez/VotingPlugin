package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginProxyCommand;

public class VotingPluginProxyCommandTest {
	@Test
	void manualVoteReportsRetryableDurabilityFailure() {
		VotingPluginProxy proxy = mock(VotingPluginProxy.class);
		doThrow(VotingPluginProxy.VoteRetryException.class).when(proxy)
				.vote(eq("Player"), eq("Service"), eq(false), eq(true), eq(0L), eq(null), eq(null), any(UUID.class));
		VotingPluginProxyCommand command = new VotingPluginProxyCommand(proxy);

		String firstResult = command.execute(new String[] { "vote", "Player", "Service" });
		String retryIdText = firstResult.substring(firstResult.lastIndexOf(' ') + 1);
		UUID retryId = UUID.fromString(retryIdText);
		String secondResult = command.execute(new String[] { "vote", "Player", "Service", retryIdText });

		assertEquals("&cVote could not be stored safely. Retry with: vote Player Service " + retryId, firstResult);
		assertEquals(firstResult, secondResult);
		ArgumentCaptor<UUID> ids = ArgumentCaptor.forClass(UUID.class);
		verify(proxy, times(2)).vote(eq("Player"), eq("Service"), eq(false), eq(true), eq(0L), eq(null), eq(null),
				ids.capture());
		assertEquals(java.util.List.of(retryId, retryId), ids.getAllValues());
	}
}
