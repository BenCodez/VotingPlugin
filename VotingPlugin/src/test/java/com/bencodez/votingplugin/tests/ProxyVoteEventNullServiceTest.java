package com.bencodez.votingplugin.tests;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.bungee.VoteEventBungee;
import com.bencodez.votingplugin.proxy.bungee.VotingPluginBungee;
import com.bencodez.votingplugin.proxy.velocity.VoteEventVelocity;
import com.bencodez.votingplugin.proxy.velocity.VotingPluginVelocity;
import com.vexsoftware.votifier.model.Vote;

class ProxyVoteEventNullServiceTest {
	@Test
	void bungeeNullServiceUsesCompatibilityNameBeforeAdmission() {
		VotingPluginBungee plugin = mock(VotingPluginBungee.class);
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn(null);
		com.vexsoftware.votifier.bungee.events.VotifierEvent event =
				mock(com.vexsoftware.votifier.bungee.events.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventBungee(plugin).onVote(event);

		verify(plugin).acceptIncomingVote("Player", "Empty");
	}

	@Test
	void velocityNullServiceUsesCompatibilityNameBeforeAdmission() {
		VotingPluginVelocity plugin = mock(VotingPluginVelocity.class);
		Vote vote = mock(Vote.class);
		when(vote.getUsername()).thenReturn("Player");
		when(vote.getServiceName()).thenReturn(null);
		com.vexsoftware.votifier.velocity.event.VotifierEvent event =
				mock(com.vexsoftware.votifier.velocity.event.VotifierEvent.class);
		when(event.getVote()).thenReturn(vote);

		new VoteEventVelocity(plugin).onVotifierEvent(event);

		verify(plugin).acceptIncomingVote("Player", "Empty");
	}
}
