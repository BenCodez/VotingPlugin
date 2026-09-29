package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class VotingPluginUserOfflineVoteReplayTest {
	@Test
	void clearsOfflineVotesBeforeStartingRewardsSoPermanentFailuresDoNotReplayForever() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getOptions().isProcessRewards()).thenReturn(true);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		VoteSiteManager manager = mock(VoteSiteManager.class);
		VoteSite site = mock(VoteSite.class);
		when(plugin.getVoteSiteManager()).thenReturn(manager);
		when(manager.hasVoteSite("Site1")).thenReturn(true);
		when(manager.getVoteSite("Site1", true)).thenReturn(site);
		doReturn(false).when(user).isTopVoterIgnore();
		doReturn(new ArrayList<>(List.of("Site1"))).when(user).getOfflineVotes();
		doNothing().when(user).sendVoteEffects(false);
		doNothing().when(user).setOfflineVotes(org.mockito.ArgumentMatchers.any());
		doThrow(new IllegalStateException("reward failed")).when(user).playerVote(site, false, false);

		assertThrows(IllegalStateException.class,
				() -> user.offVoteWithCapturedTopVoterIgnore(false));

		InOrder order = inOrder(user);
		order.verify(user).setOfflineVotes(org.mockito.ArgumentMatchers.argThat(List::isEmpty));
		order.verify(user).playerVote(site, false, false);
	}
}
