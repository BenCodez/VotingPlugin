package com.bencodez.votingplugin.topvoter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.user.VotingPluginUser;

class TopVoterTimeChangeRecoveryTest {
	@Test
	void retryUsesPersistedAbsoluteTargetAndDoesNotDuplicateCompletedStreakReward() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ServerData serverData = mock(ServerData.class);
		SpecialRewards specialRewards = mock(SpecialRewards.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getSpecialRewards()).thenReturn(specialRewards);
		when(serverData.prepareTimeChangeUserStreak(transition, uuid, 9, true))
				.thenReturn(new TimeChangeUserProgress(uuid, 8, true, true));
		when(user.getWeekVoteStreak()).thenReturn(8);

		new TopVoterHandler(plugin).applyRecoverableStreak(user, transition, uuid, TopVoter.Weekly, 9, true);

		verify(user, never()).setWeekVoteStreak(org.mockito.ArgumentMatchers.anyInt());
		verify(specialRewards, never()).checkVoteStreak(org.mockito.ArgumentMatchers.any(),
				org.mockito.ArgumentMatchers.eq(user), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyBoolean());
		verify(serverData, never()).completeTimeChangeUserStreakReward(transition, uuid);
	}
}
