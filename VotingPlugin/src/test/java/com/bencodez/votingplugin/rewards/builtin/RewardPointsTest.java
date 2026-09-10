package com.bencodez.votingplugin.rewards.builtin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

class RewardPointsTest {
	@Test
	void publishesStorageAwarePointTotalWithoutBlockingTheRewardLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserManager manager = mock(UserManager.class);
		AdvancedCoreUser advancedUser = mock(AdvancedCoreUser.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(plugin.getVotingPluginUserManager()).thenReturn(manager);
		when(manager.getVotingPluginUser(advancedUser)).thenReturn(user);
		when(user.addPointsStorageAware(5)).thenReturn(73);

		String result = new RewardPoints(plugin).onRewardRequest(mock(Reward.class), advancedUser, 5,
				new HashMap<>());

		assertEquals("73", result);
		verify(user).addPointsStorageAware(5);
		verify(user, never()).addPoints(5);
	}
}
