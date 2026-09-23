package com.bencodez.votingplugin.specialrewards;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Set;

import org.bukkit.configuration.file.FileConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.user.VotingPluginUser;

class SpecialRewardsTest {
	@Test
	void recoveredStreakRewardUsesJournaledValueInsteadOfMutableUserValue() {
		VotingPluginMain plugin = Mockito.mock(VotingPluginMain.class);
		SpecialRewardsConfig config = Mockito.mock(SpecialRewardsConfig.class);
		RewardHandler rewardHandler = Mockito.mock(RewardHandler.class);
		FileConfiguration data = Mockito.mock(FileConfiguration.class);
		VotingPluginUser user = Mockito.mock(VotingPluginUser.class);
		SpecialRewards rewards = spy(new SpecialRewards(plugin));
		when(plugin.getSpecialRewardsConfig()).thenReturn(config);
		when(plugin.getRewardHandler()).thenReturn(rewardHandler);
		when(config.getVoteStreakVotes("Day")).thenReturn(Set.of("7"));
		when(config.getVoteStreakRewardEnabled("Day", "7")).thenReturn(true);
		when(config.getVoteStreakRewardsPath("Day", "7")).thenReturn("VoteStreak.Day.7");
		when(config.getData()).thenReturn(data);
		when(rewardHandler.hasRewards(any(), anyString())).thenReturn(true);
		when(user.isOnline()).thenReturn(true);
		doNothing().when(rewards).giveVoteStreakReward(null, user, true, "Day", "7", 7, true);

		rewards.checkVoteStreakAt(null, user, "Day", 7, true);

		verify(rewards).giveVoteStreakReward(null, user, true, "Day", "7", 7, true);
		verify(user, never()).getDayVoteStreak();
	}
}
