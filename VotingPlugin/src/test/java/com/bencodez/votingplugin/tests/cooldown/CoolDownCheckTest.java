package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

class CoolDownCheckTest {
	@Test
	void perSiteRewardGuardsOnlyTheUnsafeTemplateBoundary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		RewardHandler rewards = mock(RewardHandler.class);
		PlayerVoteSiteCoolDownEndEvent event = mock(PlayerVoteSiteCoolDownEndEvent.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteSite site = mock(VoteSite.class);
		YamlConfiguration data = new YamlConfiguration();
		data.set("CoolDownEndRewards.Commands", java.util.List.of(
				"unsafe %%sitename%%", "safe %sitename%", "%url%foo%%sitename%%"));
		when(plugin.getRewardHandler()).thenReturn(rewards);
		when(event.getPlayer()).thenReturn(user);
		when(event.getSite()).thenReturn(site);
		when(site.getSiteData()).thenReturn(data);
		when(site.isDisplayNameFromAutomaticCreation()).thenReturn(true);
		when(site.getDisplayName()).thenReturn("player_name");
		when(site.getDisplayNameForActions()).thenReturn("player_name");
		when(site.getDisplayNameForFormatting()).thenReturn("\u2060player_name\u2060");
		when(site.getVoteURL(false)).thenReturn("https://example.test");
		CoolDownCheck check = new CoolDownCheck(plugin);
		try {
			check.onCoolDownEnd(event);
		} finally {
			check.shutdown();
		}

		ArgumentCaptor<ConfigurationSection> config = ArgumentCaptor.forClass(ConfigurationSection.class);
		ArgumentCaptor<RewardOptions> options = ArgumentCaptor.forClass(RewardOptions.class);
		verify(rewards).giveReward(eq(user), config.capture(), eq("CoolDownEndRewards"), options.capture());
		assertEquals(java.util.List.of("unsafe %\u2060%sitename%%", "safe %sitename%",
				"%url%foo%\u2060%sitename%%"),
				config.getValue().getStringList("CoolDownEndRewards.Commands"));
		assertEquals("player_name", options.getValue().getPlaceholders().get("sitename"));
	}
}
