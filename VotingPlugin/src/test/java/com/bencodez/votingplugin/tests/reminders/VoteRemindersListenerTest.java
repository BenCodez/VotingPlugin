package com.bencodez.votingplugin.tests.reminders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.rewards.RewardDisplayPlaceholders;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votereminding.VoteRemindersListener;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager.VoteReminderType;
import com.bencodez.votingplugin.votesites.VoteSite;

class VoteRemindersListenerTest {

	@Test
	void postVoteCarriesGuardedActionProvenanceAndSeparateDisplayValue() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		VoteRemindersManager manager = mock(VoteRemindersManager.class);
		VoteSite site = mock(VoteSite.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		PlayerPostVoteEvent event = mock(PlayerPostVoteEvent.class);
		when(plugin.getVoteRemindersManager()).thenReturn(manager);
		when(event.getUser()).thenReturn(user);
		when(event.getVoteSite()).thenReturn(site);
		when(site.getDisplayNameForActions()).thenReturn("site_key");
		when(site.getDisplayNameForFormatting()).thenReturn("\u2060site_key\u2060");
		when(site.isDisplayNameFromAutomaticCreation()).thenReturn(true);

		new VoteRemindersListener(plugin).onPostVote(event);

		verify(manager).onVoteCast(user, "site_key", "\u2060site_key\u2060", Set.of("site"));
	}

	@Test
	void cooldownKeepsRawActionValueAndSeparateDisplayValue() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		VoteRemindersManager manager = mock(VoteRemindersManager.class);
		VoteSite site = mock(VoteSite.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		PlayerVoteSiteCoolDownEndEvent event = mock(PlayerVoteSiteCoolDownEndEvent.class);
		when(plugin.getVoteRemindersManager()).thenReturn(manager);
		when(event.getPlayer()).thenReturn(user);
		when(event.getSite()).thenReturn(site);
		when(site.getDisplayNameForActions()).thenReturn("site_key");
		when(site.getDisplayNameForFormatting()).thenReturn("\u2060site_key\u2060");
		when(site.getKey()).thenReturn("site_key");

		new VoteRemindersListener(plugin).onCoolDownEnd(event);

		@SuppressWarnings("unchecked")
		ArgumentCaptor<Map<String, String>> placeholders = ArgumentCaptor.forClass(Map.class);
		verify(manager).onCooldownTrigger(eq(user), eq(VoteReminderType.COOLDOWN_END_ANY_SITE), placeholders.capture(),
				eq(Set.of()));
		HashMap<String, String> values = new HashMap<>(placeholders.getValue());
		assertEquals("site_key", values.get("votesite"));
		assertEquals("\u2060site_key\u2060", RewardDisplayPlaceholders.forDisplay(values).get("votesite"));
	}

	@Test
	void cooldownGuardsAutomaticallyCreatedSiteKeyAndItsProvenance() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		VoteRemindersManager manager = mock(VoteRemindersManager.class);
		VoteSite site = mock(VoteSite.class);
		VotingPluginUser user = mock(VotingPluginUser.class);
		PlayerVoteSiteCoolDownEndEvent event = mock(PlayerVoteSiteCoolDownEndEvent.class);
		when(plugin.getVoteRemindersManager()).thenReturn(manager);
		when(event.getPlayer()).thenReturn(user);
		when(event.getSite()).thenReturn(site);
		when(site.getDisplayNameForActions()).thenReturn("Configured display");
		when(site.getDisplayNameForFormatting()).thenReturn("\u2060Configured display\u2060");
		when(site.getKeyForActions()).thenReturn("%\u2060player_name%");
		when(site.isKeyFromAutomaticCreation()).thenReturn(true);

		new VoteRemindersListener(plugin).onCoolDownEnd(event);

		@SuppressWarnings("unchecked")
		ArgumentCaptor<Map<String, String>> placeholders = ArgumentCaptor.forClass(Map.class);
		verify(manager).onCooldownTrigger(eq(user), eq(VoteReminderType.COOLDOWN_END_ANY_SITE), placeholders.capture(),
				eq(Set.of("votesite_id")));
		assertEquals("%\u2060player_name%", placeholders.getValue().get("votesite_id"));
	}
}
