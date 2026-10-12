package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.usercache.UserDataManager;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKey;
import com.bencodez.votingplugin.VotingPluginMain;

class UserManagerRecoveryKeysTest {
	@Test
	void registersPersistedRecoveryFieldsInSharedUserStorageSchema() {
		VotingPluginMain plugin = mock(VotingPluginMain.class,
				org.mockito.Mockito.RETURNS_DEEP_STUBS);
		UserDataManager storage = mock(UserDataManager.class);
		when(plugin.getUserManager().getDataManager()).thenReturn(storage);

		new UserManager(plugin).addCachingKeys();

		ArgumentCaptor<UserDataKey> registered = ArgumentCaptor.forClass(UserDataKey.class);
		org.mockito.Mockito.verify(storage, org.mockito.Mockito.atLeastOnce()).addKey(registered.capture());
		List<String> names = registered.getAllValues().stream().map(UserDataKey::getKey).toList();
		assertTrue(names.contains("OfflineVotes"), "Legacy queue stays registered");
		assertTrue(names.contains("OfflineVotesRewardPending"), "Durable pending vote batch must be registered");
		assertTrue(names.contains("NameMCLikeRewardPending"), "Durable NameMC pending marker must be registered");
		assertTrue(names.contains("NameMCLikeRewardClaimed"), "Existing NameMC claim field must be registered");
	}
}
