package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.votingplugin.VotingPluginMain;

class VotingPluginUserVoteShopLimitTest {
	@Test
	void sharedMysqlLimitsBypassCachedReadsAndQueuedWrites() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		UserData data = mock(UserData.class);
		when(data.getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE)).thenReturn(3);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");

		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(data).when(user).getData();
		assertEquals(3, user.getVoteShopIdentifierLimit("daily"));
		user.setVoteShopIdentifierLimit("daily", 4);

		verify(data).getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE);
		verify(data).setInt("VoteShopLimitdaily", 4, false);
	}
}
