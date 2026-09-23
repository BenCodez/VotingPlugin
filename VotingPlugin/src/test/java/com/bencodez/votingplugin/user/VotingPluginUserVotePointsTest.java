package com.bencodez.votingplugin.user;

import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerReceivePointsEvent;

class VotingPluginUserVotePointsTest {
	@Test
	void repeatedVoteIdentityCommitsPointsAndDispatchesTheReceiveHookOnlyOnce() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		when(plugin.getConfigFile().getPointsOnVote()).thenReturn(5);
		MySQL table = mock(MySQL.class);
		when(plugin.getMysql()).thenReturn(table);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		String uuid = "00000000-0000-0000-0000-000000000001";
		when(base.getUserData()).thenReturn(mock(UserData.class));
		when(base.getUUID()).thenReturn(uuid);
		VotingPluginUser user = org.mockito.Mockito.spy(new VotingPluginUser(plugin, base));
		doReturn(false).when(user).isCached();
		SharedPointAdditionJournal journal = mock(SharedPointAdditionJournal.class);
		UUID voteId = UUID.randomUUID();
		String operationId = "vote-points:" + voteId;
		when(journal.findCompleted(operationId, uuid, "Points"))
				.thenReturn(null, new SharedPointAdditionJournal.AdditionResult(5));
		when(journal.add(org.mockito.ArgumentMatchers.eq(operationId), org.mockito.ArgumentMatchers.eq(uuid),
				org.mockito.ArgumentMatchers.eq("Points"), org.mockito.ArgumentMatchers.eq(5), anyLong()))
				.thenReturn(new SharedPointAdditionJournal.AdditionResult(5));
		PluginManager pluginManager = mock(PluginManager.class);

		try (MockedStatic<SharedPointAdditionJournal> journals = mockStatic(SharedPointAdditionJournal.class);
				MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			journals.when(() -> SharedPointAdditionJournal.forTable(table)).thenReturn(journal);
			bukkit.when(Bukkit::getPluginManager).thenReturn(pluginManager);

			user.addVotePoints(voteId);
			user.addVotePoints(voteId);
		}

		verify(journal).add(org.mockito.ArgumentMatchers.eq(operationId),
				org.mockito.ArgumentMatchers.eq(uuid), org.mockito.ArgumentMatchers.eq("Points"),
				org.mockito.ArgumentMatchers.eq(5), anyLong());
		verify(journal, org.mockito.Mockito.never()).acknowledge(
				org.mockito.ArgumentMatchers.eq(operationId), anyLong());
		verify(pluginManager).callEvent(isA(PlayerReceivePointsEvent.class));
	}
}
