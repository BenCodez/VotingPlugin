package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.UUID;

import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

class VotingPluginUserVoteShopLimitTest {
	@Test
	void sharedMysqlDailyVotesUseTheCrossBackendBoundaryMutation() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(mock(UserData.class));
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(null).when(user).getCache();
		doReturn(0).when(user).getDailyTotal();
		UUID voteId = UUID.randomUUID();
		try (MockedStatic<VoteShopPurchaseService> service = org.mockito.Mockito
				.mockStatic(VoteShopPurchaseService.class)) {
			service.when(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"DailyTotal", "LastDailyTotal", java.util.List.of("DailyTotal"), null)).thenReturn(true);

			user.addTotalDaily(voteId);

			service.verify(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"DailyTotal", "LastDailyTotal", java.util.List.of("DailyTotal"), null));
		}
	}

	@Test
	void sharedMysqlVotePartyCountsUseTheCrossBackendBoundaryMutation() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(mock(UserData.class));
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(null).when(user).getCache();
		doReturn(0).when(user).getVotePartyVotes();
		UUID voteId = UUID.randomUUID();
		try (MockedStatic<VoteShopPurchaseService> service = org.mockito.Mockito
				.mockStatic(VoteShopPurchaseService.class)) {
			service.when(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"VotePartyVotes", "LastVotePartyVotes", java.util.List.of("VotePartyVotes"), null))
					.thenReturn(true);

			user.addVotePartyVote(voteId);

			service.verify(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"VotePartyVotes", "LastVotePartyVotes", java.util.List.of("VotePartyVotes"), null));
		}
	}

	@Test
	void failedSharedMysqlIncrementFallsBackToTheQueuedUserMutation() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(mock(UserData.class));
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(null).when(user).getCache();
		doReturn(4).when(user).getDailyTotal();
		doNothing().when(user).setDailyTotal(org.mockito.ArgumentMatchers.anyInt());
		UUID voteId = UUID.randomUUID();
		try (MockedStatic<VoteShopPurchaseService> service = org.mockito.Mockito
				.mockStatic(VoteShopPurchaseService.class)) {
			service.when(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"DailyTotal", "LastDailyTotal", java.util.List.of("DailyTotal"), null)).thenReturn(false);

			user.addTotalDaily(voteId);

			verify(user).setDailyTotal(5);
		}
	}

	@Test
	void failedSharedMysqlMonthlyIncrementKeepsTheConfiguredCap() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getConfigFile().isLimitMonthlyVotes()).thenReturn(true);
		when(plugin.getTimeChecker().getTime().getDayOfMonth()).thenReturn(2);
		when(plugin.getVoteSiteManager().getVoteSitesEnabled().size()).thenReturn(3);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(mock(UserData.class));
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(null).when(user).getCache();
		doReturn(6).when(user).getMonthTotal();
		doNothing().when(user).setMonthTotal(org.mockito.ArgumentMatchers.anyInt());
		UUID voteId = UUID.randomUUID();
		try (MockedStatic<VoteShopPurchaseService> service = org.mockito.Mockito
				.mockStatic(VoteShopPurchaseService.class)) {
			service.when(() -> VoteShopPurchaseService.incrementMysqlPeriodTotals(plugin, voteId, base.getUUID(),
					"MonthTotal", "LastMonthTotal", java.util.List.of("MonthTotal"), Integer.valueOf(6)))
					.thenReturn(false);

			user.addMonthTotal(voteId);

			verify(user).setMonthTotal(6);
		}
	}

	@Test
	void sharedMysqlLimitsUseNonBlockingUserCacheWhenAvailable() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		UserData data = mock(UserData.class);
		when(data.getInt("VoteShopLimitdaily", UserDataFetchMode.CACHE_ONLY)).thenReturn(3);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");

		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(data).when(user).getData();
		doReturn(true).when(user).isCached();
		assertEquals(3, user.getVoteShopIdentifierLimit("daily"));

		verify(data).getInt("VoteShopLimitdaily", UserDataFetchMode.CACHE_ONLY);
		verify(data, never()).getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE);
	}

	@Test
	void sharedMysqlLimitsUseOnlyTemporaryDataWhenUserIsNotCached() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(false);
		UserData data = mock(UserData.class);
		when(data.getInt("VoteShopLimitdaily", UserDataFetchMode.TEMP_ONLY)).thenReturn(3);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");

		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(data).when(user).getData();
		assertEquals(3, user.getVoteShopIdentifierLimit("daily"));
		user.setVoteShopIdentifierLimit("daily", 4);

		verify(data).getInt("VoteShopLimitdaily", UserDataFetchMode.TEMP_ONLY);
		verify(data, never()).getInt("VoteShopLimitdaily", UserDataFetchMode.NO_CACHE);
		verify(data).setInt("VoteShopLimitdaily", 4, false);
	}

	@Test
	void perServerMysqlLimitsAlsoUseDirectWritesBecauseTheirColumnIsShared() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
		when(plugin.getBungeeSettings().isPerServerPoints()).thenReturn(true);
		UserData data = mock(UserData.class);
		AdvancedCoreUser base = mock(AdvancedCoreUser.class);
		when(base.getUserData()).thenReturn(data);
		when(base.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(base.getPlayerName()).thenReturn("Player");

		VotingPluginUser user = spy(new VotingPluginUser(plugin, base));
		doReturn(data).when(user).getData();
		user.setVoteShopIdentifierLimit("daily", 4);

		verify(data).setInt("VoteShopLimitdaily", 4, false);
	}
}
