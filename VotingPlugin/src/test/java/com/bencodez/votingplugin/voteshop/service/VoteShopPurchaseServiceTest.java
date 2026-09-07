package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

class VoteShopPurchaseServiceTest {

	@Test
	void wrappersForSamePlayerShareLockAcrossServiceReloads() {
		VoteShopPurchaseService firstService = new VoteShopPurchaseService(null, null);
		VoteShopPurchaseService reloadedService = new VoteShopPurchaseService(null, null);
		String uuid = "00000000-0000-0000-0000-000000000001";
		assertSame(firstService.purchaseLock(uuid), reloadedService.purchaseLock(uuid));
	}

	@Test
	void debitUsesAsynchronousPersistence() {
		VoteShopPurchaseService service = new VoteShopPurchaseService(null, null);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		when(user.removePoints(10, true)).thenReturn(true);

		assertEquals(VoteShopPurchaseResult.SUCCESS, service.debitForPurchase(user, item));
		verify(user).removePoints(10, true);
	}
}
