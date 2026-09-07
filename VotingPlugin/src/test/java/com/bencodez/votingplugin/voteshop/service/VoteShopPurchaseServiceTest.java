package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.shop.VoteShopItem;

class VoteShopPurchaseServiceTest {

	@Test
	void concurrentDebitsCannotSpendTheSameBalanceTwice() throws InterruptedException {
		VoteShopPurchaseService service = new VoteShopPurchaseService(null, null);
		VotingPluginUser firstWrapper = mock(VotingPluginUser.class);
		VotingPluginUser secondWrapper = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		int[] balance = { 10 };
		CountDownLatch simultaneousReads = new CountDownLatch(2);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		when(firstWrapper.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(secondWrapper.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		org.mockito.stubbing.Answer<Boolean> nonAtomicDebit = invocation -> {
			int observed = balance[0];
			simultaneousReads.countDown();
			simultaneousReads.await(100, java.util.concurrent.TimeUnit.MILLISECONDS);
			if (observed < 10) {
				return false;
			}
			balance[0] = observed - 10;
			return true;
		};
		when(firstWrapper.removePoints(10, true)).thenAnswer(nonAtomicDebit);
		when(secondWrapper.removePoints(10, true)).thenAnswer(nonAtomicDebit);

		CountDownLatch start = new CountDownLatch(1);
		AtomicReference<VoteShopPurchaseResult> first = new AtomicReference<>();
		AtomicReference<VoteShopPurchaseResult> second = new AtomicReference<>();
		Thread one = new Thread(() -> runDebit(service, firstWrapper, item, start, first));
		Thread two = new Thread(() -> runDebit(service, secondWrapper, item, start, second));
		one.start();
		two.start();
		start.countDown();
		one.join();
		two.join();

		long successes = java.util.stream.Stream.of(first.get(), second.get())
				.filter(result -> result == VoteShopPurchaseResult.SUCCESS).count();
		assertEquals(1, successes);
		assertEquals(0, balance[0]);
		verify(firstWrapper).removePoints(10, true);
		verify(secondWrapper).removePoints(10, true);
	}

	private void runDebit(VoteShopPurchaseService service, VotingPluginUser user, VoteShopItem item,
			CountDownLatch start, AtomicReference<VoteShopPurchaseResult> result) {
		try {
			start.await();
			result.set(service.debitForPurchase(user, item));
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
	}
}
