package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		AtomicInteger balance = new AtomicInteger(10);
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		when(user.removePoints(10)).thenAnswer(invocation -> balance.compareAndSet(10, 0));

		CountDownLatch start = new CountDownLatch(1);
		AtomicReference<VoteShopPurchaseResult> first = new AtomicReference<>();
		AtomicReference<VoteShopPurchaseResult> second = new AtomicReference<>();
		Thread one = new Thread(() -> runDebit(service, user, item, start, first));
		Thread two = new Thread(() -> runDebit(service, user, item, start, second));
		one.start();
		two.start();
		start.countDown();
		one.join();
		two.join();

		long successes = java.util.stream.Stream.of(first.get(), second.get())
				.filter(result -> result == VoteShopPurchaseResult.SUCCESS).count();
		assertEquals(1, successes);
		assertEquals(0, balance.get());
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
