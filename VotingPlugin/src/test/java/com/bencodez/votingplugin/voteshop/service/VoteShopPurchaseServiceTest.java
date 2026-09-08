package com.bencodez.votingplugin.voteshop.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

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

	@Test
	void concurrentWrappersCannotDebitSamePlayerTogether() throws Exception {
		VoteShopPurchaseService first = new VoteShopPurchaseService(null, null);
		VoteShopPurchaseService second = new VoteShopPurchaseService(null, null);
		VotingPluginUser user = mock(VotingPluginUser.class);
		VoteShopItem item = mock(VoteShopItem.class);
		when(user.getUUID()).thenReturn("00000000-0000-0000-0000-000000000001");
		when(item.getCost()).thenReturn(10);
		when(item.getLimit()).thenReturn(0);
		CountDownLatch firstInsideDebit = new CountDownLatch(1);
		CountDownLatch releaseFirst = new CountDownLatch(1);
		AtomicInteger calls = new AtomicInteger();
		when(user.removePoints(10, true)).thenAnswer(invocation -> {
			if (calls.incrementAndGet() == 1) {
				firstInsideDebit.countDown();
				releaseFirst.await(5, TimeUnit.SECONDS);
				return true;
			}
			return false;
		});
		ExecutorService executor = Executors.newFixedThreadPool(2);
		try {
			Future<VoteShopPurchaseResult> one = executor.submit(() -> first.debitForPurchase(user, item));
			assertEquals(true, firstInsideDebit.await(5, TimeUnit.SECONDS));
			AtomicReference<Thread> secondThread = new AtomicReference<>();
			Future<VoteShopPurchaseResult> two = executor.submit(() -> {
				secondThread.set(Thread.currentThread());
				return second.debitForPurchase(user, item);
			});
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
			while (System.nanoTime() < deadline
					&& (secondThread.get() == null || secondThread.get().getState() != Thread.State.BLOCKED)
					&& !two.isDone()) {
				Thread.onSpinWait();
			}
			assertTrue(secondThread.get() != null && secondThread.get().getState() == Thread.State.BLOCKED,
					"the second debit did not block on the shared purchase lock");
			assertEquals(1, calls.get());
			releaseFirst.countDown();
			assertEquals(VoteShopPurchaseResult.SUCCESS, one.get(5, TimeUnit.SECONDS));
			assertEquals(VoteShopPurchaseResult.NOT_ENOUGH_POINTS, two.get(5, TimeUnit.SECONDS));
			verify(user, times(2)).removePoints(10, true);
		} finally {
			releaseFirst.countDown();
			executor.shutdownNow();
		}
	}
}
