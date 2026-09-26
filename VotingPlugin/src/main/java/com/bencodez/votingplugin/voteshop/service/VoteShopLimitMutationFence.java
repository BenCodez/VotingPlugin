package com.bencodez.votingplugin.voteshop.service;

import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/** Orders local VoteShop limit mutations against period resets. */
public final class VoteShopLimitMutationFence {
	private static final ReentrantLock FENCE = new ReentrantLock(true);

	private VoteShopLimitMutationFence() {
	}

	public static <T> T withLock(Supplier<T> action) {
		FENCE.lock();
		try {
			return action.get();
		} finally {
			FENCE.unlock();
		}
	}

	public static void withLock(Runnable action) {
		withLock(() -> {
			action.run();
			return null;
		});
	}
}
