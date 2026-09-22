package com.bencodez.votingplugin.user;

import java.util.concurrent.locks.ReentrantReadWriteLock;

/** Orders period-total writes against boundary snapshots and resets. */
public final class PeriodTotalMutationFence {
	private static final ReentrantReadWriteLock FENCE = new ReentrantReadWriteLock(true);

	private PeriodTotalMutationFence() {
	}

	public static void withMutation(Runnable action) {
		var lock = FENCE.readLock();
		lock.lock();
		try {
			action.run();
		} finally {
			lock.unlock();
		}
	}

	public static void withReset(Runnable action) {
		var lock = FENCE.writeLock();
		lock.lock();
		try {
			action.run();
		} finally {
			lock.unlock();
		}
	}
}
