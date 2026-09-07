package com.bencodez.votingplugin.util;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

/** A scheduled executor with an admission limit for queued and running one-shot tasks. */
public final class BoundedScheduledExecutor extends ScheduledThreadPoolExecutor {
	private final Semaphore permits;

	public BoundedScheduledExecutor(int corePoolSize, int capacity) {
		super(corePoolSize);
		if (capacity < corePoolSize) {
			throw new IllegalArgumentException("capacity must be at least corePoolSize");
		}
		permits = new Semaphore(capacity);
		setRemoveOnCancelPolicy(true);
	}

	@Override
	public ScheduledFuture<?> schedule(Runnable command, long delay, TimeUnit unit) {
		if (!permits.tryAcquire()) {
			throw new RejectedExecutionException("Vote task capacity exhausted");
		}
		try {
			return super.schedule(() -> {
				try {
					command.run();
				} finally {
					permits.release();
				}
			}, delay, unit);
		} catch (RuntimeException ex) {
			permits.release();
			throw ex;
		}
	}
}
