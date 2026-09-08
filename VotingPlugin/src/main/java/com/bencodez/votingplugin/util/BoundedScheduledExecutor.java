package com.bencodez.votingplugin.util;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Delayed;
import java.util.concurrent.atomic.AtomicBoolean;

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
		AtomicBoolean released = new AtomicBoolean();
		try {
			ScheduledFuture<?> delegate = super.schedule(() -> {
				try {
					command.run();
				} finally {
					releaseOnce(released);
				}
			}, delay, unit);
			return new PermitReleasingFuture(delegate, released);
		} catch (RuntimeException ex) {
			permits.release();
			throw ex;
		}
	}

	private void releaseOnce(AtomicBoolean released) {
		if (released.compareAndSet(false, true)) {
			permits.release();
		}
	}

	private final class PermitReleasingFuture implements ScheduledFuture<Object> {
		private final ScheduledFuture<?> delegate;
		private final AtomicBoolean released;
		private PermitReleasingFuture(ScheduledFuture<?> delegate, AtomicBoolean released) {
			this.delegate = delegate;
			this.released = released;
		}
		@Override public boolean cancel(boolean mayInterruptIfRunning) {
			boolean cancelled = delegate.cancel(mayInterruptIfRunning);
			if (cancelled) releaseOnce(released);
			return cancelled;
		}
		@Override public boolean isCancelled() { return delegate.isCancelled(); }
		@Override public boolean isDone() { return delegate.isDone(); }
		@Override public Object get() throws java.lang.InterruptedException, java.util.concurrent.ExecutionException { return delegate.get(); }
		@Override public Object get(long timeout, TimeUnit unit) throws java.lang.InterruptedException, java.util.concurrent.ExecutionException, java.util.concurrent.TimeoutException { return delegate.get(timeout, unit); }
		@Override public long getDelay(TimeUnit unit) { return delegate.getDelay(unit); }
		@Override public int compareTo(Delayed other) { return delegate.compareTo(other); }
	}
}
