package com.bencodez.votingplugin.util;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/** Non-blocking admission helpers for work submitted to the bounded vote timer. */
public final class VoteTaskAdmission {

	private VoteTaskAdmission() {
	}

	/**
	 * Attempts to submit a one-shot vote task without running it on the caller.
	 *
	 * @param executor bounded vote executor
	 * @param task task to submit
	 * @return true when the task was admitted
	 */
	public static boolean trySubmit(ScheduledExecutorService executor, Runnable task) {
		try {
			executor.submit(task);
			return true;
		} catch (RejectedExecutionException rejected) {
			return false;
		}
	}

	/**
	 * Attempts to schedule a one-shot vote task without blocking the caller.
	 *
	 * @param executor bounded vote executor
	 * @param task task to schedule
	 * @param delay delay before execution
	 * @param unit delay unit
	 * @return true when the task was admitted
	 */
	public static boolean trySchedule(ScheduledExecutorService executor, Runnable task, long delay, TimeUnit unit) {
		try {
			executor.schedule(task, delay, unit);
			return true;
		} catch (RejectedExecutionException rejected) {
			return false;
		}
	}
}
