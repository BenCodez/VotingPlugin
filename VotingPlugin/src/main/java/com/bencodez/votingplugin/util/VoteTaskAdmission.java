package com.bencodez.votingplugin.util;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/** Non-blocking admission helpers for work submitted to the bounded vote timer. */
public final class VoteTaskAdmission {
	private static final ThreadLocal<Integer> VOTE_TASK_DEPTH = new ThreadLocal<>();

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
			executor.submit(wrap(task));
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
			executor.schedule(wrap(task), delay, unit);
			return true;
		} catch (RejectedExecutionException rejected) {
			return false;
		}
	}

	/** Returns whether the current call is owned by an admitted vote-executor task. */
	public static boolean isVoteTask() {
		Integer depth = VOTE_TASK_DEPTH.get();
		return depth != null && depth.intValue() > 0;
	}

	/** Marks an asynchronously scheduled producer as owning vote processing. */
	public static Runnable ownedTask(Runnable task) {
		return wrap(task);
	}

	private static Runnable wrap(Runnable task) {
		return () -> {
			Integer depth = VOTE_TASK_DEPTH.get();
			int previous = depth == null ? 0 : depth.intValue();
			VOTE_TASK_DEPTH.set(Integer.valueOf(previous + 1));
			try {
				task.run();
			} finally {
				if (previous == 0) VOTE_TASK_DEPTH.remove();
				else VOTE_TASK_DEPTH.set(Integer.valueOf(previous));
			}
		};
	}
}
