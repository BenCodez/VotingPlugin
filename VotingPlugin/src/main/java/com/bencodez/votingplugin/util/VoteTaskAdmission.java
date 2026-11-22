package com.bencodez.votingplugin.util;

import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

/** Non-blocking admission helpers for work submitted to the bounded vote timer. */
public final class VoteTaskAdmission {

	private VoteTaskAdmission() {
	}

	/**
	 * Command/GUI adapter: storage work runs off platform owners. An optional
	 * success message confirms event dispatch, not asynchronous reward completion.
	 * Unaccepted requests receive busy feedback before any vote effects run.
	 */
	public static void dispatchManualVote(VotingPluginMain plugin, PlayerVoteEvent event,
			CommandSender sender, String successMessage) {
		Runnable work = () -> {
			var party = plugin.getVoteParty();
			if (party == null || !party.tryDispatchVoteEvent(
					() -> plugin.getServer().getPluginManager().callEvent(event))) {
				manualVoteReply(plugin, sender, "Vote could not be triggered because vote processing is busy; please try again later.");
			} else if (successMessage != null) {
				manualVoteReply(plugin, sender, event.isCancelled() ? "Vote was cancelled before processing." : successMessage);
			}
		};
		var manager = plugin.getUserManager().getDataManager();
		if (manager != null && !manager.isPlatformOwnedThread()) work.run();
		else if (!trySubmit(plugin.getVoteTimer(), work))
			manualVoteReply(plugin, sender, "Vote could not be triggered because vote processing is busy; please try again later.");
	}

	private static void manualVoteReply(VotingPluginMain plugin, CommandSender sender, String message) {
		if (sender == null) { plugin.getLogger().warning(message); return; }
		Runnable reply = () -> sender.sendMessage(message);
		try {
			if (sender instanceof Player player) plugin.getBukkitScheduler().runTask(plugin, reply, player);
			else plugin.getBukkitScheduler().runTask(plugin, reply);
		} catch (RuntimeException rejected) {
			plugin.getLogger().warning("Manual vote feedback could not be scheduled.");
		}
	}

	/**
	 * Attempts to submit a one-shot vote task without running it on the caller.
	 *
	 * @param executor bounded vote executor, or null before startup/after shutdown
	 * @param task task to submit
	 * @return true when the task was admitted
	 */
	public static boolean trySubmit(ScheduledExecutorService executor, Runnable task) {
		if (executor == null) return false;
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
	 * @param executor bounded vote executor, or null before startup/after shutdown
	 * @param task task to schedule
	 * @param delay delay before execution
	 * @param unit delay unit
	 * @return true when the task was admitted
	 */
	public static boolean trySchedule(ScheduledExecutorService executor, Runnable task, long delay, TimeUnit unit) {
		if (executor == null) return false;
		try {
			executor.schedule(task, delay, unit);
			return true;
		} catch (RejectedExecutionException rejected) {
			return false;
		}
	}
}
