package com.bencodez.votingplugin.timequeue;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Deque;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.util.VoteTaskAdmission;

/**
 * The TimeQueueHandler class manages time-based vote queue processing.
 */
public class TimeQueueHandler implements Listener {
	private static final long TICKS_PER_SECOND = 20L;
	private final Deque<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedDeque<>();

	private VotingPluginMain plugin;
	private final AtomicBoolean retryPending = new AtomicBoolean();
	private final AtomicBoolean retryPersistenceRequired = new AtomicBoolean();
	private final AtomicInteger retryAttempts = new AtomicInteger();

	public Queue<VoteTimeQueue> getTimeChangeQueue() {
		return timeChangeQueue;
	}

	/**
	 * Constructs a new TimeQueueHandler.
	 *
	 * @param plugin the main plugin instance
	 */
	public TimeQueueHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		load();
	}

	/**
	 * Adds a vote to the time change queue.
	 *
	 * @param voteUsername the voter username
	 * @param voteSiteName the vote site name
	 */
	public void addVote(String voteUsername, String voteSiteName) {
		addVote(null, voteUsername, voteSiteName);
	}

	/** Adds a vote while preserving the identity assigned at reception. */
	public void addVote(UUID voteId, String voteUsername, String voteSiteName) {
		timeChangeQueue.add(new VoteTimeQueue(voteId, voteUsername, voteSiteName,
				LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
	}

	/**
	 * Loads cached votes from server data and schedules queue processing.
	 */
	public void load() {
		List<String> keys = new ArrayList<>(plugin.getServerData().getTimedVoteCacheKeys());
		keys.sort(Comparator.comparingInt(TimeQueueHandler::timedVoteCacheIndex));
		for (String str : keys) {
			ConfigurationSection data = plugin.getServerData().getTimedVoteCacheSection(str);
			UUID voteId = null;
			try {
				String storedVoteId = data.getString("VoteId", "");
				if (storedVoteId != null && !storedVoteId.isEmpty()) voteId = UUID.fromString(storedVoteId);
			} catch (IllegalArgumentException invalidVoteId) {
				plugin.debug(invalidVoteId);
			}
			timeChangeQueue.add(new VoteTimeQueue(voteId, data.getString("Name"), data.getString("Service"),
					data.getLong("Time")));
		}
		scheduleQueueProcessing(120, TimeUnit.SECONDS);
	}

	private static int timedVoteCacheIndex(String key) {
		try {
			return Integer.parseInt(key);
		} catch (NumberFormatException ignored) {
			return Integer.MAX_VALUE;
		}
	}

	/**
	 * Handles date change event and schedules queue processing.
	 *
	 * @param event the date changed event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void postTimeChange(DateChangedEvent event) {
		TimeChangeTransition transition = event == null ? null : event.getTransition();
		if (transition == null) {
			scheduleQueueProcessing(5, TimeUnit.SECONDS);
			return;
		}
		TimeChangeTransition.Lease lease = transition.retain();
		try {
			ensureTransitionActive(transition);
			scheduleQueueProcessing(5, TimeUnit.SECONDS);
			ensureTransitionActive(transition);
			lease.complete();
		} catch (Throwable failure) {
			lease.fail(failure);
			plugin.getLogger().warning("Time-queue post-period scheduling remains pending: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private void scheduleQueueProcessing(long delay, TimeUnit unit) {
		boolean admitted = VoteTaskAdmission.trySchedule(plugin.getVoteTimer(), () -> {
			// Clear only after the bounded executor has admitted the task. If it is
			// rejected, shutdown persistence can still recover the in-memory queue.
			plugin.getServerData().clearTimedVoteCache();
			processQueue();
		}, delay, unit);
		if (!admitted) {
			plugin.getLogger().warning("Unable to schedule time-queue processing because vote processing is busy; queued votes were retained.");
			scheduleRetry(false);
		} else {
			retryAttempts.set(0);
		}
	}

	private void scheduleRetry(boolean persistenceRequired) {
		if (persistenceRequired) retryPersistenceRequired.set(true);
		if (timeChangeQueue.isEmpty() || !retryPending.compareAndSet(false, true)) return;
		if (!plugin.isEnabled()) {
			retryPending.set(false);
			return;
		}
		int attempt = Math.min(6, retryAttempts.getAndIncrement());
		long delaySeconds = Math.min(60L, 1L << attempt);
		long delayTicks = delaySeconds * TICKS_PER_SECOND;
		try {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, () -> {
				retryPending.set(false);
				if (timeChangeQueue.isEmpty()) return;
				if (retryPersistenceRequired.getAndSet(false) && !persistQueueSnapshot()) {
					scheduleRetry(true);
					return;
				}
				scheduleQueueProcessing(0, TimeUnit.SECONDS);
			}, delayTicks);
		} catch (RuntimeException rejected) {
			retryPending.set(false);
			plugin.getLogger().warning("Unable to queue a time-queue retry; pending votes remain persisted for recovery.");
		}
	}

	private boolean persistQueueSnapshot() {
		try {
			plugin.getServerData().replaceTimedVoteCache(new ArrayList<>(timeChangeQueue));
			return true;
		} catch (RuntimeException persistenceFailure) {
			plugin.getLogger().severe("Unable to persist the time-queue retry; the vote remains in memory");
			plugin.debug(persistenceFailure);
			return false;
		}
	}

	private void ensureTransitionActive(TimeChangeTransition transition) {
		if (!plugin.isEnabled() || transition.isCancellationRequested()) {
			throw new CancellationException("Time transition was cancelled before time-queue scheduling completed");
		}
	}

	/**
	 * Processes all votes in the queue.
	 */
	public void processQueue() {
		while (getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getTimeChangeQueue().remove();
			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSiteManager().getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, vote.getService()), true), vote.getName(),
					vote.getService(), true);
			voteEvent.setTime(vote.getTime());
			voteEvent.setVoteId(vote.getVoteId() == null ? vote.legacyTimedVoteId() : vote.getVoteId());
			plugin.getServer().getPluginManager().callEvent(voteEvent);
			if (voteEvent.isAccountingAdmissionFailed()) {
				timeChangeQueue.addFirst(vote);
				scheduleRetry(!persistQueueSnapshot());
				return;
			}

			if (voteEvent.isCancelled()) {
				plugin.debug("Vote cancelled");
				return;
			}
		}
	}

	/**
	 * Saves pending votes to server data.
	 */
	public void save() {
		if (!timeChangeQueue.isEmpty()) {
			plugin.getServerData().replaceTimedVoteCache(new ArrayList<>(timeChangeQueue));
		}
		timeChangeQueue.clear();
	}
}
