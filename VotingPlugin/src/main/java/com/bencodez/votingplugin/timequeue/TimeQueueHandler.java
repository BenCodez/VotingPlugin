package com.bencodez.votingplugin.timequeue;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.util.VoteTaskAdmission;

import lombok.Getter;

/**
 * The TimeQueueHandler class manages time-based vote queue processing.
 */
public class TimeQueueHandler implements Listener {
	private static final long TICKS_PER_SECOND = 20L;
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	private VotingPluginMain plugin;
	private final Object persistenceLock = new Object();
	private boolean stopping;
	private final AtomicBoolean processing = new AtomicBoolean();
	private VoteTimeQueue dispatchingVote;
	private UUID dispatchId;
	private int persistedShutdownVotes;
	private final AtomicBoolean retryPending = new AtomicBoolean();
	private final AtomicInteger retryAttempts = new AtomicInteger();

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
		synchronized (persistenceLock) {
			timeChangeQueue.add(new VoteTimeQueue(voteUsername, voteSiteName,
					LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
			// A vote task can outlive shutdownNow(). Persist its late admission
			// through the same owner rather than appending to an already-saved queue.
			if (stopping) persistPendingVotes();
		}
	}

	/**
	 * Loads cached votes from server data and schedules queue processing.
	 */
	public void load() {
		for (String str : plugin.getServerData().getTimedVoteCacheKeys()) {
			ConfigurationSection data = plugin.getServerData().getTimedVoteCacheSection(str);
			timeChangeQueue
					.add(new VoteTimeQueue(data.getString("Name"), data.getString("Service"), data.getLong("Time")));
		}
		scheduleQueueProcessing(120, TimeUnit.SECONDS);
	}

	/**
	 * Handles date change event and schedules queue processing.
	 *
	 * @param event the date changed event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void postTimeChange(DateChangedEvent event) {
		scheduleQueueProcessing(5, TimeUnit.SECONDS);
	}

	private void scheduleQueueProcessing(long delay, TimeUnit unit) {
		synchronized (persistenceLock) { if (stopping) return; }
		boolean admitted = VoteTaskAdmission.trySchedule(plugin.getVoteTimer(), this::processQueue, delay, unit);
		if (!admitted) {
			plugin.getLogger().warning("Unable to schedule time-queue processing because vote processing is busy; queued votes were retained.");
			scheduleRetry();
		} else {
			retryAttempts.set(0);
		}
	}

	private void scheduleRetry() {
		if (timeChangeQueue.isEmpty() || !retryPending.compareAndSet(false, true)) return;
		int attempt = Math.min(6, retryAttempts.getAndIncrement());
		long delaySeconds = Math.min(60L, 1L << attempt);
		long delayTicks = delaySeconds * TICKS_PER_SECOND;
		try {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, () -> {
				retryPending.set(false);
				if (!timeChangeQueue.isEmpty()) scheduleQueueProcessing(0, TimeUnit.SECONDS);
			}, delayTicks);
		} catch (RuntimeException rejected) {
			retryPending.set(false);
			plugin.getLogger().warning("Unable to queue a time-queue retry; pending votes remain persisted for recovery.");
		}
	}

	/**
	 * Processes all votes in the queue.
	 */
	public void processQueue() {
		if (!processing.compareAndSet(false, true)) return;
		try {
			synchronized (persistenceLock) {
				if (stopping || dispatchingVote != null) return;
				// Every processing entry point retires the previous disk snapshot
				// before handing any occurrence to event listeners.
				plugin.getServerData().clearTimedVoteCache();
				persistedShutdownVotes = 0;
			}
			while (true) {
				VoteTimeQueue vote;
				synchronized (persistenceLock) {
					if (stopping || dispatchingVote != null) return;
					vote = timeChangeQueue.peek();
				}
				if (vote == null) return;
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(
						plugin.getVoteSiteManager().getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, vote.getService()), true), vote.getName(),
						vote.getService(), true);
				voteEvent.setTime(vote.getTime());
				synchronized (persistenceLock) {
					if (stopping) return;
					if (timeChangeQueue.peek() != vote) continue;
					dispatchingVote = vote;
					dispatchId = UUID.randomUUID();
				}
				try {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				} catch (RuntimeException | Error failure) {
					synchronized (persistenceLock) {
						try {
							retainUncertainDispatch();
						} catch (RuntimeException | Error persistenceFailure) {
							if (failure != persistenceFailure) failure.addSuppressed(persistenceFailure);
						}
					}
					throw failure;
				}
				synchronized (persistenceLock) {
					timeChangeQueue.remove(vote);
					dispatchingVote = null;
					dispatchId = null;
				}
				if (voteEvent.isCancelled()) {
					plugin.debug("Vote cancelled");
					return;
				}
			}
		} finally {
			processing.set(false);
		}
	}

	/** Uncertain event delivery is retained for review, never automatically replayed. */
	private void retainUncertainDispatch() {
		if (dispatchingVote == null || !timeChangeQueue.contains(dispatchingVote)) return;
		plugin.getServerData().addUncertainTimedVote(dispatchId, dispatchingVote);
		timeChangeQueue.remove(dispatchingVote);
		plugin.getLogger().warning("Timed vote event delivery is uncertain; retained in ServerData.yml VotingPlugin.UncertainTimedVotes for manual review.");
	}

	/**
	 * Saves pending votes to server data.
	 */
	public void save() {
		synchronized (persistenceLock) { persistPendingVotes(); }
	}

	/** Seal processing and durably retain late admissions from tasks that ignore interruption. */
	public void saveForShutdown() {
		synchronized (persistenceLock) {
			stopping = true;
			persistPendingVotes();
		}
	}

	/** Remove only a successfully saved prefix; failures retain the same next slot for retry. */
	private void persistPendingVotes() {
		VoteTimeQueue vote;
		while ((vote = timeChangeQueue.peek()) != null) {
			if (vote == dispatchingVote) {
				retainUncertainDispatch();
			} else {
				plugin.getServerData().addTimeVoted(persistedShutdownVotes, vote);
				persistedShutdownVotes++;
				timeChangeQueue.remove(vote);
			}
		}
	}
}
