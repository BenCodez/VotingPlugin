package com.bencodez.votingplugin.timequeue;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

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
	private volatile boolean stopping;
	private final AtomicReference<FutureTask<Void>> shutdownSave = new AtomicReference<>();
	private final AtomicBoolean processing = new AtomicBoolean();
	private VoteTimeQueue dispatchingVote;
	private UUID dispatchId;
	private boolean dispatchEnded;
	private boolean dispatchStarted;
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
				if (stopping) return;
				if (dispatchingVote != null) {
					if (!dispatchEnded) return;
					retainUncertainDispatch();
					clearDispatch();
				}
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
					if (vote.getVoteId() == null) vote.setVoteId(UUID.randomUUID());
					dispatchId = vote.getVoteId();
					dispatchEnded = false;
					try {
						plugin.getServerData().prepareTimedVoteDispatch(dispatchId, vote,
								timeChangeQueue.stream().filter(pending -> pending != vote).toList());
					} catch (RuntimeException | Error failure) {
						clearDispatch();
						throw failure;
					}
					// A preparation resumed after stop is still eligible for normal recovery.
					if (stopping) return;
					dispatchStarted = true;
				}
				try {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				} catch (RuntimeException | Error failure) {
					synchronized (persistenceLock) {
						dispatchEnded = true;
						try {
							retainUncertainDispatch();
							clearDispatch();
							if (!stopping) checkpointPendingVotes();
						} catch (RuntimeException | Error persistenceFailure) {
							if (failure != persistenceFailure) failure.addSuppressed(persistenceFailure);
						}
					}
					throw failure;
				}
				synchronized (persistenceLock) {
					dispatchEnded = true;
					if (!stopping) plugin.getServerData().completeTimedVoteDispatch(dispatchId);
					timeChangeQueue.remove(vote);
					clearDispatch();
					if (voteEvent.isCancelled() && !stopping) checkpointPendingVotes();
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

	private void clearDispatch() {
		dispatchingVote = null;
		dispatchId = null;
		dispatchEnded = false;
		dispatchStarted = false;
	}

	private void checkpointPendingVotes() {
		plugin.getServerData().replaceTimedVoteCache(
				timeChangeQueue.stream().filter(vote -> vote != dispatchingVote).toList());
		persistedShutdownVotes = 0;
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
		if (stopping) {
			saveForShutdown();
			return;
		}
		synchronized (persistenceLock) {
			if (stopping) persistPendingVotes();
			else checkpointPendingVotes();
		}
	}

	/** Seal processing and retain one persistence owner without an unbounded lifecycle wait. */
	public void saveForShutdown() {
		stopping = true;
		FutureTask<Void> save;
		while (true) {
			save = shutdownSave.get();
			if (save != null) {
				if (!save.isDone()) break;
				shutdownSave.compareAndSet(save, null);
				continue;
			}
			FutureTask<Void> candidate = new FutureTask<Void>(() -> {
				try {
					synchronized (persistenceLock) { persistPendingVotes(); }
				} catch (RuntimeException | Error failure) {
					try {
						plugin.getLogger().warning("Timed vote shutdown persistence failed; pending votes remain owned for retry. Recovery is not confirmed.");
					} catch (RuntimeException | Error ignored) { }
					throw failure;
				}
				return null;
			}) {
				@Override protected void done() {
					// Late admission failures must be retryable after this owner completes.
					shutdownSave.compareAndSet(this, null);
				}
			};
			FutureTask<Void> existing = shutdownSave.compareAndExchange(null, candidate);
			if (existing == null) {
				save = candidate;
				try {
					Thread worker = new Thread(candidate, "VotingPlugin-timed-vote-shutdown");
					worker.setDaemon(true);
					worker.start();
				}
				catch (RuntimeException | Error failure) {
					candidate.cancel(false);
					shutdownSave.compareAndSet(candidate, null);
					throw failure;
				}
				break;
			}
		}
		try {
			save.get(1, TimeUnit.SECONDS);
			shutdownSave.compareAndSet(save, null);
		} catch (TimeoutException pending) {
			plugin.getLogger().warning("Timed vote shutdown persistence is still pending; its worker retains ownership. Recovery is not yet confirmed.");
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			plugin.getLogger().warning("Timed vote shutdown persistence wait was interrupted; its worker retains ownership.");
		} catch (ExecutionException failure) {
			shutdownSave.compareAndSet(save, null);
			Throwable cause = failure.getCause();
			if (cause instanceof Error error) throw error;
			if (cause instanceof RuntimeException runtime) throw runtime;
			throw new IllegalStateException("Timed vote shutdown persistence failed", cause);
		}
	}

	/** Remove only a successfully saved prefix; failures retain the same next slot for retry. */
	private void persistPendingVotes() {
		VoteTimeQueue vote;
		while ((vote = timeChangeQueue.peek()) != null) {
			if (vote == dispatchingVote) {
				if (dispatchStarted) retainUncertainDispatch();
				else {
					var pending = timeChangeQueue.stream().toList();
					plugin.getServerData().abortTimedVoteDispatch(dispatchId, pending);
					timeChangeQueue.removeAll(pending);
					persistedShutdownVotes = pending.size();
					clearDispatch();
				}
			} else {
				plugin.getServerData().addTimeVoted(persistedShutdownVotes, vote);
				persistedShutdownVotes++;
				timeChangeQueue.remove(vote);
			}
		}
	}
}
