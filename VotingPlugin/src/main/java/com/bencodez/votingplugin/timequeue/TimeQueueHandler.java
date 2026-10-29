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
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	private VotingPluginMain plugin;
	private final Object persistenceLock = new Object();
	private volatile boolean stopping;
	private final AtomicReference<FutureTask<Void>> shutdownSave = new AtomicReference<>();
	// A failed task is retryable, but cannot release its unsaved suffix to a successor.
	private final AtomicReference<ShutdownPublication> shutdownPublicationOwner = new AtomicReference<>();
	private record ShutdownPublication(com.bencodez.votingplugin.util.ServerDataPublicationFence.Lease lease,
			java.util.concurrent.ScheduledExecutorService producers,
			java.util.concurrent.ScheduledExecutorService[] coreWriters) { }
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
	public void addVote(String voteUsername, String voteSiteName) { addVote(voteUsername, voteSiteName, 0L); }

    public void addVote(String voteUsername, String voteSiteName, long occurredAt) {
        addVote(voteUsername, voteSiteName, occurredAt, UUID.randomUUID());
    }

    public void addVote(String voteUsername, String voteSiteName, long occurredAt, UUID localOccurrenceId) {
        java.util.Objects.requireNonNull(localOccurrenceId, "localOccurrenceId");
        VoteTimeQueue queued = new VoteTimeQueue(voteUsername, voteSiteName,
                LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
        queued.setLocalOccurrenceId(localOccurrenceId);
        if (occurredAt > 0) queued.setCanonicalOccurrenceTime(occurredAt);
        synchronized (persistenceLock) {
            timeChangeQueue.add(queued);
            if (stopping) persistPendingVotes();
        }
    }

	/**
	 * Loads cached votes from server data and schedules queue processing.
	 */
	public void load() {
		for (String str : plugin.getServerData().getTimedVoteCacheKeys()) {
			ConfigurationSection data = plugin.getServerData().getTimedVoteCacheSection(str);
            VoteTimeQueue queued = new VoteTimeQueue(data.getString("Name"), data.getString("Service"), data.getLong("Time"));
            UUID localOccurrenceId;
            if (!data.contains("LocalOccurrenceId")) {
                // The existing ServerData saver can defer publication. Derive only
                // legacy migration IDs from their persisted row identity so a crash
                // before that save completes cannot reroll the same legacy row.
                String identity = "legacy-local-timed-vote\u0000" + str + "\u0000" + queued.getName()
                        + "\u0000" + queued.getService() + "\u0000" + queued.getTime();
                localOccurrenceId = UUID.nameUUIDFromBytes(identity.getBytes(java.nio.charset.StandardCharsets.UTF_8));
                plugin.getServerData().setTimedVoteLocalOccurrenceId(str, localOccurrenceId);
            } else {
                String rawId = data.getString("LocalOccurrenceId");
                try {
                    localOccurrenceId = UUID.fromString(rawId);
                    if (!localOccurrenceId.toString().equalsIgnoreCase(rawId)) throw new IllegalArgumentException();
                } catch (IllegalArgumentException | NullPointerException malformed) {
                    plugin.getLogger().warning("Invalid local occurrence ID in timed vote cache; row withheld");
                    continue;
                }
            }
            queued.setLocalOccurrenceId(localOccurrenceId);
            if (data.contains("CanonicalOccurrenceTime")) queued.setCanonicalOccurrenceTime(data.getLong("CanonicalOccurrenceTime", -1L));
            timeChangeQueue.add(queued);
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
		}
	}

	private void scheduleRetry() {
		if (timeChangeQueue.isEmpty() || !retryPending.compareAndSet(false, true)) return;
		int attempt = Math.min(6, retryAttempts.getAndIncrement());
		long delaySeconds = Math.min(60L, 1L << attempt);
		try {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin, () -> {
				retryPending.set(false);
				if (!timeChangeQueue.isEmpty()) scheduleQueueProcessing(0, TimeUnit.SECONDS);
			}, delaySeconds);
		} catch (RuntimeException rejected) {
			retryPending.set(false);
			plugin.getLogger().warning("Unable to queue a time-queue retry; pending votes remain owned for recovery.");
		}
	}

	/**
	 * Processes all votes in the queue.
	 */
	public void processQueue() {
		if (!processing.compareAndSet(false, true)) return;
		boolean retry = false;
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
				voteEvent.setLocalOccurrenceId(vote.getLocalOccurrenceId());
				if (vote.getCanonicalOccurrenceTime() != 0L) voteEvent.setCanonicalOccurrenceTime(vote.getCanonicalOccurrenceTime());
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
					continue;
				}
			}
		} catch (RuntimeException | Error failure) {
			retry = !stopping;
			throw failure;
		} finally {
			processing.set(false);
			if (retry) scheduleRetry();
			else retryAttempts.set(0);
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
					var publication = java.util.Objects.requireNonNull(shutdownPublicationOwner.get());
					try { synchronized (persistenceLock) { persistPendingVotes(); } }
					finally {
						// Publication failure does not terminate previously admitted producers.
						awaitRetiringProducers(publication.producers());
						if (publication.coreWriters() != null)
							for (var writer : publication.coreWriters()) awaitWriterTermination(writer);
					}
					synchronized (persistenceLock) { persistPendingVotes(); }
					if (publication.lease() != null) publication.lease().close();
					shutdownPublicationOwner.compareAndSet(publication, null);
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
					if (shutdownPublicationOwner.get() == null) {
						// Unload can clear these fields even while their original work survives.
						// Keep the snapshot with its lease through every rejected or failed attempt.
						var producers = plugin.getVoteTimer();
						var coreWriters = plugin.getServerDataWriterExecutors();
						var retainedWriters = coreWriters == null ? null : coreWriters.clone();
						var fence = plugin.getServerDataPublicationFence();
						shutdownPublicationOwner.set(new ShutdownPublication(
								fence == null ? null : fence.acquire(), producers, retainedWriters));
					}
					startShutdownWorker(candidate);
				}
				catch (RuntimeException | Error failure) {
					// Even a rejected worker retains the queue owner for an explicit retry.
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

	/** Kept separate so worker-admission failure can be exercised without replacing persistence. */
	protected void startShutdownWorker(Runnable task) {
		Thread worker = new Thread(task, "VotingPlugin-timed-vote-shutdown");
		worker.setDaemon(true);
		worker.start();
	}

	private void awaitRetiringProducers(java.util.concurrent.ScheduledExecutorService producers) {
		if (producers == null || !producers.isShutdown()) return;
		awaitWriterTermination(producers);
	}

	private void awaitWriterTermination(java.util.concurrent.ScheduledExecutorService producers) {
		if (producers == null) return;
		boolean interrupted = false;
		try {
			while (!producers.isTerminated()) {
				try { producers.awaitTermination(100, TimeUnit.MILLISECONDS); }
				catch (InterruptedException ignored) { interrupted = true; }
			}
		} finally { if (interrupted) Thread.currentThread().interrupt(); }
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
