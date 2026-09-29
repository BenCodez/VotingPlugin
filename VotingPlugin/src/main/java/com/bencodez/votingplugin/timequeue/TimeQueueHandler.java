package com.bencodez.votingplugin.timequeue;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Queue;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;
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
	static final int MAX_QUEUED_VOTES = 4096;
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<>();

	private VotingPluginMain plugin;
	private final AtomicBoolean retryPending = new AtomicBoolean();
	private final AtomicInteger retryAttempts = new AtomicInteger();
	private final Set<VoteTimeQueue> completedAwaitingPersistence =
			Collections.newSetFromMap(new IdentityHashMap<>());

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
	public synchronized void addVote(String voteUsername, String voteSiteName) {
		if (timeChangeQueue.size() >= MAX_QUEUED_VOTES) {
			plugin.getLogger().severe("Time-change vote queue is full; rejecting vote instead of expanding durable storage");
			return;
		}
		timeChangeQueue.add(new VoteTimeQueue(voteUsername, voteSiteName,
				LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
		persistQueueSnapshot();
	}

	/**
	 * Loads cached votes from server data and schedules queue processing.
	 */
	public void load() {
		boolean truncated = false;
		for (String str : plugin.getServerData().getTimedVoteCacheKeys()) {
			if (timeChangeQueue.size() >= MAX_QUEUED_VOTES) {
				truncated = true;
				break;
			}
			ConfigurationSection data = plugin.getServerData().getTimedVoteCacheSection(str);
			timeChangeQueue
					.add(new VoteTimeQueue(data.getString("Name"), data.getString("Service"), data.getLong("Time")));
		}
		if (truncated) {
			plugin.getLogger().severe("Timed vote recovery exceeded the bounded queue; excess persisted votes were not loaded");
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
	public synchronized void processQueue() {
		while (true) {
			VoteTimeQueue vote = getTimeChangeQueue().peek();
			if (vote == null) return;
			if (!completedAwaitingPersistence.contains(vote)) {
				PlayerVoteEvent voteEvent = new PlayerVoteEvent(
						plugin.getVoteSiteManager().getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, vote.getService()), true), vote.getName(),
						vote.getService(), true);
				voteEvent.setTime(vote.getTime());
				try {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				} catch (RuntimeException failure) {
					plugin.getLogger().warning("Unable to process queued time-change vote; retaining it for retry");
					plugin.debug(failure);
					scheduleRetry();
					return;
				}
				completedAwaitingPersistence.add(vote);
				if (voteEvent.isCancelled()) plugin.debug("Vote cancelled");
			}

			if (!persistWithout(vote)) {
				scheduleRetry();
				return;
			}
			getTimeChangeQueue().remove(vote);
			completedAwaitingPersistence.remove(vote);
		}
	}

	/**
	 * Saves pending votes to server data.
	 */
	public synchronized void save() {
		persistQueueSnapshot();
		timeChangeQueue.clear();
	}

	private void persistQueueSnapshot() {
		plugin.getServerData().replaceTimedVoteCache(new ArrayList<>(timeChangeQueue));
	}

	private boolean persistWithout(VoteTimeQueue completed) {
		ArrayList<VoteTimeQueue> remaining = new ArrayList<>(timeChangeQueue);
		remaining.remove(completed);
		try {
			plugin.getServerData().replaceTimedVoteCache(remaining);
			return true;
		} catch (RuntimeException failure) {
			plugin.getLogger().warning("Unable to persist completed time-change vote retirement; retrying storage only");
			plugin.debug(failure);
			return false;
		}
	}
}
