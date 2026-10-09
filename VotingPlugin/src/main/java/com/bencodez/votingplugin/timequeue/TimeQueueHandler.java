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
        timeChangeQueue.add(queued);
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
		boolean admitted = VoteTaskAdmission.trySchedule(plugin.getVoteTimer(), () -> {
			// Clear only after the bounded executor has admitted the task. If it is
			// rejected, shutdown persistence can still recover the in-memory queue.
			plugin.getServerData().clearTimedVoteCache();
			processQueue();
		}, delay, unit);
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
		while (getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getTimeChangeQueue().remove();
			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSiteManager().getVoteSite(plugin.getVoteSiteManager().getVoteSiteName(true, vote.getService()), true), vote.getName(),
					vote.getService(), true);
			voteEvent.setTime(vote.getTime());
            voteEvent.setLocalOccurrenceId(vote.getLocalOccurrenceId());
            if (vote.getCanonicalOccurrenceTime() != 0L) voteEvent.setCanonicalOccurrenceTime(vote.getCanonicalOccurrenceTime());
			plugin.getServer().getPluginManager().callEvent(voteEvent);

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
			int num = 0;
			for (VoteTimeQueue vote : timeChangeQueue) {
				plugin.getServerData().addTimeVoted(num, vote);
				num++;
			}
		}
		timeChangeQueue.clear();
	}
}
