package com.bencodez.votingplugin.timequeue;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

import lombok.Getter;

public class TimeQueueHandler implements Listener {
	@Getter
	private Queue<VoteTimeQueue> timeChangeQueue = new ConcurrentLinkedQueue<VoteTimeQueue>();

	private VotingPluginMain plugin;

	public TimeQueueHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		load();
	}

	public void processQueue() {
		while (getTimeChangeQueue().size() > 0) {
			VoteTimeQueue vote = getTimeChangeQueue().remove();
			PlayerVoteEvent voteEvent = new PlayerVoteEvent(
					plugin.getVoteSite(plugin.getVoteSiteName(true, vote.getService()), true), vote.getName(),
					vote.getService(), true);
			voteEvent.setTime(voteEvent.getTime());
			plugin.getServer().getPluginManager().callEvent(voteEvent);

			if (voteEvent.isCancelled()) {
				plugin.debug("Vote cancelled");
				return;
			}
		}
	}

	public void addVote(String voteUsername, String voteSiteName) {
		timeChangeQueue.add(new VoteTimeQueue(voteUsername, voteSiteName,
				LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli()));
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void postTimeChange(DateChangedEvent event) {
		plugin.getTimer().schedule(new Runnable() {

			@Override
			public void run() {
				plugin.getVoteQueue().add(new Runnable() {

					@Override
					public void run() {
						processQueue();
					}
				});

			}
		}, 5, TimeUnit.SECONDS);
	}

	public void load() {
		for (String str : plugin.getServerData().getTimedVoteCacheKeys()) {
			ConfigurationSection data = plugin.getServerData().getTimedVoteCacheSection(str);
			timeChangeQueue
					.add(new VoteTimeQueue(data.getString("Name"), data.getString("Service"), data.getLong("Time")));
		}
		plugin.getServerData().clearTimedVoteCache();
		plugin.getTimer().schedule(new Runnable() {

			@Override
			public void run() {
				plugin.getVoteQueue().add(new Runnable() {

					@Override
					public void run() {
						processQueue();
					}
				});

			}
		}, 120, TimeUnit.SECONDS);
	}

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
