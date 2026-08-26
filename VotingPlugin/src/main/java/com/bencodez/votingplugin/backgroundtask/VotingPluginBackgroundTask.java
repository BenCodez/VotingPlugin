package com.bencodez.votingplugin.backgroundtask;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.simpleapi.skull.SkullCache;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

/** Owns VotingPlugin's periodic background data refresh task and its state. */
public final class VotingPluginBackgroundTask {

	private final VotingPluginMain plugin;
	private volatile boolean requested;
	private volatile boolean running;
	private volatile long lastRunTimeSeconds = -1;

	public VotingPluginBackgroundTask(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public synchronized void run() {
		if (!(requested || plugin.getConfigFile().isAlwaysUpdate())) {
			return;
		}
		if (!plugin.isEnabled() || running) {
			return;
		}
		if (plugin.getConfigFile().isUpdateWithPlayersOnlineOnly() && Bukkit.getOnlinePlayers().isEmpty()) {
			return;
		}

		running = true;
		requested = false;
		try {
			runRefresh();
		} finally {
			running = false;
		}
	}

	private void runRefresh() {
		synchronized (plugin) {
			try {
				if (!plugin.isEnabled()) {
					return;
				}

				plugin.getUserManager().getDataManager().clearCacheBasic();
				SkullCache.flushWeek();
				plugin.debug("Starting background task, current cached users: "
						+ plugin.getUserManager().getDataManager().getUserDataCache().keySet().size());

				boolean extraBackgroundUpdate = plugin.getConfigFile().isExtraBackgroundUpdate();
				long startTime = System.currentTimeMillis();
				LinkedHashMap<TopVoterPlayer, HashMap<VoteSite, LocalDateTime>> voteToday = new LinkedHashMap<>();
				LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> tempTopVoter = new LinkedHashMap<>();

				ArrayList<TopVoter> topVotersToCheck = new ArrayList<>();
				for (TopVoter top : TopVoter.values()) {
					if (plugin.getConfigFile().getLoadTopVoter(top)) {
						topVotersToCheck.add(top);
						tempTopVoter.put(top, new LinkedHashMap<>());
					}
				}

				boolean ignorePermission = plugin.getConfigFile().isTopVoterIgnorePermission();
				ArrayList<String> blackList = plugin.getConfigFile().getBlackList();
				ZoneId zone = ZoneId.systemDefault();
				LocalDate today = LocalDate.now(zone);
				long startOfDayMs = today.atStartOfDay(zone).toInstant().toEpochMilli();
				long startOfNextDayMs = today.plusDays(1).atStartOfDay(zone).toInstant().toEpochMilli();
				long afterSetup = System.currentTimeMillis();

				plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
					if (!plugin.isEnabled()) {
						return;
					}

					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
					user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
					user.updateTempCacheWithColumns(columns);
					try {
						if (!user.isBanned() && !blackList.contains(user.getPlayerName())) {
							if (!ignorePermission || !user.isTopVoterIgnore()) {
								TopVoterPlayer player = user.getTopVoterPlayer();
								for (TopVoter top : topVotersToCheck) {
									int total = user.getTotal(top);
									if (total > 0) {
										tempTopVoter.get(top).put(player, total);
									}
								}
							}

							HashMap<VoteSite, LocalDateTime> times = null;
							for (Entry<VoteSite, Long> entry : user.getLastVotes().entrySet()) {
								VoteSite site = entry.getKey();
								if (!site.isEnabled() || site.isHidden()) {
									continue;
								}
								long time = entry.getValue();
								if (time >= startOfDayMs && time < startOfNextDayMs) {
									if (times == null) {
										times = new HashMap<>();
									}
									times.put(site, LocalDateTime.ofInstant(Instant.ofEpochMilli(time), zone));
								}
							}
							if (times != null && !times.isEmpty()) {
								voteToday.put(user.getTopVoterPlayer(), times);
							}
						}

						if (extraBackgroundUpdate && user.isOnline()) {
							user.offVote();
						}
						if (!plugin.getPlaceholders().getCacheLevel().onlineOnly() || user.isOnline()) {
							plugin.getPlaceholders().onUpdate(user, false);
						}
					} finally {
						user.clearTempCache();
					}
				}, count -> {
					long elapsed = (System.currentTimeMillis() - afterSetup) / 1000;
					plugin.debug("Finished getting player data in " + elapsed + " seconds, " + count + " users, "
							+ plugin.getStorageType());
				});

				plugin.getTopVoterHandler().updateTopVoters(tempTopVoter);
				plugin.getPlaceholders().onUpdate();
				plugin.setVoteToday(voteToday);
				plugin.getServerData().updateValues();
				plugin.getSigns().updateSigns();

				if (plugin.getConfigFile().isDiscordSRVEnabled() && plugin.getDiscordHandler() != null) {
					for (TopVoter top : TopVoter.values()) {
						if (!plugin.getConfigFile().isDiscordSRVTopVoterNewMessageOnUpdate(top)) {
							plugin.getDiscordHandler().updateTopVoterMessageId(top);
						}
					}
				}

				plugin.getUserManager().getDataManager().clearNonNeededCachedUsers();
				plugin.extraDebug("Current cached users: "
						+ plugin.getUserManager().getDataManager().getUserDataCache().keySet().size());

				lastRunTimeSeconds = (System.currentTimeMillis() - startTime) / 1000;
				plugin.debug("Background task finished. Total time: " + lastRunTimeSeconds + " seconds");
			} catch (Exception exception) {
				if (plugin.isEnabled()) {
					plugin.getLogger().info("Looks like something went wrong");
				}
				exception.printStackTrace();
			}
		}
	}

	public boolean isRequested() {
		return requested;
	}

	public void setRequested(boolean requested) {
		this.requested = requested;
	}

	public boolean isRunning() {
		return running;
	}

	public long getLastRunTimeSeconds() {
		return lastRunTimeSeconds;
	}
}
