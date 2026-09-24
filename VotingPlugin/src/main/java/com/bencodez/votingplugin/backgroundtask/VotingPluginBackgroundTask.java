package com.bencodez.votingplugin.backgroundtask;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

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
	private final long snapshotWaitMillis;
	private volatile boolean requested;
	private volatile boolean running;
	private volatile long lastRunTimeSeconds = -1;

	public VotingPluginBackgroundTask(VotingPluginMain plugin) {
		this(plugin, 5_000L);
	}

	VotingPluginBackgroundTask(VotingPluginMain plugin, long snapshotWaitMillis) {
		this.plugin = plugin;
		this.snapshotWaitMillis = snapshotWaitMillis;
	}

	public void run() {
		CompletableFuture<Void> completion = new CompletableFuture<>();
		CompletableFuture<Void> snapshotStarted = new CompletableFuture<>();
		AtomicBoolean snapshotPending = new AtomicBoolean(true);
		synchronized (this) {
			if (!(requested || plugin.getConfigFile().isAlwaysUpdate()) || !plugin.isEnabled() || running) return;
			running = true;
		}
		try {
			plugin.captureOnlineTopVoterIgnore(online -> {
				if (!snapshotPending.compareAndSet(true, false)) return;
				snapshotStarted.complete(null);
				if (plugin.getConfigFile().isUpdateWithPlayersOnlineOnly() && online.isEmpty()) {
					finishRun(completion);
					return;
				}
				synchronized (VotingPluginBackgroundTask.this) { requested = false; }
				try {
					plugin.getUserManager().getDataManager().getTimer().execute(() -> {
						try {
							runRefresh(online);
							completion.complete(null);
						} catch (Throwable failure) {
							plugin.debug(failure);
							setRequested(true);
							completion.complete(null);
						} finally { finishRun(completion); }
					});
				} catch (RuntimeException failure) {
					plugin.debug(failure);
					setRequested(true);
					completion.complete(null);
					finishRun(completion);
				}
			}, () -> {
				setRequested(true);
				snapshotPending.set(false);
				snapshotStarted.complete(null);
				finishRun(completion);
			});
		} catch (RuntimeException failure) {
			plugin.debug(failure);
			setRequested(true);
			snapshotPending.set(false);
			snapshotStarted.complete(null);
			completion.complete(null);
			finishRun(completion);
		}
		if (!isPlatformOwnedThread()) {
			try {
				snapshotStarted.get(snapshotWaitMillis, TimeUnit.MILLISECONDS);
			} catch (TimeoutException failure) {
				if (snapshotPending.compareAndSet(true, false)) {
					setRequested(true);
					finishRun(completion);
					return;
				}
			} catch (InterruptedException failure) {
				Thread.currentThread().interrupt();
				if (snapshotPending.compareAndSet(true, false)) {
					setRequested(true);
					finishRun(completion);
					return;
				}
			} catch (java.util.concurrent.ExecutionException impossible) {
				throw new IllegalStateException(impossible);
			}
			completion.join();
		}
	}

	private synchronized void finishRun(CompletableFuture<Void> completion) {
		running = false;
		if (!completion.isDone()) completion.complete(null);
	}

	private boolean isPlatformOwnedThread() {
		if (Bukkit.getServer() == null) return false;
		try { if (Bukkit.isPrimaryThread()) return true; } catch (RuntimeException ignored) { }
		try {
			java.lang.reflect.Method method = Bukkit.getServer().getClass().getMethod("isGlobalTickThread");
			if (Boolean.TRUE.equals(method.invoke(Bukkit.getServer()))) return true;
		} catch (ReflectiveOperationException | RuntimeException ignored) { }
		try {
			Class<?> tickThread = Class.forName("ca.spottedleaf.moonrise.common.util.TickThread");
			if (Boolean.TRUE.equals(tickThread.getMethod("isTickThread").invoke(null))) return true;
		} catch (ReflectiveOperationException | LinkageError | RuntimeException ignored) { }
		return false;
	}

	private void runRefresh(Map<UUID, Boolean> onlineUsers) {
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

						Boolean topVoterIgnore = onlineUsers.get(uuid);
						boolean online = topVoterIgnore != null;
						if (extraBackgroundUpdate && online) {
							user.offVoteWithCapturedTopVoterIgnore(topVoterIgnore.booleanValue());
							user.checkOfflineRewards();
						}
						if (!plugin.getPlaceholders().getCacheLevel().onlineOnly() || online) {
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
				plugin.getPlaceholders().checkNonCachedPlaceholders();
				for (UUID onlineUuid : onlineUsers.keySet()) {
					VotingPluginUser onlineUser = plugin.getVotingPluginUserManager().getVotingPluginUser(onlineUuid, false);
					if (onlineUser != null) plugin.getPlaceholders().onUpdate(onlineUser, true);
				}
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
