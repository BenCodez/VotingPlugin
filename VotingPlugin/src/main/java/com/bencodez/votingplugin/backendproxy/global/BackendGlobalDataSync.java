package com.bencodez.votingplugin.backendproxy.global;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalMySQL;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueBoolean;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfigSpigot;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import lombok.Getter;

/**
 * Owns backend global-data polling and proxy-driven time-change processing.
 */
public class BackendGlobalDataSync {
	private static final long CLOSE_GRACE_SECONDS = 5L;
	private static final Map<VotingPluginMain, Set<TimeType>> ACTIVE_TIME_CHANGES = new WeakHashMap<>();

	private final VotingPluginMain plugin;
	private final Object senderLock = new Object();
	private Consumer<JsonEnvelope> sender;
	private BackendGlobalDataSync completionReplacement;
	private boolean senderHandedOff;
	private final AtomicBoolean forceUpdateInProgress = new AtomicBoolean(false);
	private final Set<TimeType> timeChangesInProgress = ConcurrentHashMap.newKeySet();
	private final Set<TimeType> activeTimeChangeAdmissions;
	private final Object timeChangeLifecycleLock = new Object();
	private final Object timeChangePersistenceLock = new Object();
	private final Map<GlobalDataHandler, Integer> activeTimeChangesByHandler = new HashMap<>();
	private final Set<GlobalDataHandler> retiredOwnedHandlers = new HashSet<>();

	@Getter
	private GlobalDataHandler globalDataHandler;
	private boolean ownsGlobalMysql;
	@Getter
	private ScheduledExecutorService timer;

	public BackendGlobalDataSync(VotingPluginMain plugin, Consumer<JsonEnvelope> sender) {
		this.plugin = plugin;
		this.sender = sender;
		// Runtime replacements use the same plugin instance. Share admission across
		// those generations so a replacement cannot enqueue the period transition
		// that its predecessor is still processing. A process restart intentionally
		// gets a fresh set so the persisted period flag remains eligible for recovery.
		synchronized (ACTIVE_TIME_CHANGES) {
			activeTimeChangeAdmissions = ACTIVE_TIME_CHANGES.computeIfAbsent(plugin,
					ignored -> ConcurrentHashMap.newKeySet());
		}
	}

	public void checkGlobalData() {
		if (globalDataHandler == null) {
			return;
		}
		HashMap<String, DataValue> data = globalDataHandler.getExact(plugin.getBungeeSettings().getServer());

		if (data.containsKey("ForceUpdate") && checkGlobalDataTimeValue(data.get("ForceUpdate"))
				&& forceUpdateInProgress.compareAndSet(false, true)) {
			String serverName = plugin.getBungeeSettings().getServer();
			try {
				if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
					plugin.getMysql().clearCacheBasic();
				}
				plugin.getBukkitScheduler().executeOrScheduleSync(plugin, () -> {
					try {
						plugin.getUserManager().getDataManager().clearCacheAsyncCompletion().whenComplete((ignored, failure) -> {
							if (failure != null) {
								forceUpdateInProgress.set(false);
								plugin.debug(failure);
								return;
							}
							try {
								plugin.getBukkitScheduler().runTaskAsynchronously(plugin, () -> {
									try {
										plugin.setUpdate(true);
										plugin.update();
										clearForceUpdateFlag(serverName);
									} catch (RuntimeException updateFailure) {
										forceUpdateInProgress.set(false);
										plugin.debug(updateFailure);
									}
								});
							} catch (RuntimeException schedulingFailure) {
								forceUpdateInProgress.set(false);
								plugin.debug(schedulingFailure);
							}
						});
					} catch (RuntimeException failure) {
						forceUpdateInProgress.set(false);
						plugin.debug(failure);
					}
				});
			} catch (RuntimeException failure) {
				forceUpdateInProgress.set(false);
				plugin.debug(failure);
			}
		}

		checkGlobalDataTime(TimeType.MONTH, data);
		checkGlobalDataTime(TimeType.WEEK, data);
		checkGlobalDataTime(TimeType.DAY, data);
	}

	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		if (!data.containsKey(type.toString()) || !checkGlobalDataTimeValue(data.get(type.toString()))) {
			return false;
		}

		long lastUpdated = Long.valueOf(data.get("LastUpdated").getString()).longValue();
		plugin.debug("LastUpdated: " + lastUpdated);
		if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastUpdated > 1000 * 60 * 60 * 2) {
			plugin.getLogger().warning("Ignoring bungee time change since it was more than 2 hours ago");
			globalDataHandler.setBoolean(plugin.getBungeeSettings().getServer(), type.toString(), false);
			return false;
		}
		String serverName = plugin.getBungeeSettings().getServer();
		GlobalDataHandler transitionHandler = admitTimeChange(type, serverName);
		if (transitionHandler == null) return false;
		plugin.debug("Detected time change from bungee: " + type.toString());
		ScheduledExecutorService transitionExecutor = plugin.getTimeChecker().getTimer();
		if (transitionExecutor == null) {
			releaseTimeChange(transitionHandler, type, serverName, false);
			plugin.debug("Unable to process proxy time change before the time checker is ready");
			return false;
		}
		try {
			transitionExecutor.execute(() -> {
				try {
					plugin.getTimeChecker().forceChanged(type, false, true, true);
				} catch (RuntimeException failure) {
					releaseTimeChange(transitionHandler, type, serverName, false);
					plugin.debug(failure);
					return;
				}
				try {
					finishTimeChange(transitionHandler, type, serverName);
				} catch (RuntimeException failure) {
					// finishTimeChange releases the pinned handler in its finally block.
					plugin.debug(failure);
				}
			});
		} catch (RejectedExecutionException failure) {
			releaseTimeChange(transitionHandler, type, serverName, false);
			plugin.debug(failure);
			return false;
		}
		return true;
	}

	private GlobalDataHandler admitTimeChange(TimeType type, String serverName) {
		GlobalDataHandler handler;
		synchronized (timeChangeLifecycleLock) {
			handler = globalDataHandler;
			if (handler == null || !activeTimeChangeAdmissions.add(type)) return null;
			if (!timeChangesInProgress.add(type)) {
				activeTimeChangeAdmissions.remove(type);
				return null;
			}
			activeTimeChangesByHandler.merge(handler, 1, Integer::sum);
		}
		try {
			synchronized (timeChangePersistenceLock) {
				handler.setBoolean(serverName, "Processing", true);
			}
			return handler;
		} catch (RuntimeException failure) {
			try {
				releaseTimeChange(handler, type, serverName, false);
			} catch (RuntimeException cleanupFailure) {
				failure.addSuppressed(cleanupFailure);
			}
			throw failure;
		}
	}

	private void finishTimeChange(GlobalDataHandler handler, TimeType type, String serverName) {
		boolean completed = false;
		try {
			handler.setBoolean(serverName, type.toString(), false);
			JsonEnvelope.Builder builder = JsonEnvelope.builder("TimeChangeFinished")
					.schema(VotingPluginWire.SCHEMA_VERSION);
			builder.put("server", serverName);
			sendTimeChangeFinished(builder.build());
			completed = true;
		} finally {
			releaseTimeChange(handler, type, serverName, completed);
		}
	}

	private void releaseTimeChange(GlobalDataHandler handler, TimeType type, String serverName,
			boolean completed) {
		GlobalMySQL closeAfterRelease = null;
		try {
			synchronized (timeChangeLifecycleLock) {
				timeChangesInProgress.remove(type);
			}
			synchronized (timeChangePersistenceLock) {
				boolean lastTimeChange;
				synchronized (timeChangeLifecycleLock) {
					lastTimeChange = timeChangesInProgress.isEmpty();
				}
				if (lastTimeChange) {
					HashMap<String, DataValue> dataToSet = new HashMap<>();
					if (completed) dataToSet.put("FinishedProcessing", new DataValueBoolean(true));
					dataToSet.put("Processing", new DataValueBoolean(false));
					handler.setData(serverName, dataToSet);
				}
			}
		} finally {
			activeTimeChangeAdmissions.remove(type);
			synchronized (timeChangeLifecycleLock) {
				closeAfterRelease = releaseHandlerReferenceLocked(handler);
			}
			if (closeAfterRelease != null) closeAfterRelease.close();
			synchronized (timeChangeLifecycleLock) {
				timeChangeLifecycleLock.notifyAll();
			}
		}
	}

	private void sendTimeChangeFinished(JsonEnvelope envelope) {
		Consumer<JsonEnvelope> completionSender;
		BackendGlobalDataSync replacement;
		synchronized (senderLock) {
			replacement = completionReplacement;
			if (replacement == null && sender == null) {
				throw new RejectedExecutionException("Backend proxy transport retired before time-change completion");
			}
			completionSender = sender;
		}
		if (replacement != null) {
			replacement.sendTimeChangeFinished(envelope);
			return;
		}
		try {
			completionSender.accept(envelope);
		} catch (RuntimeException sendFailure) {
			synchronized (senderLock) {
				replacement = completionReplacement;
			}
			if (replacement == null) throw sendFailure;
			try {
				replacement.sendTimeChangeFinished(envelope);
			} catch (RuntimeException replacementFailure) {
				replacementFailure.addSuppressed(sendFailure);
				throw replacementFailure;
			}
		}
	}

	/** Routes an admitted transition's final notification through the published replacement. */
	public void handoffCompletionSender(BackendGlobalDataSync replacement) {
		synchronized (senderLock) {
			completionReplacement = java.util.Objects.requireNonNull(replacement, "replacement");
			senderHandedOff = true;
		}
	}

	private GlobalMySQL releaseHandlerReferenceLocked(GlobalDataHandler handler) {
		Integer references = activeTimeChangesByHandler.get(handler);
		if (references == null || references <= 1) {
			activeTimeChangesByHandler.remove(handler);
			return retiredOwnedHandlers.remove(handler) ? handler.getGlobalMysql() : null;
		}
		activeTimeChangesByHandler.put(handler, references - 1);
		return null;
	}

	public boolean checkGlobalDataTimeValue(DataValue data) {
		if (data.isBoolean()) {
			return data.getBoolean();
		}
		return Boolean.valueOf(data.getString());
	}

	private void clearForceUpdateFlag(String serverName) {
		try {
			globalDataHandler.setBoolean(serverName, "ForceUpdate", false);
		} finally {
			forceUpdateInProgress.set(false);
		}
	}

	public void load() {
		if (!plugin.getBungeeSettings().isGloblalDataEnabled()) {
			return;
		}

		long retirementDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(CLOSE_GRACE_SECONDS);
		shutdownTimer(retirementDeadline);
		timer = Executors.newScheduledThreadPool(1);
		timer.scheduleWithFixedDelay(this::checkGlobalData, 60, 10, TimeUnit.SECONDS);
		timer.scheduleWithFixedDelay(() -> {
			if (globalDataHandler != null) {
				globalDataHandler.setString(plugin.getBungeeSettings().getServer(), "LastOnline",
						"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli());
			}
		}, 1, 60, TimeUnit.MINUTES);

		retireGlobalMysql(retirementDeadline);

		GlobalDataHandler loadedHandler;
		boolean loadedHandlerOwnsMysql;
		if (plugin.getBungeeSettings().isGloblalDataUseMainMySQL()
				&& plugin.getStorageType().equals(UserStorage.MYSQL)) {
			loadedHandler = new GlobalDataHandler(new GlobalMySQL("VotingPlugin_GlobalData", plugin.getMysql().getMysql()) {
				@Override public void debugEx(Exception e) { plugin.debug(e); }
				@Override public void debugLog(String text) { plugin.debug(text); }
				@Override public void info(String text) { plugin.getLogger().info(text); }
				@Override public void logSevere(String text) { plugin.getLogger().severe(text); }
				@Override public void warning(String text) { plugin.getLogger().warning(text); }
			});
			loadedHandlerOwnsMysql = false;
		} else {
			loadedHandler = new GlobalDataHandler(new GlobalMySQL("VotingPlugin_GlobalData",
					new MysqlConfigSpigot(plugin.getBungeeSettings().getData().getConfigurationSection("GlobalData"))) {
				@Override public void debugEx(Exception e) { plugin.debug(e); }
				@Override public void debugLog(String text) { plugin.debug(text); }
				@Override public void info(String text) { plugin.getLogger().info(text); }
				@Override public void logSevere(String text) { plugin.getLogger().severe(text); }
				@Override public void warning(String text) { plugin.getLogger().warning(text); }
			});
			loadedHandlerOwnsMysql = true;
		}
		synchronized (timeChangeLifecycleLock) {
			globalDataHandler = loadedHandler;
			ownsGlobalMysql = loadedHandlerOwnsMysql;
		}

		for (Map.Entry<String, String> column : Map.of(
				"IgnoreTime", "VARCHAR(5)",
				"MONTH", "VARCHAR(5)",
				"WEEK", "VARCHAR(5)",
				"DAY", "VARCHAR(5)",
				"FinishedProcessing", "VARCHAR(5)",
				"Processing", "VARCHAR(5)",
				"LastUpdated", "MEDIUMTEXT",
				"ForceUpdate", "VARCHAR(5)").entrySet()) {
			loadedHandler.getGlobalMysql().alterColumnType(column.getKey(), column.getValue());
		}
		plugin.getTimeChecker().setProcessingEnabled(false);
	}

	public void close() {
		close(CLOSE_GRACE_SECONDS, TimeUnit.SECONDS);
	}

	void close(long timeout, TimeUnit unit) {
		long timeoutNanos = Math.max(0L, unit.toNanos(timeout));
		long deadline = System.nanoTime() + timeoutNanos;
		shutdownTimer(deadline);
		retireGlobalMysql(deadline);
		synchronized (senderLock) {
			if (!senderHandedOff) sender = null;
		}
	}

	private void retireGlobalMysql(long deadlineNanos) {
		GlobalMySQL closeNow = null;
		GlobalDataHandler deferred = null;
		synchronized (timeChangeLifecycleLock) {
			GlobalDataHandler previous = globalDataHandler;
			boolean closeConnection = ownsGlobalMysql;
			globalDataHandler = null;
			ownsGlobalMysql = false;
			if (previous != null && activeTimeChangesByHandler.containsKey(previous)) {
				deferred = previous;
				if (closeConnection) retiredOwnedHandlers.add(previous);
			} else if (previous != null && closeConnection) {
				closeNow = previous.getGlobalMysql();
			}
		}
		if (closeNow != null) closeNow.close();
		if (deferred != null) awaitOrForceRetiredHandlerClose(deferred, deadlineNanos);
	}

	private void awaitOrForceRetiredHandlerClose(GlobalDataHandler handler, long deadlineNanos) {
		GlobalMySQL forceClose = null;
		boolean interrupted = false;
		synchronized (timeChangeLifecycleLock) {
			while (activeTimeChangesByHandler.containsKey(handler)) {
				long remaining = deadlineNanos - System.nanoTime();
				if (remaining <= 0L) break;
				try {
					TimeUnit.NANOSECONDS.timedWait(timeChangeLifecycleLock, remaining);
				} catch (InterruptedException failure) {
					interrupted = true;
					break;
				}
			}
			if (activeTimeChangesByHandler.containsKey(handler) && retiredOwnedHandlers.remove(handler)) {
				forceClose = handler.getGlobalMysql();
			}
		}
		if (forceClose != null) {
			plugin.getLogger().warning(
					"Forcing an owned global-data connection closed after its time-change shutdown grace expired");
			forceClose.close();
		}
		if (interrupted) Thread.currentThread().interrupt();
	}

	private void shutdownTimer(long deadlineNanos) {
		if (timer == null) {
			return;
		}
		timer.shutdown();
		try {
			long remaining = Math.max(0L, deadlineNanos - System.nanoTime());
			timer.awaitTermination(remaining, TimeUnit.NANOSECONDS);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		timer.shutdownNow();
		timer = null;
	}
}
