package com.bencodez.votingplugin.backendproxy.global;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
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
	private static final Map<VotingPluginMain, GlobalWorkAdmissions> GLOBAL_WORK_ADMISSIONS = new WeakHashMap<>();

	private final VotingPluginMain plugin;
	private final Object senderLock = new Object();
	private Consumer<JsonEnvelope> sender;
	private BackendGlobalDataSync completionReplacement;
	private boolean senderHandedOff;
	private final Set<TimeType> timeChangesInProgress = ConcurrentHashMap.newKeySet();
	private final GlobalWorkAdmissions globalWorkAdmissions;
	private final Object timeChangeLifecycleLock = new Object();
	private final Map<GlobalDataHandler, Integer> activeGlobalWorkByHandler = new HashMap<>();
	private final Set<GlobalDataHandler> retiredOwnedHandlers = new HashSet<>();
	private volatile boolean acceptingWork = true;
	private volatile ForceUpdateAdmission activeForceUpdate;

	@Getter
	private GlobalDataHandler globalDataHandler;
	private boolean ownsGlobalMysql;
	@Getter
	private ScheduledExecutorService timer;

	public BackendGlobalDataSync(VotingPluginMain plugin, Consumer<JsonEnvelope> sender) {
		this.plugin = plugin;
		this.sender = sender;
		// Runtime replacements use the same plugin instance. Share each period fence
		// and the aggregate completion state across those generations. A process
		// restart intentionally gets a fresh state so persisted flags remain eligible
		// for recovery.
		synchronized (GLOBAL_WORK_ADMISSIONS) {
			globalWorkAdmissions = GLOBAL_WORK_ADMISSIONS.computeIfAbsent(plugin,
					ignored -> new GlobalWorkAdmissions());
		}
	}

	public void checkGlobalData() {
		GlobalDataHandler pollingHandler = acquireHandlerReference();
		if (pollingHandler == null) return;
		try {
			String serverName = plugin.getBungeeSettings().getServer();
			HashMap<String, DataValue> data = pollingHandler.getExact(serverName);
			retryTimeChangeFinalization(pollingHandler, serverName);

			if (data.containsKey("ForceUpdate")) {
				String requestId = forceUpdateRequestId(data);
				if (checkGlobalDataTimeValue(data.get("ForceUpdate"))) {
					ForceUpdateAdmission admission = admitForceUpdate(pollingHandler, requestId);
					if (admission != null) startForceUpdate(admission, serverName);
				} else {
					globalWorkAdmissions.releaseAcknowledgedForceUpdate(requestId);
				}
			}

			checkGlobalDataTime(TimeType.MONTH, data, pollingHandler);
			checkGlobalDataTime(TimeType.WEEK, data, pollingHandler);
			checkGlobalDataTime(TimeType.DAY, data, pollingHandler);
		} finally {
			releaseHandlerReference(pollingHandler);
		}
	}

	private String forceUpdateRequestId(HashMap<String, DataValue> data) {
		DataValue value = data.get("ForceUpdateId");
		if (value == null) return null;
		String requestId = value.getString();
		return requestId == null || requestId.isBlank() ? null : requestId;
	}

	private ForceUpdateAdmission admitForceUpdate(GlobalDataHandler expectedHandler, String requestId) {
		GlobalDataHandler handler;
		ForceUpdateAdmission admission = null;
		synchronized (timeChangeLifecycleLock) {
			handler = acceptingWork ? globalDataHandler : null;
			if (handler != null && handler == expectedHandler) {
				ForceUpdateGrant grant = globalWorkAdmissions.admitForceUpdate(this, requestId);
				if (grant != null) {
					activeGlobalWorkByHandler.merge(handler, 1, Integer::sum);
					admission = new ForceUpdateAdmission(handler, grant.fence(), grant.acknowledgmentOnly());
					activeForceUpdate = admission;
				}
			}
		}
		return admission;
	}

	private void startForceUpdate(ForceUpdateAdmission admission, String serverName) {
		try {
			if (!admission.isPending()) return;
			if (admission.acknowledgmentOnly()) {
				finishForceUpdate(admission, serverName);
				return;
			}
			if (UserStorage.MYSQL.equals(plugin.getStorageType())) plugin.getMysql().clearCacheBasic();
			if (!acceptingWork || !admission.isPending()) {
				releaseForceUpdate(admission);
				return;
			}
			plugin.getBukkitScheduler().executeOrScheduleSync(plugin, () -> continueForceUpdate(admission, serverName));
		} catch (RuntimeException failure) {
			releaseForceUpdate(admission);
			plugin.debug(failure);
		}
	}

	private void continueForceUpdate(ForceUpdateAdmission admission, String serverName) {
		if (!acceptingWork || !admission.isPending()) {
			releaseForceUpdate(admission);
			return;
		}
		try {
			plugin.getUserManager().getDataManager().clearCacheAsyncCompletion().whenComplete((ignored, failure) -> {
				if (failure != null) {
					releaseForceUpdate(admission);
					plugin.debug(failure);
					return;
				}
				if (!acceptingWork || !admission.isPending()) {
					releaseForceUpdate(admission);
					return;
				}
				try {
					plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
							() -> finishForceUpdate(admission, serverName));
				} catch (RuntimeException schedulingFailure) {
					releaseForceUpdate(admission);
					plugin.debug(schedulingFailure);
				}
			});
		} catch (RuntimeException failure) {
			releaseForceUpdate(admission);
			plugin.debug(failure);
		}
	}

	private void finishForceUpdate(ForceUpdateAdmission admission, String serverName) {
		if (!acceptingWork) {
			if (admission.acknowledgmentOnly()) retainForceUpdateAcknowledgment(admission);
			else releaseForceUpdate(admission);
			return;
		}
		if (!admission.beginExecution()) return;
		try {
			if (!admission.acknowledgmentOnly()) {
				plugin.setUpdate(true);
				plugin.update();
				globalWorkAdmissions.markForceUpdateEffectApplied(admission.fence());
			}
			acknowledgeForceUpdate(admission.handler(), serverName, admission.fence().requestId);
		} catch (RuntimeException failure) {
			plugin.debug(failure);
			if (globalWorkAdmissions.isForceUpdateEffectApplied(admission.fence())) {
				retainForceUpdateAcknowledgment(admission);
				return;
			}
		} finally {
			if (!admission.isReleased()) releaseForceUpdate(admission);
		}
	}

	private void acknowledgeForceUpdate(GlobalDataHandler handler, String serverName, String requestId) {
		if (requestId == null) {
			handler.setBoolean(serverName, "ForceUpdate", false);
			return;
		}
		String escapedServer = serverName.replace("'", "''");
		String escapedRequest = requestId.replace("'", "''");
		handler.getGlobalMysql().executeQuery("UPDATE %tablename% SET ForceUpdate='false' WHERE server='"
				+ escapedServer + "' AND ForceUpdateId='" + escapedRequest + "';");
		HashMap<String, DataValue> current = handler.getExact(serverName);
		if (!current.containsKey("ForceUpdateId") || !current.containsKey("ForceUpdate")) {
			throw new IllegalStateException("ForceUpdate acknowledgment could not be verified");
		}
		String currentRequest = forceUpdateRequestId(current);
		if (requestId.equals(currentRequest) && checkGlobalDataTimeValue(current.get("ForceUpdate"))) {
			throw new IllegalStateException("ForceUpdate acknowledgment was not persisted");
		}
	}

	private void releaseForceUpdate(ForceUpdateAdmission admission) {
		if (!admission.release()) return;
		releaseForceUpdateResources(admission);
	}

	private void releaseForceUpdateResources(ForceUpdateAdmission admission) {
		synchronized (timeChangeLifecycleLock) {
			if (activeForceUpdate == admission) activeForceUpdate = null;
		}
		globalWorkAdmissions.releaseForceUpdate(admission.fence());
		releaseHandlerReference(admission.handler());
	}

	private void retainForceUpdateAcknowledgment(ForceUpdateAdmission admission) {
		if (!admission.release()) return;
		retainForceUpdateAcknowledgmentResources(admission);
	}

	private void retainForceUpdateAcknowledgmentResources(ForceUpdateAdmission admission) {
		synchronized (timeChangeLifecycleLock) {
			if (activeForceUpdate == admission) activeForceUpdate = null;
		}
		globalWorkAdmissions.retryForceUpdateAcknowledgment(admission.fence());
		releaseHandlerReference(admission.handler());
	}

	private GlobalDataHandler acquireHandlerReference() {
		synchronized (timeChangeLifecycleLock) {
			GlobalDataHandler handler = acceptingWork ? globalDataHandler : null;
			if (handler != null) activeGlobalWorkByHandler.merge(handler, 1, Integer::sum);
			return handler;
		}
	}

	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		return checkGlobalDataTime(type, data, globalDataHandler);
	}

	private boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data,
			GlobalDataHandler expectedHandler) {
		if (!data.containsKey(type.toString()) || !checkGlobalDataTimeValue(data.get(type.toString()))) {
			return false;
		}

		long lastUpdated = Long.valueOf(data.get("LastUpdated").getString()).longValue();
		plugin.debug("LastUpdated: " + lastUpdated);
		if (LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli() - lastUpdated > 1000 * 60 * 60 * 2) {
			plugin.getLogger().warning("Ignoring bungee time change since it was more than 2 hours ago");
			if (!isCurrentHandler(expectedHandler)) return false;
			expectedHandler.setBoolean(plugin.getBungeeSettings().getServer(), type.toString(), false);
			return false;
		}
		String serverName = plugin.getBungeeSettings().getServer();
		GlobalDataHandler transitionHandler = admitTimeChange(type, serverName, expectedHandler);
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

	private boolean isCurrentHandler(GlobalDataHandler expectedHandler) {
		synchronized (timeChangeLifecycleLock) {
			return acceptingWork && expectedHandler != null && globalDataHandler == expectedHandler;
		}
	}

	private GlobalDataHandler admitTimeChange(TimeType type, String serverName,
			GlobalDataHandler expectedHandler) {
		GlobalDataHandler handler;
		synchronized (timeChangeLifecycleLock) {
			handler = acceptingWork ? globalDataHandler : null;
			if (handler == null || handler != expectedHandler
					|| !globalWorkAdmissions.admitTimeChange(this, type)) return null;
			if (!timeChangesInProgress.add(type)) {
				globalWorkAdmissions.cancelTimeChangeAdmission(this, type);
				return null;
			}
			activeGlobalWorkByHandler.merge(handler, 1, Integer::sum);
		}
		try {
			synchronized (globalWorkAdmissions.persistenceLock()) {
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
		try {
			synchronized (timeChangeLifecycleLock) {
				timeChangesInProgress.remove(type);
			}
			persistTimeChangeFinalization(handler, serverName,
					globalWorkAdmissions.releaseTimeChange(this, type, completed));
		} finally {
			releaseHandlerReference(handler);
		}
	}

	private void retryTimeChangeFinalization(GlobalDataHandler handler, String serverName) {
		try {
			persistTimeChangeFinalization(handler, serverName,
					globalWorkAdmissions.claimTimeChangeFinalization(this));
		} catch (RuntimeException failure) {
			plugin.debug(failure);
		}
	}

	private void persistTimeChangeFinalization(GlobalDataHandler handler, String serverName,
			TimeChangeRelease release) {
		if (!release.finalizeBatch()) return;
		boolean persisted = false;
		try {
			synchronized (globalWorkAdmissions.persistenceLock()) {
				HashMap<String, DataValue> dataToSet = new HashMap<>();
				if (release.batchCompleted()) dataToSet.put("FinishedProcessing", new DataValueBoolean(true));
				dataToSet.put("Processing", new DataValueBoolean(false));
				handler.setData(serverName, dataToSet);
				persisted = true;
			}
		} finally {
			if (persisted) globalWorkAdmissions.finishTimeChangeRelease();
			else globalWorkAdmissions.retryTimeChangeFinalization();
		}
	}

	private void releaseHandlerReference(GlobalDataHandler handler) {
		GlobalMySQL closeAfterRelease;
		synchronized (timeChangeLifecycleLock) {
			closeAfterRelease = releaseHandlerReferenceLocked(handler);
			timeChangeLifecycleLock.notifyAll();
		}
		if (closeAfterRelease != null) closeAfterRelease.close();
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
		Integer references = activeGlobalWorkByHandler.get(handler);
		if (references == null || references <= 1) {
			activeGlobalWorkByHandler.remove(handler);
			return retiredOwnedHandlers.remove(handler) ? handler.getGlobalMysql() : null;
		}
		activeGlobalWorkByHandler.put(handler, references - 1);
		return null;
	}

	public boolean checkGlobalDataTimeValue(DataValue data) {
		if (data.isBoolean()) {
			return data.getBoolean();
		}
		return Boolean.valueOf(data.getString());
	}

	public void load() {
		if (!plugin.getBungeeSettings().isGloblalDataEnabled()) {
			return;
		}
		acceptingWork = false;

		long retirementDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(CLOSE_GRACE_SECONDS);
		shutdownTimer(retirementDeadline);
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
				"ForceUpdateId", "VARCHAR(36)",
				"ForceUpdate", "VARCHAR(5)").entrySet()) {
			loadedHandler.getGlobalMysql().alterColumnType(column.getKey(), column.getValue());
		}
		acceptingWork = true;
		timer = Executors.newSingleThreadScheduledExecutor(task -> {
			Thread thread = new Thread(task, "VotingPlugin-GlobalData");
			thread.setDaemon(true);
			return thread;
		});
		timer.scheduleWithFixedDelay(this::checkGlobalData, 60, 10, TimeUnit.SECONDS);
		timer.scheduleWithFixedDelay(this::updateLastOnline, 1, 60, TimeUnit.MINUTES);
		plugin.getTimeChecker().setProcessingEnabled(false);
	}

	public void close() {
		close(CLOSE_GRACE_SECONDS, TimeUnit.SECONDS);
	}

	void close(long timeout, TimeUnit unit) {
		acceptingWork = false;
		ForceUpdateAdmission forceUpdate = activeForceUpdate;
		if (forceUpdate != null && forceUpdate.cancelPending()) {
			if (forceUpdate.acknowledgmentOnly()) retainForceUpdateAcknowledgmentResources(forceUpdate);
			else releaseForceUpdateResources(forceUpdate);
		}
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
			if (previous != null && activeGlobalWorkByHandler.containsKey(previous)) {
				deferred = previous;
				if (closeConnection) retiredOwnedHandlers.add(previous);
			} else if (previous != null && closeConnection) {
				closeNow = previous.getGlobalMysql();
			}
		}
		if (closeNow != null) closeNow.close();
		if (deferred != null) awaitOrForceRetiredHandlerClose(deferred, deadlineNanos);
		BackendGlobalDataSync replacement;
		synchronized (senderLock) {
			replacement = completionReplacement;
		}
		// Failed finalization and pending acknowledgment retries may release their SQL
		// references before retirement observes active work. Transfer both fences.
		globalWorkAdmissions.transferTimeChangeOwner(this, replacement);
		globalWorkAdmissions.transferForceUpdateOwner(this, replacement);
	}

	void updateLastOnline() {
		GlobalDataHandler pollingHandler = acquireHandlerReference();
		if (pollingHandler == null) return;
		try {
			pollingHandler.setString(plugin.getBungeeSettings().getServer(), "LastOnline",
					"" + LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant().toEpochMilli());
		} finally {
			releaseHandlerReference(pollingHandler);
		}
	}

	private boolean awaitOrForceRetiredHandlerClose(GlobalDataHandler handler, long deadlineNanos) {
		GlobalMySQL forceClose = null;
		boolean interrupted = false;
		boolean stillActive;
		synchronized (timeChangeLifecycleLock) {
			while (activeGlobalWorkByHandler.containsKey(handler)) {
				long remaining = deadlineNanos - System.nanoTime();
				if (remaining <= 0L) break;
				try {
					TimeUnit.NANOSECONDS.timedWait(timeChangeLifecycleLock, remaining);
				} catch (InterruptedException failure) {
					interrupted = true;
					break;
				}
			}
			if (activeGlobalWorkByHandler.containsKey(handler) && retiredOwnedHandlers.remove(handler)) {
				forceClose = handler.getGlobalMysql();
			}
			stillActive = activeGlobalWorkByHandler.containsKey(handler);
		}
		if (forceClose != null) {
			plugin.getLogger().warning(
					"Forcing an owned global-data connection closed after its time-change shutdown grace expired");
			forceClose.close();
		}
		if (interrupted) Thread.currentThread().interrupt();
		return stillActive;
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

	private record ForceUpdateAdmission(GlobalDataHandler handler, ForceUpdateFence fence,
			boolean acknowledgmentOnly, AtomicInteger phase) {
		private static final int PENDING = 0;
		private static final int EXECUTING = 1;
		private static final int RELEASED = 2;

		private ForceUpdateAdmission(GlobalDataHandler handler, ForceUpdateFence fence,
				boolean acknowledgmentOnly) {
			this(handler, fence, acknowledgmentOnly, new AtomicInteger(PENDING));
		}

		private boolean beginExecution() {
			return phase.compareAndSet(PENDING, EXECUTING);
		}

		private boolean isPending() {
			return phase.get() == PENDING;
		}

		private boolean cancelPending() {
			return phase.compareAndSet(PENDING, RELEASED);
		}

		private boolean release() {
			return phase.getAndSet(RELEASED) != RELEASED;
		}

		private boolean isReleased() {
			return phase.get() == RELEASED;
		}
	}

	private static final class ForceUpdateFence {
		private final String requestId;
		private boolean effectApplied;
		private boolean acknowledgmentClaimed;

		private ForceUpdateFence(String requestId) {
			this.requestId = requestId;
		}
	}

	private record ForceUpdateGrant(ForceUpdateFence fence, boolean acknowledgmentOnly) {
	}

	private static final class GlobalWorkAdmissions {
		private final Map<TimeType, BackendGlobalDataSync> activeTimeChanges = new EnumMap<>(TimeType.class);
		private final Object persistenceLock = new Object();
		private BackendGlobalDataSync timeChangeOwner;
		private BackendGlobalDataSync forceUpdateOwner;
		private ForceUpdateFence forceUpdateFence;
		private boolean timeChangeBatchFailed;
		private boolean timeChangeBatchFinalizing;
		private boolean timeChangeFinalizationClaimed;

		private synchronized boolean admitTimeChange(BackendGlobalDataSync requester, TimeType type) {
			if (timeChangeBatchFinalizing) return false;
			if (timeChangeOwner != null && timeChangeOwner != requester) return false;
			if (activeTimeChanges.containsKey(type)) return false;
			if (activeTimeChanges.isEmpty()) timeChangeBatchFailed = false;
			activeTimeChanges.put(type, requester);
			timeChangeOwner = requester;
			return true;
		}

		private synchronized TimeChangeRelease releaseTimeChange(BackendGlobalDataSync requester, TimeType type,
				boolean completed) {
			if (activeTimeChanges.get(type) != requester) return TimeChangeRelease.NONE;
			activeTimeChanges.remove(type);
			if (!completed) timeChangeBatchFailed = true;
			if (!activeTimeChanges.isEmpty()) return TimeChangeRelease.NONE;
			timeChangeBatchFinalizing = true;
			timeChangeFinalizationClaimed = true;
			return new TimeChangeRelease(true, !timeChangeBatchFailed);
		}

		private synchronized void cancelTimeChangeAdmission(BackendGlobalDataSync requester, TimeType type) {
			if (activeTimeChanges.get(type) != requester) return;
			activeTimeChanges.remove(type);
			if (activeTimeChanges.isEmpty()) timeChangeOwner = null;
		}

		private synchronized void finishTimeChangeRelease() {
			if (!timeChangeBatchFinalizing || !activeTimeChanges.isEmpty()) return;
			timeChangeBatchFinalizing = false;
			timeChangeFinalizationClaimed = false;
			timeChangeBatchFailed = false;
			timeChangeOwner = null;
		}

		private synchronized TimeChangeRelease claimTimeChangeFinalization(BackendGlobalDataSync requester) {
			if (!timeChangeBatchFinalizing || timeChangeFinalizationClaimed
					|| (timeChangeOwner != null && timeChangeOwner != requester)) return TimeChangeRelease.NONE;
			timeChangeOwner = requester;
			timeChangeFinalizationClaimed = true;
			return new TimeChangeRelease(true, !timeChangeBatchFailed);
		}

		private synchronized void retryTimeChangeFinalization() {
			if (timeChangeBatchFinalizing) timeChangeFinalizationClaimed = false;
		}

		private synchronized void transferTimeChangeOwner(BackendGlobalDataSync previous,
				BackendGlobalDataSync replacement) {
			if (timeChangeOwner != previous) return;
			timeChangeOwner = replacement;
			if (timeChangeBatchFinalizing) timeChangeFinalizationClaimed = false;
		}

		private Object persistenceLock() {
			return persistenceLock;
		}

		private synchronized ForceUpdateGrant admitForceUpdate(BackendGlobalDataSync requester, String requestId) {
			if (forceUpdateFence == null) {
				forceUpdateFence = new ForceUpdateFence(requestId);
				forceUpdateFence.acknowledgmentClaimed = true;
				forceUpdateOwner = requester;
				return new ForceUpdateGrant(forceUpdateFence, false);
			}
			if (forceUpdateFence.effectApplied && !forceUpdateFence.acknowledgmentClaimed
					&& (forceUpdateOwner == null || forceUpdateOwner == requester)) {
				forceUpdateOwner = requester;
				forceUpdateFence.acknowledgmentClaimed = true;
				return new ForceUpdateGrant(forceUpdateFence, true);
			}
			return null;
		}

		private synchronized void markForceUpdateEffectApplied(ForceUpdateFence fence) {
			if (forceUpdateFence == fence) fence.effectApplied = true;
		}

		private synchronized boolean isForceUpdateEffectApplied(ForceUpdateFence fence) {
			return forceUpdateFence == fence && fence.effectApplied;
		}

		private synchronized void retryForceUpdateAcknowledgment(ForceUpdateFence fence) {
			if (forceUpdateFence == fence) fence.acknowledgmentClaimed = false;
		}

		private synchronized void releaseForceUpdate(ForceUpdateFence fence) {
			if (forceUpdateFence != fence) return;
			forceUpdateFence = null;
			forceUpdateOwner = null;
		}

		private synchronized void releaseAcknowledgedForceUpdate(String requestId) {
			if (forceUpdateFence == null || !forceUpdateFence.effectApplied
					|| !Objects.equals(forceUpdateFence.requestId, requestId)) return;
			forceUpdateFence = null;
			forceUpdateOwner = null;
		}

		private synchronized void transferForceUpdateOwner(BackendGlobalDataSync previous,
				BackendGlobalDataSync replacement) {
			if (forceUpdateOwner != previous) return;
			forceUpdateOwner = replacement;
			if (forceUpdateFence != null && forceUpdateFence.effectApplied) {
				forceUpdateFence.acknowledgmentClaimed = false;
			}
		}
	}

	private record TimeChangeRelease(boolean finalizeBatch, boolean batchCompleted) {
		private static final TimeChangeRelease NONE = new TimeChangeRelease(false, false);
	}
}
