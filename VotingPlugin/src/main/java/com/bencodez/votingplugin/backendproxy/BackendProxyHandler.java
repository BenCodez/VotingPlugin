package com.bencodez.votingplugin.backendproxy;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.backendproxy.global.BackendGlobalDataSync;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter;
import com.bencodez.votingplugin.backendproxy.messaging.BackendProxyMessageRouter.OrderedVoteOutcome;
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.util.VoteTaskAdmission;

import lombok.Getter;

/**
 * Coordinates backend/proxy communication components.
 */
public class BackendProxyHandler implements Listener {

	private final VotingPluginMain plugin;
	static final int MAX_ORDERED_VOTE_QUEUE = 256;

	private final ProcessedVoteCache processedVoteCache;
	private final BackendOrderedVoteOverflowQueue orderedVoteOverflow;
	private final BackendProxyTransportManager transportManager;
	private final BackendGlobalDataSync globalDataSync;

	private BackendPresenceManager presenceManager;
	private boolean presenceReportingActivated;
	private final Object inboundPublication = new Object();
	private final Object orderedVoteDispatch = new Object();
	private final ArrayDeque<JsonEnvelope> orderedVoteDispatchQueue = new ArrayDeque<>();
	private boolean orderedVoteDispatchActive;
	private boolean orderedVoteDispatchPaused = true;
	private boolean orderedVoteDispatchClosing;
	private boolean orderedVoteQuarantineFailed;
	private JsonEnvelope orderedVoteDispatchInFlight;
	private BackendOrderedVoteOverflowQueue.PendingEnvelope orderedVoteOverflowInFlight;
	private JsonEnvelope orderedVoteShutdownQuarantined;
	private BackendProxyHandler orderedVoteHandoffTarget;
	private boolean orderedVoteOverflowWarningLogged;
	private boolean inboundPublished;
	private boolean inboundAborted;
	private BackendProxyHandler inboundRollbackTarget;
	private BackendVotePartySync votePartySync;
	private boolean persistVotePartyOnClose = true;
	private BackendProxyMessageRouter messageRouter;

	@Getter
	private BungeeMethod method;
	@Getter
	private GlobalMessageHandler globalMessageHandler;

	public BackendProxyHandler(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache(), null);
	}

	public BackendProxyHandler(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this(plugin, processedVoteCache, null);
	}

	public BackendProxyHandler(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache,
			BackendOrderedVoteOverflowQueue orderedVoteOverflow) {
		this.plugin = plugin;
		this.processedVoteCache = java.util.Objects.requireNonNull(processedVoteCache, "processedVoteCache");
		this.orderedVoteOverflow = orderedVoteOverflow;
		transportManager = new BackendProxyTransportManager(plugin, processedVoteCache);
		globalDataSync = new BackendGlobalDataSync(plugin, this::sendEnvelope);
	}

	/**
	 * Loads the configured backend/proxy communication components.
	 */
	public void load() {
		load(true);
	}

	/** Loads a replacement without announcing a new presence generation before publication. */
	public void loadForReplacement() {
		load(false);
	}

	private void load(boolean activatePresenceReporting) {
		plugin.debug("Loading backend proxy handler");
		method = BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod());
		plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		globalDataSync.load();
		globalMessageHandler = new GlobalMessageHandler() {
			@Override
			public void onMessage(JsonEnvelope envelope) {
				BackendProxyHandler.this.dispatchIncomingAfterPublication(envelope, () -> super.onMessage(envelope));
			}

			@Override
			public void sendMessage(JsonEnvelope envelope) {
				transportManager.send(envelope);
			}
		};

		presenceManager = new BackendPresenceManager(plugin, method, globalMessageHandler);
		votePartySync = new BackendVotePartySync(plugin);
		messageRouter = new BackendProxyMessageRouter(plugin, presenceManager, globalDataSync, votePartySync,
				processedVoteCache);
		messageRouter.register(globalMessageHandler, method);
		transportManager.start(method, globalMessageHandler, activatePresenceReporting);

		if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
			plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
		}
		if (activatePresenceReporting) {
			activatePresenceReporting();
			activateInboundMessages();
		}
	}

	/** Starts presence only after a staged handler reaches the atomic publication boundary. */
	public void activatePresenceReporting() {
		if (presenceManager != null && !presenceReportingActivated) {
			presenceManager.start();
			presenceReportingActivated = true;
		}
		// Presence startup can throw while scheduling its heartbeat. Keep inbound
		// HTTP callbacks behind the publication barrier until every fallible part of
		// the replacement is active, so rollback cannot race a queued callback.
		transportManager.activateAfterPublication();
	}

	/**
	 * Closes backend/proxy components and persists cached proxy state.
	 */
	public void close() {
		synchronized (inboundPublication) {
			if (!inboundPublished && !inboundAborted) inboundAborted = true;
			inboundPublication.notifyAll();
		}
		persistPendingOrderedVotesOnClose();
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceReportingActivated = false;
		}
		transportManager.close();
		if (votePartySync != null && persistVotePartyOnClose) {
			votePartySync.persist();
		}
		globalDataSync.close();
	}

	/** Opens inbound dispatch only after the replacement and all handoffs are committed. */
	public void activateInboundMessages() {
		synchronized (inboundPublication) {
			if (inboundAborted) return;
			inboundPublished = true;
			inboundPublication.notifyAll();
		}
		activateOrderedVoteDispatch();
	}

	/** Routes an already accepted staged callback through the restored predecessor on rollback. */
	public void abortStagedInboundTo(BackendProxyHandler previous) {
		synchronized (inboundPublication) {
			if (inboundPublished || inboundAborted) return;
			inboundRollbackTarget = previous;
			inboundAborted = true;
			inboundPublication.notifyAll();
		}
	}

	void dispatchIncomingAfterPublication(JsonEnvelope envelope, Runnable localDispatch) {
		BackendProxyHandler rollbackTarget;
		synchronized (inboundPublication) {
			while (!inboundPublished && !inboundAborted) {
				try {
					inboundPublication.wait();
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					return;
				}
			}
			rollbackTarget = inboundPublished ? null : inboundRollbackTarget;
			if (!inboundPublished && rollbackTarget == null) return;
		}
		if (rollbackTarget != null) {
			GlobalMessageHandler rollbackHandler = rollbackTarget.globalMessageHandler;
			if (rollbackHandler != null) rollbackHandler.onMessage(envelope);
			return;
		}
		// Reward-bearing proxy votes construct an asynchronous PlayerVoteEvent. The
		// proxy's HTTP/Redis/MQTT/MySQL/socket transports do not preserve the numeric
		// plugin-message delay, so Vote and VoteUpdate must also share one ordered
		// asynchronous lane.
		if (isOrderedVoteMessage(envelope)) {
			dispatchOrderedVote(envelope, localDispatch);
			return;
		}
		// Global-data checks perform synchronous SQL and already run on their own
		// timer worker. Explicitly dispatch an inbound wake-up asynchronously too:
		// plugin messaging can invoke this method on Bukkit's primary thread.
		if (VotingPluginWire.SUB_BUNGEE_TIME_CHANGE.equals(envelope.getSubChannel())) {
			plugin.getBukkitScheduler().runTaskAsynchronously(plugin, localDispatch);
			return;
		}
		plugin.getBukkitScheduler().executeOrScheduleSync(plugin, localDispatch);
	}

	private boolean isOrderedVoteMessage(JsonEnvelope envelope) {
		String subChannel = envelope.getSubChannel();
		return VotingPluginWire.SUB_VOTE.equals(subChannel)
				|| VotingPluginWire.SUB_VOTE_ONLINE.equals(subChannel)
				|| VotingPluginWire.SUB_VOTE_UPDATE.equals(subChannel);
	}

	private void dispatchOrderedVote(JsonEnvelope envelope, Runnable ignoredLocalDispatch) {
		BackendProxyHandler handoffTarget;
		synchronized (orderedVoteDispatch) {
			handoffTarget = orderedVoteHandoffTarget;
			if (handoffTarget == null) {
				if (orderedVoteDispatchClosing) {
					if (!spillOrderedVote(envelope)) {
						throw new IllegalStateException("Ordered proxy vote shutdown overflow is full");
					}
					return;
				}
				boolean preserveOverflowOrder = orderedVoteOverflow != null && orderedVoteOverflow.hasEntries();
				if (preserveOverflowOrder || orderedVoteDispatchQueue.size() >= MAX_ORDERED_VOTE_QUEUE) {
					if (!spillOrderedVote(envelope)) {
						throw new IllegalStateException("Ordered proxy vote queue and durable overflow are full");
					}
					return;
				}
				orderedVoteDispatchQueue.addLast(envelope);
				scheduleOrderedVoteDispatchLocked();
				return;
			}
		}
		handoffTarget.dispatchOrderedVote(envelope, ignoredLocalDispatch);
	}

	private boolean spillOrderedVote(JsonEnvelope envelope) {
		if (orderedVoteOverflow != null
				&& orderedVoteOverflow.enqueueAsync(envelope, orderedVoteDispatchQueue.size(), this::completeOverflowAdmission)) {
			if (!orderedVoteOverflowWarningLogged && plugin != null && plugin.getLogger() != null) {
				orderedVoteOverflowWarningLogged = true;
				plugin.getLogger().warning("Ordered proxy vote lane is saturated; queueing messages for durable overflow");
			}
			return true;
		}
		if (plugin != null && plugin.getLogger() != null) {
			plugin.getLogger().severe("Ordered proxy vote lane and durable overflow are full; delivery was rejected");
		}
		return false;
	}

	private void completeOverflowAdmission(boolean stored) {
		if (stored) return;
		synchronized (orderedVoteDispatch) {
			orderedVoteQuarantineFailed = true;
			orderedVoteDispatchPaused = true;
			orderedVoteDispatch.notifyAll();
		}
		if (plugin != null && plugin.getLogger() != null) {
			plugin.getLogger().severe("Unable to persist an admitted ordered proxy vote; processing has stopped");
		}
	}

	private void activateOrderedVoteDispatch() {
		if (orderedVoteOverflow != null) {
			orderedVoteOverflow.bindWakeup(this, this::onOrderedVoteOverflowDurable);
		}
		synchronized (orderedVoteDispatch) {
			orderedVoteDispatchPaused = false;
			scheduleOrderedVoteDispatchLocked();
		}
	}

	private void onOrderedVoteOverflowDurable() {
		synchronized (orderedVoteDispatch) {
			scheduleOrderedVoteDispatchLocked();
		}
	}

	private void scheduleOrderedVoteDispatchLocked() {
		if (orderedVoteDispatchPaused || orderedVoteDispatchActive || orderedVoteQuarantineFailed) return;
		if (orderedVoteOverflow != null && orderedVoteOverflow.isQuarantineCapacityExhausted()) {
			orderedVoteQuarantineFailed = true;
			orderedVoteDispatchPaused = true;
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().severe("Ordered proxy vote processing is stopped because FailedEnvelopes is full");
			}
			return;
		}
		if (orderedVoteDispatchQueue.isEmpty()
				&& (orderedVoteOverflow == null || orderedVoteOverflow.peekDurable() == null)) {
			return;
		}
		orderedVoteDispatchActive = true;
		try {
			plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
					VoteTaskAdmission.ownedTask(this::runNextOrderedVoteDispatch));
		} catch (RuntimeException schedulingFailure) {
			orderedVoteDispatchActive = false;
			orderedVoteDispatch.notifyAll();
			if (!spillPrimaryOrderedVotesLocked() && plugin != null) plugin.debug(schedulingFailure);
			retryOrderedVoteDispatchLocked();
		}
	}

	private void retryOrderedVoteDispatchLocked() {
		if (orderedVoteDispatchClosing || orderedVoteDispatchPaused || orderedVoteQuarantineFailed) return;
		if (orderedVoteOverflow != null && orderedVoteOverflow.retryLater(this, this::onOrderedVoteOverflowDurable)) {
			return;
		}
		if (orderedVoteOverflow == null) {
			try {
				plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin,
						this::onOrderedVoteOverflowDurable, 20L);
				return;
			} catch (RuntimeException ignored) {
				// A disabled scheduler cannot retry this test-only, non-durable handler.
			}
		}
		if (plugin != null && plugin.getLogger() != null) {
			plugin.getLogger().severe("Unable to schedule an ordered proxy vote retry; queued messages remain pending");
		}
	}

	private boolean spillPrimaryOrderedVotesLocked() {
		if (orderedVoteDispatchQueue.isEmpty()) return true;
		if (orderedVoteOverflow == null) return false;
		List<JsonEnvelope> pending = new ArrayList<>(orderedVoteDispatchQueue);
		if (!orderedVoteOverflow.prepend(pending)) {
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().severe("Unable to preserve ordered proxy votes after async scheduler rejection");
			}
			return false;
		}
		orderedVoteDispatchQueue.clear();
		return true;
	}

	private void runNextOrderedVoteDispatch() {
		JsonEnvelope next;
		BackendOrderedVoteOverflowQueue.PendingEnvelope overflowEntry = null;
		synchronized (orderedVoteDispatch) {
			if (orderedVoteDispatchPaused) {
				orderedVoteDispatchActive = false;
				orderedVoteDispatch.notifyAll();
				return;
			}
			next = orderedVoteDispatchQueue.peekFirst();
			if (next == null && orderedVoteOverflow != null) {
				overflowEntry = orderedVoteOverflow.peekDurable();
				if (overflowEntry != null) next = overflowEntry.envelope();
			}
			if (next == null) {
				orderedVoteDispatchActive = false;
				orderedVoteDispatch.notifyAll();
				return;
			}
			orderedVoteDispatchInFlight = next;
			orderedVoteOverflowInFlight = overflowEntry;
		}
		BackendOrderedVoteOverflowQueue.PendingEnvelope completingOverflowEntry = overflowEntry;
		JsonEnvelope completingEnvelope = next;
		AtomicBoolean completed = new AtomicBoolean();
		java.util.function.Consumer<OrderedVoteOutcome> complete = outcome -> {
			if (completed.compareAndSet(false, true)) {
				finishOrderedVoteDispatch(completingOverflowEntry, completingEnvelope, outcome);
			}
		};
		try {
			messageRouter.handleOrderedVote(next, complete);
		} catch (RuntimeException | Error failure) {
			complete.accept(OrderedVoteOutcome.QUARANTINE);
			throw failure;
		}
	}

	private void finishOrderedVoteDispatch(BackendOrderedVoteOverflowQueue.PendingEnvelope overflowEntry,
			JsonEnvelope envelope, OrderedVoteOutcome outcome) {
		synchronized (orderedVoteDispatch) {
			if (envelope == orderedVoteShutdownQuarantined) {
				orderedVoteDispatch.notifyAll();
				return;
			}
		}
		if (outcome == OrderedVoteOutcome.QUARANTINE) {
			if (orderedVoteOverflow == null) {
				completeOrderedVoteQuarantine(overflowEntry, false);
			} else {
				orderedVoteOverflow.quarantineAsync(overflowEntry, envelope,
						stored -> completeOrderedVoteQuarantine(overflowEntry, stored));
			}
			return;
		}
		boolean successful = outcome == OrderedVoteOutcome.COMPLETE;
		if (successful && overflowEntry != null && orderedVoteOverflow != null) {
			orderedVoteOverflow.acknowledgeAsync(overflowEntry,
					stored -> completeOrderedVoteAcknowledgement(stored));
			return;
		}
		synchronized (orderedVoteDispatch) {
			if (successful && overflowEntry == null
					&& orderedVoteDispatchQueue.peekFirst() == orderedVoteDispatchInFlight) {
				orderedVoteDispatchQueue.removeFirst();
			}
			orderedVoteDispatchInFlight = null;
			orderedVoteOverflowInFlight = null;
			orderedVoteDispatchActive = false;
			orderedVoteDispatch.notifyAll();
			if (successful) scheduleOrderedVoteDispatchLocked();
			else retryOrderedVoteDispatchLocked();
		}
	}

	private void completeOrderedVoteAcknowledgement(boolean stored) {
		synchronized (orderedVoteDispatch) {
			if (stored) {
				orderedVoteDispatchInFlight = null;
				orderedVoteOverflowInFlight = null;
			} else {
				orderedVoteQuarantineFailed = true;
				orderedVoteDispatchPaused = true;
				if (plugin != null && plugin.getLogger() != null) {
					plugin.getLogger().severe("Unable to persist ordered proxy vote completion; processing has stopped");
				}
			}
			orderedVoteDispatchActive = false;
			orderedVoteDispatch.notifyAll();
			if (stored) scheduleOrderedVoteDispatchLocked();
		}
	}

	private void completeOrderedVoteQuarantine(BackendOrderedVoteOverflowQueue.PendingEnvelope overflowEntry,
			boolean stored) {
		synchronized (orderedVoteDispatch) {
			boolean capacityExhausted = stored && orderedVoteOverflow != null
					&& orderedVoteOverflow.isQuarantineCapacityExhausted();
			if (stored && overflowEntry == null
					&& orderedVoteDispatchQueue.peekFirst() == orderedVoteDispatchInFlight) {
				orderedVoteDispatchQueue.removeFirst();
			}
			if (stored) {
				orderedVoteDispatchInFlight = null;
				orderedVoteOverflowInFlight = null;
			} else {
				orderedVoteQuarantineFailed = true;
			}
			if (capacityExhausted) orderedVoteQuarantineFailed = true;
			orderedVoteDispatchActive = false;
			if (!stored || capacityExhausted) orderedVoteDispatchPaused = true;
			orderedVoteDispatch.notifyAll();
			if (plugin != null && plugin.getLogger() != null) {
				plugin.getLogger().log(stored && !capacityExhausted ? java.util.logging.Level.WARNING
						: java.util.logging.Level.SEVERE,
						capacityExhausted
								? "FailedEnvelopes is full; ordered vote processing has stopped for manual review"
								: stored
										? "Failed proxy vote retained in BackendProxyVoteQueue.yml FailedEnvelopes for manual review"
										: "Unable to retain failed proxy vote; ordered vote processing has stopped");
			}
			if (stored && !capacityExhausted) scheduleOrderedVoteDispatchLocked();
		}
	}

	/**
	 * Pauses the predecessor's ordered lane and waits off-thread for only its
	 * currently running message. Pending messages stay queued for atomic handoff.
	 */
	public void pauseOrderedVoteDispatchForReplacement(long deadlineNanos) {
		synchronized (orderedVoteDispatch) {
			orderedVoteDispatchPaused = true;
			while (orderedVoteDispatchActive) {
				long remaining = deadlineNanos - System.nanoTime();
				if (remaining <= 0L) {
					throw new IllegalStateException("Ordered proxy vote lane did not quiesce before replacement");
				}
				try {
					java.util.concurrent.TimeUnit.NANOSECONDS.timedWait(orderedVoteDispatch, remaining);
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					throw new IllegalStateException("Interrupted while quiescing ordered proxy votes", interrupted);
				}
			}
		}
	}

	/** Restores the paused predecessor when staged publication is abandoned. */
	public void resumeOrderedVoteDispatchAfterFailedReplacement() {
		if (orderedVoteOverflow != null) orderedVoteOverflow.bindWakeup(this, this::onOrderedVoteOverflowDurable);
		synchronized (orderedVoteDispatch) {
			if (orderedVoteHandoffTarget != null) return;
			persistVotePartyOnClose = true;
			orderedVoteDispatchPaused = false;
			scheduleOrderedVoteDispatchLocked();
		}
	}

	/** Transfers the quiesced vote-party snapshot and fences stale predecessor persistence. */
	public void completeVotePartyHandoff(BackendProxyHandler replacement) {
		if (replacement == null) return;
		synchronized (orderedVoteDispatch) {
			if (!orderedVoteDispatchPaused || orderedVoteDispatchActive || votePartySync == null
					|| replacement.votePartySync == null) {
				throw new IllegalStateException("Vote-party state was not quiesced before handoff");
			}
			replacement.votePartySync.replace(votePartySync.getCurrent(), votePartySync.getRequired());
			persistVotePartyOnClose = false;
		}
	}

	/**
	 * Transfers the predecessor's bounded in-memory prefix after every fallible
	 * publication step succeeds. The shared durable overflow remains in place.
	 */
	public void completeOrderedVoteHandoff(BackendProxyHandler replacement) {
		if (replacement == null) return;
		synchronized (orderedVoteDispatch) {
			if (!orderedVoteDispatchPaused || orderedVoteDispatchActive || orderedVoteQuarantineFailed) {
				throw new IllegalStateException("Ordered proxy vote lane was not quiesced before handoff");
			}
			List<JsonEnvelope> pending = new ArrayList<>(orderedVoteDispatchQueue);
			replacement.acceptOrderedVoteHandoff(pending);
			orderedVoteDispatchQueue.clear();
			orderedVoteHandoffTarget = replacement;
			if (orderedVoteOverflow != null) orderedVoteOverflow.unbindWakeup(this);
		}
	}

	private void acceptOrderedVoteHandoff(List<JsonEnvelope> pending) {
		synchronized (orderedVoteDispatch) {
			if (orderedVoteDispatchQueue.size() + pending.size() > MAX_ORDERED_VOTE_QUEUE) {
				throw new IllegalStateException("Ordered proxy vote handoff exceeds the bounded in-memory queue");
			}
			orderedVoteDispatchQueue.addAll(pending);
		}
	}

	private void persistPendingOrderedVotesOnClose() {
		List<JsonEnvelope> pending = new ArrayList<>();
		synchronized (orderedVoteDispatch) {
			orderedVoteDispatchClosing = true;
			orderedVoteDispatchPaused = true;
			long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(1);
			while (orderedVoteDispatchActive) {
				long remaining = deadline - System.nanoTime();
				if (remaining <= 0L) break;
				try {
					java.util.concurrent.TimeUnit.NANOSECONDS.timedWait(orderedVoteDispatch, remaining);
				} catch (InterruptedException interrupted) {
					Thread.currentThread().interrupt();
					break;
				}
			}
			if (orderedVoteHandoffTarget == null && orderedVoteOverflow != null) {
				pending.addAll(orderedVoteDispatchQueue);
				if (orderedVoteDispatchActive && orderedVoteDispatchInFlight != null) {
					boolean inMemory = !pending.isEmpty() && pending.get(0) == orderedVoteDispatchInFlight;
					boolean quarantinePending = orderedVoteOverflow.hasPendingFailure(orderedVoteDispatchInFlight);
					boolean quarantined = quarantinePending || orderedVoteOverflow.quarantineForShutdown(
							orderedVoteOverflowInFlight, orderedVoteDispatchInFlight);
					if (quarantined) {
						if (inMemory) {
							pending.remove(0);
							orderedVoteDispatchQueue.removeFirst();
						}
						if (!quarantinePending) {
							orderedVoteShutdownQuarantined = orderedVoteDispatchInFlight;
							orderedVoteDispatchInFlight = null;
							orderedVoteOverflowInFlight = null;
							orderedVoteDispatchActive = false;
							orderedVoteDispatch.notifyAll();
						}
					}
					if (plugin != null && plugin.getLogger() != null) {
						plugin.getLogger().log(quarantined ? java.util.logging.Level.WARNING
								: java.util.logging.Level.SEVERE,
								quarantined
										? "Ordered proxy vote exceeded the shutdown bound and was retained for manual review"
										: "Unable to isolate an ordered proxy vote that exceeded the shutdown bound");
					}
				} else if (!pending.isEmpty() && pending.get(0) == orderedVoteDispatchInFlight
						&& orderedVoteOverflow.hasPendingFailure(orderedVoteDispatchInFlight)) {
					pending.remove(0);
				}
				if (!pending.isEmpty() && orderedVoteOverflow.prepend(pending)) {
					orderedVoteDispatchQueue.clear();
				} else if (!pending.isEmpty() && plugin != null && plugin.getLogger() != null) {
					plugin.getLogger().severe("Unable to persist all pending ordered proxy votes during shutdown");
				}
			}
			if (orderedVoteOverflow != null) orderedVoteOverflow.unbindWakeup(this);
		}
	}

	/** Returns whether replacement preparation must preserve accepted deliveries. */
	public boolean requiresPreparationForReplacement() {
		return method == BungeeMethod.HTTP || method == BungeeMethod.PLUGINMESSAGING
				|| transportManager.hasPendingAsyncHandoff() || transportManager.hasPendingRedisReplay();
	}

	/** Prepares HTTP state or waits off-thread for an earlier cross-transport handoff. */
	public boolean prepareForReplacement(BungeeMethod replacementMethod) {
		return prepareForReplacement(replacementMethod,
				System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(25));
	}

	public boolean prepareForReplacement(BungeeMethod replacementMethod, long deadlineNanos) {
		// SocketHandler swallows a listener bind failure. A same-method replacement
		// must therefore retire its listener before the staged handler is started;
		// rollback recreates this prepared transport if validation later fails.
		if (method == BungeeMethod.SOCKETS && replacementMethod == BungeeMethod.SOCKETS) {
			transportManager.prepareForReplacement();
			return true;
		}
		if (method == BungeeMethod.MQTT && replacementMethod == BungeeMethod.MQTT) {
			// A duplicate MQTT ClientID disconnects the live broker session, so stage
			// only after retiring the predecessor and restore it on validation rollback.
			transportManager.prepareForReplacement();
			return true;
		}
		if (method == BungeeMethod.HTTP) {
			transportManager.prepareForReplacement();
			return true;
		}
		if (method == BungeeMethod.PLUGINMESSAGING) {
			transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
			return true;
		}
		if (method == BungeeMethod.REDIS && replacementMethod != BungeeMethod.REDIS) {
			if (transportManager.hasPendingAsyncHandoff())
				transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
			return transportManager.prepareRedisReplayTransition(replacementMethod, deadlineNanos);
		}
		if (method == BungeeMethod.REDIS) {
			// Same-Redis retirement installs its send fence during the bounded
			// off-thread handoff. Returning true here lets the staged replacement
			// buffer its own sends until the predecessor FIFO is admitted at Bukkit
			// publication.
			return true;
		}
		if (!transportManager.hasPendingAsyncHandoff()) return false;
		transportManager.prepareAsyncHandoffForReplacement(deadlineNanos);
		return true;
	}

	/** Atomically fences new sends only when disabling cannot discard prepared HTTP messages. */
	public boolean commitPreparedDisable() {
		return transportManager.commitPreparedDisable();
	}

	/** Publishes the final presence update while fencing other sends before transport preparation. */
	public void preparePresenceForDisable() {
		preparePresenceForDisableInternal(null);
	}

	/** Publishes the final presence update before the caller's validation deadline. */
	public void preparePresenceForDisable(long deadlineNanos) {
		preparePresenceForDisableInternal(deadlineNanos);
	}

	private void preparePresenceForDisableInternal(Long deadlineNanos) {
		transportManager.beginPreparedDisable();
		if (presenceManager != null && presenceReportingActivated) {
			// stopForDisable may reject when the transport cannot accept the final
			// presence update. Mark this inactive first so rollback can start it again.
			presenceReportingActivated = false;
			if (deadlineNanos == null) presenceManager.stopForDisable();
			else presenceManager.stopForDisable(deadlineNanos);
		}
	}

	/** Restores delivery and presence when a prepared disable is abandoned. */
	public void restorePresenceAfterFailedDisablePreparation() {
		transportManager.cancelPreparedDisable();
		activatePresenceReporting();
	}

	public void beginPreparedHttpHandoff() {
		transportManager.beginPreparedHttpHandoff();
	}

	/** Reserves staged HTTP capacity before publication can admit replacement sends. */
	public void reservePreparedHttpHandoff(BackendProxyHandler replacement) {
		transportManager.reservePreparedTransportHandoff(replacement.transportManager);
	}

	/** Restores a prepared HTTP transport when its replacement fails validation. */
	public void restoreAfterFailedReplacement() {
		transportManager.restoreAfterFailedReplacement();
	}

	/** Restores a failed same-Redis predecessor without discarding its replacement's replay FIFO. */
	public void restoreAfterFailedReplacement(BackendProxyHandler failedReplacement) {
		transportManager.restoreAfterFailedReplacement(
				failedReplacement == null ? null : failedReplacement.transportManager);
	}

	/** Reasserts the old handler with a fresh presence generation after rollback. */
	public void refreshPresenceAfterFailedReplacement() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceManager.start();
		}
	}

	public void awaitRestoreAfterFailedReplacement(long deadlineNanos) {
		transportManager.awaitPreparedTransportRestoration(deadlineNanos);
	}

	/** Fails a configuration apply when its selected transport did not initialize. */
	public void validateTransport() {
		validateTransport(System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(25));
	}

	/** Validates transport startup without extending the caller's existing deadline. */
	public void validateTransport(long deadlineNanos) {
		if (method == null || globalMessageHandler == null || presenceManager == null) {
			throw new IllegalStateException("Backend proxy handler initialization failed");
		}
		transportManager.validate(deadlineNanos);
	}

	/** Completes the no-loss/no-duplicate same-Redis subscriber handoff after validation. */
	public void completeRedisHandoff(BackendProxyHandler replacement) {
		if (!requiresRedisHandoff(replacement)) return;
		transportManager.completeRedisHandoff(replacement.transportManager);
	}

	/** Returns whether this handler owns a Redis listener whose shutdown can block. */
	public boolean requiresRedisRetirement() {
		return method == BungeeMethod.REDIS;
	}

	/** Returns whether this replacement needs the bounded same-Redis retirement path. */
	public boolean requiresRedisHandoff(BackendProxyHandler replacement) {
		return replacement != null && method == BungeeMethod.REDIS && replacement.method == BungeeMethod.REDIS;
	}

	/** Replays Redis handoff deliveries only after inbound publication is open. */
	public void replayRedisAfterHandoffPublication() {
		transportManager.replayRedisAfterHandoffPublication();
	}

	/** Forwards messages buffered while the previous transport was fenced. */
	public void completeHttpHandoff(BackendProxyHandler replacement) {
		if (replacement == null) return;
		transportManager.completePreparedTransportHandoff(replacement.transportManager);
	}

	public void playerOnline(String playerName, String uuid) {
		if (presenceManager != null) {
			presenceManager.playerOnline(playerName, uuid);
		}
	}

	public void playerOffline(String playerName) {
		if (presenceManager != null) {
			presenceManager.playerOffline(playerName);
		}
	}

	public void reloadPresenceReporting() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.reload();
		}
	}

	public void disablePresenceReporting() {
		if (presenceManager != null && presenceReportingActivated) {
			presenceManager.stop();
			presenceReportingActivated = false;
		}
	}

	public void loadGlobalMysql() {
		globalDataSync.load();
	}

	public GlobalDataHandler getGlobalDataHandler() {
		return globalDataSync.getGlobalDataHandler();
	}

	public void checkGlobalData() {
		globalDataSync.checkGlobalData();
	}

	public boolean checkGlobalDataTime(TimeType type, HashMap<String, DataValue> data) {
		return globalDataSync.checkGlobalDataTime(type, data);
	}

	public boolean checkGlobalDataTimeValue(DataValue data) {
		return globalDataSync.checkGlobalDataTimeValue(data);
	}

	public ConcurrentHashMap<UUID, Long> getProcessedWireVotes() {
		return processedVoteCache.getProcessedVotes();
	}

	public int getBungeeVotePartyCurrent() {
		return votePartySync == null ? plugin.getServerData().getBungeeVotePartyCurrent() : votePartySync.getCurrent();
	}

	public int getBungeeVotePartyRequired() {
		return votePartySync == null ? plugin.getServerData().getBungeeVotePartyRequired() : votePartySync.getRequired();
	}

	public ScheduledExecutorService getTimer() {
		return globalDataSync.getTimer();
	}

	public ClientHandler getClientHandler() {
		return transportManager.getClientHandler();
	}

	public SocketHandler getSocketHandler() {
		return transportManager.getSocketHandler();
	}

	public RedisHandler getRedisHandler() {
		return transportManager.getRedisHandler();
	}

	public MySqlMessenger getBackendMysqlMessenger() {
		return transportManager.getBackendMysqlMessenger();
	}

	public MqttHandler getMqttHandler() {
		return transportManager.getMqttHandler();
	}

	private void sendEnvelope(JsonEnvelope envelope) {
		if (globalMessageHandler != null) {
			globalMessageHandler.sendMessage(envelope);
		}
	}
}
