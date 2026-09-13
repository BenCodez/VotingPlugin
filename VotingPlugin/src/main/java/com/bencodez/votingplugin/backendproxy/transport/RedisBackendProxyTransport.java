package com.bencodez.votingplugin.backendproxy.transport;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLParameters;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import redis.clients.jedis.DefaultJedisClientConfig;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.Jedis;

import lombok.Getter;

public class RedisBackendProxyTransport implements BackendProxyTransport {
	public static final class HandoffQuiescenceException extends IllegalStateException {
		private static final long serialVersionUID = 1L;

		public HandoffQuiescenceException(String message) {
			super(message);
		}
	}

	public static final class HandoffReplayBackpressureException extends IllegalStateException {
		private static final long serialVersionUID = 1L;

		public HandoffReplayBackpressureException(String message) {
			super(message);
		}
	}
	static final int MAX_LEGACY_HANDOFF_DELIVERIES = 4096;
	static final int MAX_IDENTIFIED_HANDOFF_DELIVERIES = 4096;
	static final int MAX_REPLAY_HANDOFF_DELIVERIES = MAX_LEGACY_HANDOFF_DELIVERIES
			+ MAX_IDENTIFIED_HANDOFF_DELIVERIES;
	static final int REPLAY_BATCH_SIZE = 32;
	private static final long HANDOFF_QUIESCE_TIMEOUT_NANOS = TimeUnit.SECONDS.toNanos(3);
	private static final long HANDOFF_REPLAY_BACKPRESSURE_TIMEOUT_NANOS = TimeUnit.SECONDS.toNanos(3);
	private static final long REPLAY_BATCH_EXECUTION_TIMEOUT_NANOS = TimeUnit.SECONDS.toNanos(3);

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	@Getter
	private RedisHandler redisHandler;
	private CountDownLatch subscriptionReady;
	private Thread listenerThread;
	private final Object subscriberIdentity = new Object();
	private final Object legacyLifecycle = new Object();
	private final List<BufferedHandoffDelivery> bufferedLegacyDeliveries = new ArrayList<>();
	private final List<BufferedHandoffDelivery> bufferedIdentifiedDeliveries = new ArrayList<>();
	private long bufferedLegacyDeliveryBytes;
	private long bufferedIdentifiedDeliveryBytes;
	private long nextHandoffSequence;
	private boolean standbySubscriber;
	private boolean identifiedHandoffOverflowed;
	private boolean legacyHandoffOverflowed;
	private boolean legacyHandoffDegraded;
	private boolean retiredAfterHandoff;
	private boolean replayingHandoff;
	private boolean replayTransferFrozen;
	private boolean replayBackpressureFailureLogged;
	private long replayGeneration;
	private Thread replayWorker;
	private boolean replayTaskOutstanding;
	private long replayTaskGeneration = -1L;
	private int replayDeliveriesInFlight;
	// Counts only handlers which have already crossed the Bukkit dispatch fence.
	// It is deliberately independent of replayGeneration so cancellation cannot
	// erase accounting for a callback that was already executing.
	private int replayCallbacksInFlight;
	private int dispatchesInFlight;
	private final java.util.ArrayDeque<JsonEnvelope> deliveriesAfterReplay = new java.util.ArrayDeque<>();
	private GlobalMessageHandler messageHandler;
	private GlobalMessageHandler handoffMessageHandler;
	private String publishChannel;

	public RedisBackendProxyTransport(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache());
	}

	public RedisBackendProxyTransport(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.processedVoteCache = processedVoteCache;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		this.messageHandler = messageHandler;
		publishChannel = plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin";
		retiredAfterHandoff = false;
		standbySubscriber = !processedVoteCache.registerRedisSubscriber(subscriberIdentity);
		redisHandler = new RedisHandler(plugin.getBungeeSettings().getRedisHost(),
				plugin.getBungeeSettings().getRedisPort(), plugin.getBungeeSettings().getRedisUsername(),
				plugin.getBungeeSettings().getRedisPassword(), plugin.getBungeeSettings().getRedisdbindex(),
				plugin.getBungeeSettings().isRedisSsl()) {
			@Override
			public void debug(String message) {
				if (plugin.getBungeeSettings().isBungeeDebug()) {
					plugin.debug(message);
				}
			}
		};
		RedisHandler handler = redisHandler;
		CountDownLatch ready = new CountDownLatch(1);
		subscriptionReady = ready;
		RedisListener listener = new RedisListener(handler,
				plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin_" + plugin.getBungeeSettings().getServer(),
				(ch, payload) -> {
					try {
						JsonEnvelope envelope = com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.decode(payload);
						String deliveryId = envelope.getFields().get(VotingPluginWire.K_REDIS_DELIVERY_ID);
						dispatchReceivedSubscriberEnvelope(envelope, deliveryId);
					} catch (Exception e) {
						plugin.debug("Redis decode failed: " + e.getMessage());
					}
				}) {
			@Override
			public void onSubscribe(String channel, int subscribedChannels) {
				ready.countDown();
			}
		};
		listenerThread = new Thread(() -> handler.loadListener(listener), "VotingPlugin-Redis-Backend");
		listenerThread.setDaemon(true);
		listenerThread.start();
	}

	/**
	 * Redis Pub/Sub has already consumed this payload when its callback is invoked.
	 * A bounded queue wait therefore cannot discard it: retry the same callback in
	 * place, applying TCP/Pub/Sub backpressure until FIFO replay capacity returns or
	 * this subscriber is deliberately fenced during shutdown/rollback.
	 */
	void dispatchReceivedSubscriberEnvelope(JsonEnvelope envelope, String deliveryId) {
		while (true) {
			try {
				if (deliveryId != null) dispatchIdentified(envelope, deliveryId);
				else dispatchLegacy(envelope);
				return;
			} catch (HandoffReplayBackpressureException retry) {
				synchronized (legacyLifecycle) {
					if (retiredAfterHandoff) return;
				}
				if (plugin != null)
					plugin.debug("Redis handoff replay remains full; retaining the received Pub/Sub payload for retry");
			}
		}
	}

	void dispatchIdentified(JsonEnvelope envelope, String deliveryId) {
		boolean accepted;
		synchronized (legacyLifecycle) {
			if (!awaitReplayTransferResolution()) return;
			if (retiredAfterHandoff) return;
			awaitReplayCapacity();
			if (retiredAfterHandoff) return;
			if (standbySubscriber) {
				bufferIdentifiedDelivery(envelope, deliveryId);
				return;
			}
			accepted = processedVoteCache.reserveRedisDelivery(deliveryId);
			if (accepted && replayingHandoff) {
				enqueueReplayDelivery(envelope);
				accepted = false;
			}
			if (accepted) dispatchesInFlight++;
		}
		if (accepted) dispatchTracked(envelope);
	}

	void dispatchLegacy(JsonEnvelope envelope) {
		String signature = com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.encode(envelope);
		int encodedBytes = ProcessedVoteCache.legacyRedisDeliveryBytes(signature);
		boolean dispatch = false;
		synchronized (legacyLifecycle) {
			if (!awaitReplayTransferResolution()) return;
			if (retiredAfterHandoff) return;
			awaitReplayCapacity();
			if (retiredAfterHandoff) return;
			if (processedVoteCache.reserveLegacyRedisDelivery(subscriberIdentity, signature)) {
				dispatch = true;
			} else if (encodedBytes <= ProcessedVoteCache.MAX_LEGACY_REDIS_DELIVERY_BYTES
					&& bufferedLegacyDeliveries.size() < MAX_LEGACY_HANDOFF_DELIVERIES
					&& handoffDeliveryCount() < MAX_REPLAY_HANDOFF_DELIVERIES
					&& bufferedLegacyDeliveryBytes <= ProcessedVoteCache.MAX_LEGACY_REDIS_TOTAL_BYTES - encodedBytes) {
				bufferedLegacyDeliveries.add(new BufferedHandoffDelivery(
						nextHandoffSequence++, envelope, signature, false));
				bufferedLegacyDeliveryBytes += encodedBytes;
			} else {
				if (standbySubscriber) {
					if (!legacyHandoffOverflowed && plugin != null)
						plugin.getLogger().warning("Redis legacy handoff buffer is full; aborting the staged handoff");
					legacyHandoffOverflowed = true;
				} else {
					if (!legacyHandoffDegraded && plugin != null) {
						plugin.getLogger().warning("Redis legacy handoff exceeded its " + MAX_LEGACY_HANDOFF_DELIVERIES
								+ " delivery / " + ProcessedVoteCache.MAX_LEGACY_REDIS_TOTAL_BYTES
								+ " byte buffer; temporarily degrading duplicate suppression");
					}
					legacyHandoffDegraded = true;
					dispatch = true;
				}
			}
			if (dispatch && replayingHandoff) {
				enqueueReplayDelivery(envelope);
				dispatch = false;
			}
			if (dispatch) dispatchesInFlight++;
		}
		if (dispatch) dispatchTracked(envelope);
	}

	/** Holds a consumed subscriber callback until a same-Redis transfer commits or rolls back. */
	private boolean awaitReplayTransferResolution() {
		boolean interrupted = false;
		while (replayTransferFrozen) {
			try {
				legacyLifecycle.wait();
			} catch (InterruptedException waitInterrupted) {
				// Do not drop a Pub/Sub payload merely because the listener was nudged
				// during transfer. Its final owner will explicitly release this wait.
				interrupted = true;
			}
		}
		if (interrupted) Thread.currentThread().interrupt();
		return !retiredAfterHandoff;
	}

	private void dispatchTracked(JsonEnvelope envelope) {
		try {
			messageHandler.onMessage(envelope);
		} finally {
			synchronized (legacyLifecycle) {
				dispatchesInFlight--;
				legacyLifecycle.notifyAll();
			}
		}
	}

	/** Promotes a validated standby after the previous listener has completely stopped. */
	public void activateAfterHandoff() {
		synchronized (legacyLifecycle) {
			if (identifiedHandoffOverflowed || legacyHandoffOverflowed
					|| processedVoteCache.isLegacyRedisHandoffOverflowed())
				throw new IllegalStateException("Redis handoff buffer overflowed before publication");
			replayingHandoff = true;
			replayBackpressureFailureLogged = false;
			processedVoteCache.activateRedisSubscriber(subscriberIdentity);
			standbySubscriber = false;
			java.util.ArrayList<BufferedHandoffDelivery> buffered = new java.util.ArrayList<>(
					bufferedIdentifiedDeliveries.size() + bufferedLegacyDeliveries.size());
			buffered.addAll(bufferedIdentifiedDeliveries);
			buffered.addAll(bufferedLegacyDeliveries);
			buffered.sort(java.util.Comparator.comparingLong(BufferedHandoffDelivery::sequence));
			for (BufferedHandoffDelivery delivery : buffered) {
				boolean dispatch;
				try {
					dispatch = delivery.identified()
							? processedVoteCache.reserveRedisDelivery(delivery.identity())
							: !processedVoteCache.consumeLegacyRedisDelivery(delivery.identity());
				} catch (RuntimeException replayFailure) {
					if (plugin != null) plugin.debug("Redis handoff replay failed: " + replayFailure.getMessage());
					continue;
				}
				if (dispatch) enqueueReplayDelivery(delivery.envelope());
			}
			bufferedIdentifiedDeliveries.clear();
			bufferedIdentifiedDeliveryBytes = 0;
			bufferedLegacyDeliveries.clear();
			bufferedLegacyDeliveryBytes = 0;
			identifiedHandoffOverflowed = false;
			legacyHandoffOverflowed = false;
			legacyHandoffDegraded = false;
			processedVoteCache.finishRedisHandoff();
		}
	}

	/** Replays buffered deliveries after the owning handler opens its publication gate. */
	public void replayAfterHandoffPublication() {
		if (plugin == null) {
			// Unit tests use a transport without a Bukkit scheduler. Production always
			// takes the bounded worker path below.
			drainReplayWithoutScheduler();
			return;
		}
		startReplayWorkerIfNeeded();
	}

	/** True while a same-Redis replay still owns accepted envelopes or Bukkit work. */
	public boolean hasPendingReplayForReplacement() {
		synchronized (legacyLifecycle) {
			return replayingHandoff || replayTransferFrozen || !deliveriesAfterReplay.isEmpty()
					|| replayDeliveriesInFlight != 0 || replayCallbacksInFlight != 0 || replayTaskOutstanding;
		}
	}

	/**
	 * Waits only on the Control/validation worker for active replay to finish
	 * before a non-Redis replacement can retire this transport. It never moves
	 * Redis envelopes into a transport that cannot preserve Redis semantics.
	 */
	public boolean awaitReplayDrainForNonRedisReplacement(long deadlineNanos) {
		boolean interrupted = false;
		synchronized (legacyLifecycle) {
			while (hasPendingReplayForReplacement()) {
				long remaining = deadlineNanos - System.nanoTime();
				if (remaining <= 0L) {
					if (interrupted) Thread.currentThread().interrupt();
					return false;
				}
				try {
					TimeUnit.NANOSECONDS.timedWait(legacyLifecycle, remaining);
				} catch (InterruptedException waitInterrupted) {
					interrupted = true;
					break;
				}
			}
		}
		if (interrupted) Thread.currentThread().interrupt();
		return !interrupted;
	}

	private void startReplayWorkerIfNeeded() {
		Thread worker;
		synchronized (legacyLifecycle) {
			if (!replayingHandoff || replayTransferFrozen || replayWorker != null || replayTaskOutstanding) return;
			worker = new Thread(this::drainReplayOnWorker, "VotingPlugin-Redis-Backend-Replay");
			worker.setDaemon(true);
			replayWorker = worker;
		}
		try {
			worker.start();
		} catch (RuntimeException failed) {
			synchronized (legacyLifecycle) {
				if (replayWorker == worker) replayWorker = null;
				legacyLifecycle.notifyAll();
			}
			throw failed;
		}
	}

	private void enqueueReplayDelivery(JsonEnvelope envelope) {
		if (deliveriesAfterReplay.size() + replayDeliveriesInFlight >= MAX_REPLAY_HANDOFF_DELIVERIES)
			throw new IllegalStateException("Redis handoff replay capacity was not reserved");
		deliveriesAfterReplay.addLast(envelope);
	}

	/**
	 * Preserves handoff FIFO without allowing the replay queue to grow without
	 * bound. A timeout is explicit and happens before the identified ID is
	 * reserved, so an undeliverable callback is never silently marked processed.
	 */
	private void awaitReplayCapacity() {
		if (!replayingHandoff) return;
		boolean interrupted = false;
		long deadline = System.nanoTime() + HANDOFF_REPLAY_BACKPRESSURE_TIMEOUT_NANOS;
		while (replayingHandoff && deliveriesAfterReplay.size() + replayDeliveriesInFlight
				>= MAX_REPLAY_HANDOFF_DELIVERIES) {
			long remaining = deadline - System.nanoTime();
			if (remaining <= 0L) {
				if (!replayBackpressureFailureLogged && plugin != null) {
					plugin.getLogger().severe("Redis handoff replay is not draining; rejected an unreserved callback");
					replayBackpressureFailureLogged = true;
				}
				if (interrupted) Thread.currentThread().interrupt();
				throw new HandoffReplayBackpressureException("Redis handoff replay queue did not drain before its deadline");
			}
			try {
				TimeUnit.NANOSECONDS.timedWait(legacyLifecycle, remaining);
			} catch (InterruptedException waitInterrupted) {
				interrupted = true;
				break;
			}
		}
		if (interrupted) {
			Thread.currentThread().interrupt();
			throw new HandoffReplayBackpressureException("Interrupted while waiting for Redis handoff replay capacity");
		}
	}

	private void drainReplayWithoutScheduler() {
		while (true) {
			java.util.ArrayList<JsonEnvelope> replay;
			GlobalMessageHandler handler;
			long generation;
			synchronized (legacyLifecycle) {
				if (!replayingHandoff || deliveriesAfterReplay.isEmpty()) {
					replayingHandoff = false;
					legacyLifecycle.notifyAll();
					return;
				}
				replay = takeReplayBatch();
				handler = messageHandler;
				generation = replayGeneration;
			}
			dispatchReplayBatch(replay, handler, generation);
			completeReplayBatch(replay.size());
		}
	}

	/** Coordinates one bounded Bukkit batch at a time without blocking publication. */
	private void drainReplayOnWorker() {
		try {
			while (!Thread.currentThread().isInterrupted()) {
				java.util.ArrayList<JsonEnvelope> replay;
				GlobalMessageHandler handler;
				long generation;
				synchronized (legacyLifecycle) {
					if (!replayingHandoff || deliveriesAfterReplay.isEmpty()) {
						replayingHandoff = false;
						legacyLifecycle.notifyAll();
						return;
					}
					if (replayTransferFrozen) return;
					replay = takeReplayBatch();
					handler = messageHandler;
					generation = replayGeneration;
				}
				ReplayBatch batch = new ReplayBatch(generation);
				java.util.concurrent.CountDownLatch completed = new java.util.concurrent.CountDownLatch(1);
				try {
					synchronized (legacyLifecycle) {
						replayTaskOutstanding = true;
						replayTaskGeneration = batch.generation;
					}
					plugin.getBukkitScheduler().runTask(plugin, () -> {
						try {
							synchronized (legacyLifecycle) {
								if (batch.generation != replayGeneration || batch.returned || !replayingHandoff) return;
								if (replayTransferFrozen) return;
								batch.started = true;
							}
							dispatchReplayBatch(replay, handler, batch.generation);
						} finally {
							synchronized (legacyLifecycle) {
								if (batch.generation == replayGeneration && !batch.returned) {
									if (!batch.started && replayTransferFrozen) {
										batch.returned = true;
										requeueReplayBatch(replay);
									} else completeReplayBatch(replay.size());
								}
								if (replayTaskOutstanding && replayTaskGeneration == batch.generation) {
									replayTaskOutstanding = false;
									replayTaskGeneration = -1L;
								}
								legacyLifecycle.notifyAll();
							}
							completed.countDown();
							startReplayWorkerIfNeeded();
						}
					});
					if (!completed.await(REPLAY_BATCH_EXECUTION_TIMEOUT_NANOS, TimeUnit.NANOSECONDS)) {
						synchronized (legacyLifecycle) {
							if (!batch.started && !batch.returned && batch.generation == replayGeneration) {
								batch.returned = true;
								replayGeneration++;
								requeueReplayBatch(replay);
								// Do not leave replay owned by a Bukkit task that was accepted but
								// never began. A task already running must retain ownership until its
								// finally block completes; otherwise a second worker could overlap it
								// and the older task could clear the newer task's shared state.
								if (replayTaskOutstanding && replayTaskGeneration == batch.generation) {
									replayTaskOutstanding = false;
									replayTaskGeneration = -1L;
								}
							}
							legacyLifecycle.notifyAll();
						}
						return;
					}
				} catch (InterruptedException interrupted) {
					synchronized (legacyLifecycle) {
						// A batch is removed before it can be scheduled. If the worker is
						// interrupted while waiting for Bukkit, put an unstarted batch back
						// at the front; otherwise the accepted deliveries would vanish.
						if (!batch.started && !batch.returned && batch.generation == replayGeneration) {
							batch.returned = true;
							requeueReplayBatch(replay);
							if (replayTaskOutstanding && replayTaskGeneration == batch.generation) {
								replayTaskOutstanding = false;
								replayTaskGeneration = -1L;
							}
						}
						legacyLifecycle.notifyAll();
					}
					Thread.currentThread().interrupt();
					return;
				} catch (RuntimeException schedulingFailure) {
					synchronized (legacyLifecycle) {
						if (!batch.returned && batch.generation == replayGeneration) {
							batch.returned = true;
							requeueReplayBatch(replay);
						}
						if (replayTaskOutstanding && replayTaskGeneration == batch.generation) {
							replayTaskOutstanding = false;
							replayTaskGeneration = -1L;
						}
						legacyLifecycle.notifyAll();
					}
					if (plugin != null) plugin.debug("Redis handoff replay scheduling failed: " + schedulingFailure.getMessage());
					try {
						Thread.sleep(250L);
					} catch (InterruptedException interrupted) {
						Thread.currentThread().interrupt();
						return;
					}
				}
			}
		} finally {
			synchronized (legacyLifecycle) {
				if (replayWorker == Thread.currentThread()) replayWorker = null;
				legacyLifecycle.notifyAll();
			}
			startReplayWorkerIfNeeded();
		}
	}

	private java.util.ArrayList<JsonEnvelope> takeReplayBatch() {
		java.util.ArrayList<JsonEnvelope> replay = new java.util.ArrayList<>(REPLAY_BATCH_SIZE);
		while (!deliveriesAfterReplay.isEmpty() && replay.size() < REPLAY_BATCH_SIZE)
			replay.add(deliveriesAfterReplay.removeFirst());
		replayDeliveriesInFlight += replay.size();
		return replay;
	}

	private void completeReplayBatch(int size) {
		synchronized (legacyLifecycle) {
			replayDeliveriesInFlight -= size;
			if (replayDeliveriesInFlight < 0) replayDeliveriesInFlight = 0;
			legacyLifecycle.notifyAll();
		}
	}

	private void requeueReplayBatch(java.util.List<JsonEnvelope> replay) {
		for (int index = replay.size() - 1; index >= 0; index--) deliveriesAfterReplay.addFirst(replay.get(index));
		completeReplayBatch(replay.size());
	}

	private static final class ReplayBatch {
		private final long generation;
		private boolean started;
		private boolean returned;

		private ReplayBatch(long generation) {
			this.generation = generation;
		}
	}

	private void dispatchReplayBatch(java.util.List<JsonEnvelope> replay, GlobalMessageHandler handler,
			long generation) {
		if (handler == null) return;
		for (JsonEnvelope envelope : replay) {
			synchronized (legacyLifecycle) {
				// A close/fence may happen after Bukkit accepts the batch. Never let a
				// stale generation start another callback after that point.
				if (!replayingHandoff || generation != replayGeneration) return;
				replayCallbacksInFlight++;
			}
			try {
				handler.onMessage(envelope);
			} catch (RuntimeException replayFailure) {
				if (plugin != null) plugin.debug("Redis handoff replay failed: " + replayFailure.getMessage());
			} finally {
				synchronized (legacyLifecycle) {
					replayCallbacksInFlight--;
					if (replayCallbacksInFlight < 0) replayCallbacksInFlight = 0;
					legacyLifecycle.notifyAll();
				}
			}
		}
	}

	/** Stops the old listener while retaining its overlap accounting for standby promotion. */
	public void closeForHandoff() {
		handoffMessageHandler = messageHandler;
		fenceAfterHandoff();
		// A successful fence guarantees this listener will not dispatch the held
		// callback. Release it before listener shutdown so the Redis listener thread
		// can return and join promptly; the overlapping standby owns its duplicate.
		completeFrozenReplayTransfer();
		closeListener(false);
	}

	/** Reopens a fenced active subscriber when standby promotion is rejected. */
	public void restoreAfterFailedHandoff() {
		restoreAfterFailedHandoff(java.util.Collections.emptyList());
	}

	/**
	 * Reinstates the retired subscriber and prepends deliveries accepted by a
 * promoted replacement before publication later failed. Standby overlap IDs
 * are resolved while detaching them, so this FIFO can replay directly exactly
 * once after the old subscriber is restored.
	 */
	public void restoreAfterFailedHandoff(java.util.List<JsonEnvelope> replacementReplay) {
		GlobalMessageHandler handler = handoffMessageHandler;
		if (handler == null) throw new IllegalStateException("Redis handoff transport cannot be restored");
		Thread retiredListener = listenerThread;
		if (retiredListener != null && retiredListener.isAlive())
			throw new IllegalStateException("Redis handoff listener did not stop before rollback restart");
		synchronized (legacyLifecycle) {
			if (replacementReplay.size() > MAX_REPLAY_HANDOFF_DELIVERIES)
				throw new IllegalStateException("Redis rollback replay exceeds its bounded handoff capacity");
			deliveriesAfterReplay.clear();
			deliveriesAfterReplay.addAll(replacementReplay);
			replayDeliveriesInFlight = 0;
			replayingHandoff = !replacementReplay.isEmpty();
			replayGeneration++;
			retiredAfterHandoff = false;
		}
		processedVoteCache.restoreRedisSubscriber(subscriberIdentity);
		start(handler);
		if (!replacementReplay.isEmpty()) replayAfterHandoffPublication();
		handoffMessageHandler = null;
	}

	/** Transfers pre-publication replay ownership to a restored predecessor. */
	public java.util.List<JsonEnvelope> detachReplayForFailedHandoff() {
		synchronized (legacyLifecycle) {
			if (replayDeliveriesInFlight != 0 || replayCallbacksInFlight != 0)
				throw new IllegalStateException("Redis rollback cannot detach a replay batch already executing");
			java.util.ArrayList<BufferedHandoffDelivery> buffered = new java.util.ArrayList<>(
					bufferedIdentifiedDeliveries.size() + bufferedLegacyDeliveries.size());
			buffered.addAll(bufferedIdentifiedDeliveries);
			buffered.addAll(bufferedLegacyDeliveries);
			buffered.sort(java.util.Comparator.comparingLong(BufferedHandoffDelivery::sequence));
			if (deliveriesAfterReplay.size() + buffered.size() > MAX_REPLAY_HANDOFF_DELIVERIES)
				throw new IllegalStateException("Redis rollback replay exceeds its bounded handoff capacity");
			java.util.ArrayList<JsonEnvelope> pending = new java.util.ArrayList<>(
					deliveriesAfterReplay.size() + buffered.size());
			pending.addAll(deliveriesAfterReplay);
			for (BufferedHandoffDelivery delivery : buffered) {
				boolean replay;
				try {
					replay = delivery.identified()
							? processedVoteCache.reserveRedisDelivery(delivery.identity())
							: !processedVoteCache.consumeLegacyRedisDelivery(delivery.identity());
				} catch (RuntimeException cacheFailure) {
					// A rollback must keep an accepted staged callback. Retain it for the
					// restored subscriber rather than letting a transient dedupe failure
					// turn replacement close into data loss.
					replay = true;
					if (plugin != null) plugin.debug("Redis rollback dedupe failed: " + cacheFailure.getMessage());
				}
				if (replay) pending.add(delivery.envelope());
			}
			retiredAfterHandoff = true;
			replayingHandoff = false;
			replayTransferFrozen = false;
			replayGeneration++;
			deliveriesAfterReplay.clear();
			bufferedIdentifiedDeliveries.clear();
			bufferedIdentifiedDeliveryBytes = 0;
			bufferedLegacyDeliveries.clear();
			bufferedLegacyDeliveryBytes = 0;
			identifiedHandoffOverflowed = false;
			legacyHandoffOverflowed = false;
			legacyHandoffDegraded = false;
			replayTaskOutstanding = false;
			replayTaskGeneration = -1L;
			legacyLifecycle.notifyAll();
			return pending;
		}
	}

	/**
	 * Stops admitting new callbacks and transfers the remaining active replay FIFO
	 * to the next same-Redis standby. An already-started Bukkit batch is allowed
	 * to finish; an unstarted scheduled batch is returned to the deque first.
	 */
	public java.util.List<JsonEnvelope> freezeReplayForSuccessiveHandoff() {
		boolean interrupted = false;
		long deadline = System.nanoTime() + HANDOFF_QUIESCE_TIMEOUT_NANOS;
		synchronized (legacyLifecycle) {
			replayTransferFrozen = true;
			retiredAfterHandoff = true;
			Thread worker = replayWorker;
			if (worker != null) worker.interrupt();
			while (replayDeliveriesInFlight != 0 || replayCallbacksInFlight != 0 || replayTaskOutstanding) {
				long remaining = deadline - System.nanoTime();
				if (remaining <= 0L) {
					resumeReplayAfterFailedSuccessiveHandoffLocked();
					if (interrupted) Thread.currentThread().interrupt();
					throw new HandoffQuiescenceException("Redis replay did not quiesce before successive handoff");
				}
				try {
					TimeUnit.NANOSECONDS.timedWait(legacyLifecycle, remaining);
				} catch (InterruptedException waitInterrupted) {
					interrupted = true;
					resumeReplayAfterFailedSuccessiveHandoffLocked();
					Thread.currentThread().interrupt();
					throw new HandoffQuiescenceException("Interrupted while freezing Redis replay for successive handoff");
				}
			}
			java.util.ArrayList<JsonEnvelope> pending = new java.util.ArrayList<>(deliveriesAfterReplay);
			deliveriesAfterReplay.clear();
			replayingHandoff = false;
			replayGeneration++;
			legacyLifecycle.notifyAll();
			if (interrupted) Thread.currentThread().interrupt();
			return pending;
		}
	}

	/** Prepends a predecessor's still-unplayed replay FIFO before this standby's own overlap buffer. */
	public void acceptReplayFromPreviousHandoff(java.util.List<JsonEnvelope> predecessorReplay) {
		if (predecessorReplay == null || predecessorReplay.isEmpty()) return;
		synchronized (legacyLifecycle) {
			if (deliveriesAfterReplay.size() + replayDeliveriesInFlight + bufferedLegacyDeliveries.size()
					+ bufferedIdentifiedDeliveries.size() + predecessorReplay.size()
					> MAX_REPLAY_HANDOFF_DELIVERIES)
				throw new IllegalStateException("Redis replacement replay capacity is exhausted");
			for (JsonEnvelope envelope : predecessorReplay) deliveriesAfterReplay.addLast(envelope);
		}
	}

	/** Rolls back a not-yet-published predecessor transfer when its old listener cannot retire. */
	public void removeReplayFromPreviousHandoff(java.util.List<JsonEnvelope> predecessorReplay) {
		if (predecessorReplay == null || predecessorReplay.isEmpty()) return;
		synchronized (legacyLifecycle) {
			if (deliveriesAfterReplay.size() < predecessorReplay.size())
				throw new IllegalStateException("Redis replacement replay transfer is incomplete");
			for (JsonEnvelope expected : predecessorReplay) {
				JsonEnvelope actual = deliveriesAfterReplay.removeFirst();
				if (!java.util.Objects.equals(actual, expected))
					throw new IllegalStateException("Redis replacement replay FIFO changed during handoff rollback");
			}
		}
	}

	/** Finalizes a successful transfer and releases callbacks to the promoted standby overlap. */
	public void completeFrozenReplayTransfer() {
		synchronized (legacyLifecycle) {
			replayTransferFrozen = false;
			legacyLifecycle.notifyAll();
		}
	}

	/** Restores the frozen active FIFO when successor admission or retirement fails. */
	public void restoreFrozenReplayAfterFailedSuccessiveHandoff(java.util.List<JsonEnvelope> replay) {
		synchronized (legacyLifecycle) {
			if (!deliveriesAfterReplay.isEmpty())
				throw new IllegalStateException("Redis replay ownership changed while successive handoff was aborted");
			deliveriesAfterReplay.addAll(replay);
			resumeReplayAfterFailedSuccessiveHandoffLocked();
		}
		if (plugin != null) startReplayWorkerIfNeeded();
	}

	private void resumeReplayAfterFailedSuccessiveHandoffLocked() {
		replayTransferFrozen = false;
		retiredAfterHandoff = false;
		if (!deliveriesAfterReplay.isEmpty()) replayingHandoff = true;
		legacyLifecycle.notifyAll();
	}

	/** Prevents a listener that misses its shutdown deadline from dispatching duplicates. */
	void fenceAfterHandoff() {
		boolean interrupted = false;
		long deadline = System.nanoTime() + HANDOFF_QUIESCE_TIMEOUT_NANOS;
		synchronized (legacyLifecycle) {
			retiredAfterHandoff = true;
			replayGeneration++;
			Thread worker = replayWorker;
			if (worker != null) worker.interrupt();
			replayWorker = null;
			while (dispatchesInFlight > 0) {
				long remaining = deadline - System.nanoTime();
				if (remaining <= 0) break;
				try {
					TimeUnit.NANOSECONDS.timedWait(legacyLifecycle, remaining);
				} catch (InterruptedException waitInterrupted) {
					interrupted = true;
					break;
				}
			}
			if (dispatchesInFlight > 0) {
				retiredAfterHandoff = false;
				if (interrupted) Thread.currentThread().interrupt();
				throw new HandoffQuiescenceException(
						"Redis backend callbacks did not quiesce before handoff");
			}
			replayingHandoff = false;
			deliveriesAfterReplay.clear();
			replayBackpressureFailureLogged = false;
			replayTaskOutstanding = false;
			replayTaskGeneration = -1L;
			replayDeliveriesInFlight = 0;
			bufferedLegacyDeliveries.clear();
			bufferedLegacyDeliveryBytes = 0;
			bufferedIdentifiedDeliveries.clear();
			bufferedIdentifiedDeliveryBytes = 0;
			identifiedHandoffOverflowed = false;
			legacyHandoffOverflowed = false;
		}
		if (interrupted) Thread.currentThread().interrupt();
	}

	private void bufferIdentifiedDelivery(JsonEnvelope envelope, String deliveryId) {
		String signature = com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.encode(envelope);
		int encodedBytes = ProcessedVoteCache.legacyRedisDeliveryBytes(signature);
		if (encodedBytes <= ProcessedVoteCache.MAX_LEGACY_REDIS_DELIVERY_BYTES
				&& bufferedIdentifiedDeliveries.size() < MAX_IDENTIFIED_HANDOFF_DELIVERIES
				&& handoffDeliveryCount() < MAX_REPLAY_HANDOFF_DELIVERIES
				&& bufferedIdentifiedDeliveryBytes <= ProcessedVoteCache.MAX_LEGACY_REDIS_TOTAL_BYTES - encodedBytes) {
			bufferedIdentifiedDeliveries.add(new BufferedHandoffDelivery(
					nextHandoffSequence++, envelope, deliveryId, true));
			bufferedIdentifiedDeliveryBytes += encodedBytes;
		} else {
			if (plugin != null && !identifiedHandoffOverflowed)
				plugin.getLogger().warning("Redis identified handoff buffer is full; aborting the staged handoff");
			identifiedHandoffOverflowed = true;
		}
	}

	/** Includes inherited replay plus staged overlap entries so rollback remains bounded. */
	private int handoffDeliveryCount() {
		return deliveriesAfterReplay.size() + replayDeliveriesInFlight
				+ bufferedIdentifiedDeliveries.size() + bufferedLegacyDeliveries.size();
	}

	private record BufferedHandoffDelivery(long sequence, JsonEnvelope envelope, String identity,
			boolean identified) {}

	@Override
	public boolean send(JsonEnvelope envelope) {
		if (redisHandler != null) {
			redisHandler.publishEnvelope(publishChannel,
					VotingPluginWire.withRedisDeliveryId(envelope));
			return true;
		}
		return false;
	}

	@Override
	public void validate() {
		if (redisHandler == null) throw new IllegalStateException("Redis backend proxy transport initialization failed");
		try (Jedis jedis = new Jedis(new HostAndPort(plugin.getBungeeSettings().getRedisHost(),
				plugin.getBungeeSettings().getRedisPort()), buildValidationClientConfig(
						plugin.getBungeeSettings().getRedisdbindex(), plugin.getBungeeSettings().getRedisUsername(),
						plugin.getBungeeSettings().getRedisPassword(), plugin.getBungeeSettings().isRedisSsl()))) {
			if (!"PONG".equalsIgnoreCase(jedis.ping())) {
				throw new IllegalStateException("Redis backend proxy transport did not answer PING");
			}
		} catch (RuntimeException failure) {
			throw new IllegalStateException("Redis backend proxy transport connection failed", failure);
		}
		try {
			if (subscriptionReady == null || !subscriptionReady.await(3, TimeUnit.SECONDS)) {
				throw new IllegalStateException("Redis backend proxy subscription did not become ready");
			}
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted while waiting for Redis backend proxy subscription", e);
		}
	}

	static DefaultJedisClientConfig buildValidationClientConfig(int database, String username, String password,
			boolean ssl) {
		DefaultJedisClientConfig.Builder config = DefaultJedisClientConfig.builder().database(database).ssl(ssl)
				.connectionTimeoutMillis(2000).socketTimeoutMillis(2000);
		if (ssl) {
			SSLParameters sslParameters = new SSLParameters();
			sslParameters.setEndpointIdentificationAlgorithm("HTTPS");
			config.sslParameters(sslParameters);
		}
		if (username != null && !username.isEmpty()) config.user(username);
		if (password != null && !password.isEmpty()) config.password(password);
		return config.build();
	}

	@Override
	public void close() {
		cancelReplay();
		closeListener(true);
	}

	private void cancelReplay() {
		synchronized (legacyLifecycle) {
			retiredAfterHandoff = true;
			replayGeneration++;
			replayingHandoff = false;
			replayTransferFrozen = false;
			deliveriesAfterReplay.clear();
			replayBackpressureFailureLogged = false;
			replayTaskOutstanding = false;
			replayTaskGeneration = -1L;
			replayDeliveriesInFlight = 0;
			Thread worker = replayWorker;
			replayWorker = null;
			if (worker != null) worker.interrupt();
			legacyLifecycle.notifyAll();
		}
	}

	private void closeListener(boolean unregister) {
		Thread thread = listenerThread;
		if (redisHandler != null) {
			redisHandler.close();
			redisHandler = null;
		}
		if (thread != null) {
			thread.interrupt();
			try {
				thread.join(TimeUnit.SECONDS.toMillis(3));
			} catch (InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Interrupted while stopping Redis backend subscription", e);
			}
			if (thread.isAlive()) throw new IllegalStateException("Redis backend subscription did not stop");
		}
		listenerThread = null;
		subscriptionReady = null;
		messageHandler = null;
		if (unregister) processedVoteCache.unregisterRedisSubscriber(subscriberIdentity);
	}
}
