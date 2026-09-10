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
	static final int MAX_LEGACY_HANDOFF_DELIVERIES = 4096;
	static final int MAX_IDENTIFIED_HANDOFF_DELIVERIES = 4096;
	private static final long HANDOFF_QUIESCE_TIMEOUT_NANOS = TimeUnit.SECONDS.toNanos(3);

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
						if (deliveryId != null) {
							dispatchIdentified(envelope, deliveryId);
						} else {
							dispatchLegacy(envelope);
						}
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

	void dispatchIdentified(JsonEnvelope envelope, String deliveryId) {
		boolean accepted;
		synchronized (legacyLifecycle) {
			if (retiredAfterHandoff) return;
			if (standbySubscriber) {
				bufferIdentifiedDelivery(envelope, deliveryId);
				return;
			}
			accepted = processedVoteCache.reserveRedisDelivery(deliveryId);
			if (accepted && replayingHandoff) {
				deliveriesAfterReplay.addLast(envelope);
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
			if (retiredAfterHandoff) return;
			if (processedVoteCache.reserveLegacyRedisDelivery(subscriberIdentity, signature)) {
				dispatch = true;
			} else if (encodedBytes <= ProcessedVoteCache.MAX_LEGACY_REDIS_DELIVERY_BYTES
					&& bufferedLegacyDeliveries.size() < MAX_LEGACY_HANDOFF_DELIVERIES
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
				deliveriesAfterReplay.addLast(envelope);
				dispatch = false;
			}
			if (dispatch) dispatchesInFlight++;
		}
		if (dispatch) dispatchTracked(envelope);
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
			processedVoteCache.activateRedisSubscriber(subscriberIdentity);
			standbySubscriber = false;
			java.util.ArrayList<BufferedHandoffDelivery> buffered = new java.util.ArrayList<>(
					bufferedIdentifiedDeliveries.size() + bufferedLegacyDeliveries.size());
			buffered.addAll(bufferedIdentifiedDeliveries);
			buffered.addAll(bufferedLegacyDeliveries);
			buffered.sort(java.util.Comparator.comparingLong(BufferedHandoffDelivery::sequence));
			for (BufferedHandoffDelivery delivery : buffered) {
				try {
					boolean dispatch = delivery.identified()
							? processedVoteCache.reserveRedisDelivery(delivery.identity())
							: !processedVoteCache.consumeLegacyRedisDelivery(delivery.identity());
					if (dispatch) deliveriesAfterReplay.addLast(delivery.envelope());
				} catch (RuntimeException replayFailure) {
					if (plugin != null) plugin.debug("Redis handoff replay failed: " + replayFailure.getMessage());
				}
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
		java.util.ArrayList<JsonEnvelope> replay;
		while (true) {
			synchronized (legacyLifecycle) {
				if (deliveriesAfterReplay.isEmpty()) {
					replayingHandoff = false;
					break;
				}
				replay = new java.util.ArrayList<>(deliveriesAfterReplay);
				deliveriesAfterReplay.clear();
			}
			dispatchReplayBatch(replay);
		}
	}

	private void dispatchReplayBatch(java.util.List<JsonEnvelope> replay) {
		for (JsonEnvelope envelope : replay) {
			try {
				messageHandler.onMessage(envelope);
			} catch (RuntimeException replayFailure) {
				if (plugin != null) plugin.debug("Redis handoff replay failed: " + replayFailure.getMessage());
			}
		}
	}

	/** Stops the old listener while retaining its overlap accounting for standby promotion. */
	public void closeForHandoff() {
		handoffMessageHandler = messageHandler;
		fenceAfterHandoff();
		closeListener(false);
	}

	/** Reopens a fenced active subscriber when standby promotion is rejected. */
	public void restoreAfterFailedHandoff() {
		GlobalMessageHandler handler = handoffMessageHandler;
		if (handler == null) throw new IllegalStateException("Redis handoff transport cannot be restored");
		start(handler);
		handoffMessageHandler = null;
	}

	/** Prevents a listener that misses its shutdown deadline from dispatching duplicates. */
	void fenceAfterHandoff() {
		boolean interrupted = false;
		long deadline = System.nanoTime() + HANDOFF_QUIESCE_TIMEOUT_NANOS;
		synchronized (legacyLifecycle) {
			retiredAfterHandoff = true;
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
		closeListener(true);
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
