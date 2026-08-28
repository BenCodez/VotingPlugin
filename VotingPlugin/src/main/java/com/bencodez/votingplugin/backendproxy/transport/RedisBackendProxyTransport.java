package com.bencodez.votingplugin.backendproxy.transport;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.ArrayList;
import java.util.List;

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

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	@Getter
	private RedisHandler redisHandler;
	private CountDownLatch subscriptionReady;
	private Thread listenerThread;
	private final Object subscriberIdentity = new Object();
	private final Object legacyLifecycle = new Object();
	private final List<JsonEnvelope> bufferedLegacyDeliveries = new ArrayList<>();
	private GlobalMessageHandler messageHandler;

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
		processedVoteCache.registerRedisSubscriber(subscriberIdentity);
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
							if (processedVoteCache.reserveRedisDelivery(deliveryId)) messageHandler.onMessage(envelope);
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

	private void dispatchLegacy(JsonEnvelope envelope) {
		String signature = com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.encode(envelope);
		synchronized (legacyLifecycle) {
			if (processedVoteCache.reserveLegacyRedisDelivery(subscriberIdentity, signature)) {
				messageHandler.onMessage(envelope);
			} else {
				bufferedLegacyDeliveries.add(envelope);
			}
		}
	}

	/** Promotes a validated standby after the previous listener has completely stopped. */
	public void activateAfterHandoff() {
		synchronized (legacyLifecycle) {
			processedVoteCache.activateRedisSubscriber(subscriberIdentity);
			for (JsonEnvelope envelope : bufferedLegacyDeliveries) {
				String signature = com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.encode(envelope);
				if (!processedVoteCache.consumeLegacyRedisDelivery(signature)) messageHandler.onMessage(envelope);
			}
			bufferedLegacyDeliveries.clear();
			processedVoteCache.finishRedisHandoff();
		}
	}

	/** Stops the old listener while retaining its overlap accounting for standby promotion. */
	public void closeForHandoff() {
		closeListener(false);
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (redisHandler != null) {
			redisHandler.publishEnvelope(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin",
					VotingPluginWire.withRedisDeliveryId(envelope));
		}
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
