package com.bencodez.votingplugin.backendproxy.transport;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.votingplugin.VotingPluginMain;

import redis.clients.jedis.DefaultJedisClientConfig;
import redis.clients.jedis.HostAndPort;
import redis.clients.jedis.Jedis;

import lombok.Getter;

public class RedisBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private RedisHandler redisHandler;
	private CountDownLatch subscriptionReady;

	public RedisBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
		redisHandler = new RedisHandler(plugin.getBungeeSettings().getRedisHost(),
				plugin.getBungeeSettings().getRedisPort(), plugin.getBungeeSettings().getRedisUsername(),
				plugin.getBungeeSettings().getRedisPassword(), plugin.getBungeeSettings().getRedisdbindex()) {
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
						messageHandler.onMessage(
								com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec.decode(payload));
					} catch (Exception e) {
						plugin.debug("Redis decode failed: " + e.getMessage());
					}
				}) {
			@Override
			public void onSubscribe(String channel, int subscribedChannels) {
				ready.countDown();
			}
		};
		handler.loadListener(listener);
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (redisHandler != null) {
			redisHandler.publishEnvelope(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin", envelope);
		}
	}

	@Override
	public void validate() {
		if (redisHandler == null) throw new IllegalStateException("Redis backend proxy transport initialization failed");
		DefaultJedisClientConfig.Builder config = DefaultJedisClientConfig.builder()
				.database(plugin.getBungeeSettings().getRedisdbindex())
				.connectionTimeoutMillis(2000).socketTimeoutMillis(2000);
		String username = plugin.getBungeeSettings().getRedisUsername();
		String password = plugin.getBungeeSettings().getRedisPassword();
		if (username != null && !username.isEmpty()) config.user(username);
		if (password != null && !password.isEmpty()) config.password(password);
		try (Jedis jedis = new Jedis(new HostAndPort(plugin.getBungeeSettings().getRedisHost(),
				plugin.getBungeeSettings().getRedisPort()), config.build())) {
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

	@Override
	public void close() {
		if (redisHandler != null) {
			redisHandler.close();
			redisHandler = null;
		}
		subscriptionReady = null;
	}
}
