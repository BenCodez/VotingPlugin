package com.bencodez.votingplugin.backendproxy.transport;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class RedisBackendProxyTransport implements BackendProxyTransport {

	private final VotingPluginMain plugin;
	@Getter
	private RedisHandler redisHandler;
	private Thread listenerThread;

	public RedisBackendProxyTransport(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public void start(GlobalMessageHandler messageHandler) {
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

		listenerThread = new Thread(() -> {
			if (plugin.isEnabled()) {
				RedisListener listener = redisHandler.createEnvelopeListener(
						plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin_" + plugin.getBungeeSettings().getServer(),
						(ch, envelope) -> messageHandler.onMessage(envelope));
				redisHandler.loadListener(listener);
			}
		}, "VotingPlugin-Redis-Backend");
		listenerThread.start();
	}

	@Override
	public void send(JsonEnvelope envelope) {
		if (redisHandler != null) {
			redisHandler.publishEnvelope(plugin.getBungeeSettings().getRedisPrefix() + "VotingPlugin", envelope);
		}
	}

	@Override
	public void close() {
		// Preserve current Redis lifecycle behavior; the listener is owned by RedisHandler.
	}
}
