package com.bencodez.votingplugin.backendproxy.transport;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.global.GlobalMessageHandler;
import com.bencodez.simpleapi.servercomm.mqtt.MqttHandler;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backendproxy.cache.ProcessedVoteCache;
import com.bencodez.votingplugin.proxy.BungeeMethod;

/**
 * Selects and owns the active backend-to-proxy transport.
 */
public class BackendProxyTransportManager {

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache;
	private BackendProxyTransport transport;

	public BackendProxyTransportManager(VotingPluginMain plugin) {
		this(plugin, new ProcessedVoteCache());
	}

	public BackendProxyTransportManager(VotingPluginMain plugin, ProcessedVoteCache processedVoteCache) {
		this.plugin = plugin;
		this.processedVoteCache = processedVoteCache;
	}

	public void start(BungeeMethod method, GlobalMessageHandler messageHandler) {
		close();
		switch (method) {
		case MYSQL:
			transport = new MysqlBackendProxyTransport(plugin);
			break;
		case PLUGINMESSAGING:
			transport = new PluginMessagingBackendProxyTransport(plugin);
			break;
		case SOCKETS:
			transport = new SocketBackendProxyTransport(plugin);
			break;
		case HTTP:
			transport = new HttpBackendProxyTransport(plugin);
			break;
		case REDIS:
			transport = new RedisBackendProxyTransport(plugin, processedVoteCache);
			break;
		case MQTT:
			transport = new MqttBackendProxyTransport(plugin);
			break;
		default:
			throw new IllegalArgumentException("Unsupported backend proxy method: " + method);
		}
		transport.start(messageHandler);
	}

	public void send(JsonEnvelope envelope) {
		if (transport != null) {
			transport.send(envelope);
		}
	}

	public void close() {
		if (transport != null) {
			transport.close();
			transport = null;
		}
	}

	public void validate() {
		if (transport == null) throw new IllegalStateException("Backend proxy transport was not initialized");
		transport.validate();
	}

	public void prepareForReplacement() {
		if (transport != null) {
			transport.prepareForReplacement();
			transport = null;
		}
	}

	public void closeRedisForHandoff() {
		if (!(transport instanceof RedisBackendProxyTransport)) {
			throw new IllegalStateException("Redis backend proxy transport is unavailable");
		}
		((RedisBackendProxyTransport) transport).closeForHandoff();
		transport = null;
	}

	public void activateRedisAfterHandoff() {
		if (!(transport instanceof RedisBackendProxyTransport)) {
			throw new IllegalStateException("Redis replacement transport is unavailable");
		}
		((RedisBackendProxyTransport) transport).activateAfterHandoff();
	}

	public ClientHandler getClientHandler() {
		return transport instanceof SocketBackendProxyTransport
				? ((SocketBackendProxyTransport) transport).getClientHandler() : null;
	}

	public SocketHandler getSocketHandler() {
		return transport instanceof SocketBackendProxyTransport
				? ((SocketBackendProxyTransport) transport).getSocketHandler() : null;
	}

	public RedisHandler getRedisHandler() {
		return transport instanceof RedisBackendProxyTransport
				? ((RedisBackendProxyTransport) transport).getRedisHandler() : null;
	}

	public MySqlMessenger getBackendMysqlMessenger() {
		return transport instanceof MysqlBackendProxyTransport
				? ((MysqlBackendProxyTransport) transport).getMessenger() : null;
	}

	public MqttHandler getMqttHandler() {
		return transport instanceof MqttBackendProxyTransport
				? ((MqttBackendProxyTransport) transport).getMqttHandler() : null;
	}
}
