package com.bencodez.votingplugin.backendproxy;

import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeType;
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
import com.bencodez.votingplugin.backendproxy.presence.BackendPresenceManager;
import com.bencodez.votingplugin.backendproxy.transport.BackendProxyTransportManager;
import com.bencodez.votingplugin.backendproxy.voteparty.BackendVotePartySync;
import com.bencodez.votingplugin.proxy.BungeeMethod;

import lombok.Getter;

/**
 * Coordinates backend/proxy communication components.
 */
public class BackendProxyHandler implements Listener {

	private final VotingPluginMain plugin;
	private final ProcessedVoteCache processedVoteCache = new ProcessedVoteCache();
	private final BackendProxyTransportManager transportManager;
	private final BackendGlobalDataSync globalDataSync;

	private BackendPresenceManager presenceManager;
	private BackendVotePartySync votePartySync;
	private BackendProxyMessageRouter messageRouter;

	@Getter
	private BungeeMethod method;
	@Getter
	private GlobalMessageHandler globalMessageHandler;

	public BackendProxyHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		transportManager = new BackendProxyTransportManager(plugin);
		globalDataSync = new BackendGlobalDataSync(plugin, this::sendEnvelope);
	}

	/**
	 * Loads the configured backend/proxy communication components.
	 */
	public void load() {
		plugin.debug("Loading backend proxy handler");
		method = BungeeMethod.getByName(plugin.getBungeeSettings().getBungeeMethod());
		plugin.getLogger().info("Using BungeeMethod: " + method.toString());

		globalDataSync.load();
		globalMessageHandler = new GlobalMessageHandler() {
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
		transportManager.start(method, globalMessageHandler);

		if (plugin.getOptions().getServer().equalsIgnoreCase("pleaseset")) {
			plugin.getLogger().warning("Server name for bungee voting is not set, please set it");
		}
		presenceManager.start();
	}

	/**
	 * Closes backend/proxy components and persists cached proxy state.
	 */
	public void close() {
		if (presenceManager != null) {
			presenceManager.stop();
		}
		transportManager.close();
		if (votePartySync != null) {
			votePartySync.persist();
		}
		globalDataSync.close();
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
		if (presenceManager != null) {
			presenceManager.reload();
		}
	}

	public void disablePresenceReporting() {
		if (presenceManager != null) {
			presenceManager.stop();
		}
	}

	public void loadGlobalMysql() {
		globalDataSync.load();
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
