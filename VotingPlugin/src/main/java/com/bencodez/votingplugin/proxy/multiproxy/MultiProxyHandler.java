// File: com/bencodez/votingplugin/proxy/multiproxy/MultiProxyHandler.java
package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

import lombok.Getter;

/**
 * Abstract handler for multi-proxy support.
 */
public abstract class MultiProxyHandler {
	private HashMap<String, ClientHandler> multiproxyClientHandles;
	private SocketHandler multiproxySocketHandler;

	@Getter
	private RedisHandler multiProxyRedis;

	/**
	 * Constructs a new multi-proxy handler.
	 */
	public MultiProxyHandler() {
	}

	/**
	 * Adds a non-voted player to the cache.
	 *
	 * @param uuid the player UUID
	 * @param playerName the player name
	 */
	public abstract void addNonVotedPlayerCache(String uuid, String playerName);

	/**
	 * Clears a vote for a player.
	 *
	 * @param uuid the player UUID
	 */
	public abstract void clearVote(String uuid);

	/**
	 * Closes the multi-proxy handler.
	 */
	public void close() {
		if (multiproxySocketHandler != null) {
			multiproxySocketHandler.closeConnection();
			multiproxySocketHandler = null;
		}
		if (multiProxyRedis != null && !getMultiProxyRedisUseExistingConnection()) {
			multiProxyRedis.close();
		}
	}

	/**
	 * Gets whether debug mode is enabled.
	 *
	 * @return true if debug mode is enabled
	 */
	public abstract boolean getDebug();

	/**
	 * Gets the encryption handler.
	 *
	 * @return the encryption handler
	 */
	public abstract EncryptionHandler getEncryptionHandler();

	/**
	 * Gets the multi-proxy method.
	 *
	 * @return the multi-proxy method
	 */
	public abstract MultiProxyMethod getMultiProxyMethod();

	/**
	 * Gets the multi-proxy password.
	 *
	 * @return the password
	 */
	public abstract String getMultiProxyPassword();

	/**
	 * Gets the multi-proxy Redis host.
	 *
	 * @return the Redis host
	 */
	public abstract String getMultiProxyRedisHost();

	/**
	 * Gets the multi-proxy Redis port.
	 *
	 * @return the Redis port
	 */
	public abstract int getMultiProxyRedisPort();

	/**
	 * Gets the multi-proxy Redis database index.
	 *
	 * @return the database index
	 */
	public abstract int getMultiProxyRedisDbIndex();

	/**
	 * Gets whether to use an existing Redis connection.
	 *
	 * @return true if using existing connection
	 */
	public abstract boolean getMultiProxyRedisUseExistingConnection();

	/**
	 * Gets the multi-proxy server name.
	 *
	 * @return the server name
	 */
	public abstract String getMultiProxyServerName();

	/**
	 * Gets the multi-proxy servers.
	 *
	 * @return the servers
	 */
	public abstract Collection<String> getMultiProxyServers();

	/**
	 * Gets the configuration for a multi-proxy server.
	 *
	 * @param s the server name
	 * @return the server configuration
	 */
	public abstract MultiProxyServerSocketConfiguration getMultiProxyServersConfiguration(String s);

	/**
	 * Gets the multi-proxy socket host.
	 *
	 * @return the socket host
	 */
	public abstract String getMultiProxySocketHostHost();

	/**
	 * Gets the multi-proxy socket host port.
	 *
	 * @return the socket host port
	 */
	public abstract int getMultiProxySocketHostPort();

	/**
	 * Gets whether multi-proxy support is enabled.
	 *
	 * @return true if multi-proxy support is enabled
	 */
	public abstract boolean getMultiProxySupportEnabled();

	/**
	 * Gets the multi-proxy username.
	 *
	 * @return the username
	 */
	public abstract String getMultiProxyUsername();

	/**
	 * Gets the plugin data folder.
	 *
	 * @return the plugin data folder
	 */
	public abstract File getPluginDataFolder();

	/**
	 * Gets whether this is the primary server.
	 *
	 * @return true if this is the primary server
	 */
	public abstract boolean getPrimaryServer();

	/**
	 * Gets the proxy servers.
	 *
	 * @return the proxy servers
	 */
	public abstract List<String> getProxyServers();

	/**
	 * Gets the Redis handler.
	 *
	 * @return the Redis handler
	 */
	public abstract RedisHandler getRedisHandler();

	/**
	 * Gets the version.
	 *
	 * @return the version
	 */
	public abstract String getVersion();

	/**
	 * Logs an info message.
	 *
	 * @param msg the message to log
	 */
	public abstract void logInfo(String msg);

	/**
	 * Runs a task asynchronously.
	 *
	 * @param runnable the task to run
	 */
	public abstract void runAsnc(Runnable runnable);

	/**
	 * Sets the encryption handler.
	 *
	 * @param encryptionHandler the encryption handler
	 */
	public abstract void setEncryptionHandler(EncryptionHandler encryptionHandler);

	/**
	 * Triggers a vote.
	 *
	 * @param player the player name
	 * @param service the service name
	 * @param realVote whether this is a real vote
	 * @param timeQueue whether to queue by time
	 * @param queueTime the queue time
	 * @param text the vote totals snapshot
	 * @param uuid the player UUID
	 */
	public abstract void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid);

	/**
	 * Loads multi-proxy support.
	 */
	public void loadMultiProxySupport() {
		if (!getMultiProxySupportEnabled()) {
			return;
		}

		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (getEncryptionHandler() == null) {
				setEncryptionHandler(
						new EncryptionHandler("VotingPlugin", new File(getPluginDataFolder(), "secretkey.key")));
			}

			if (multiproxySocketHandler != null) {
				multiproxySocketHandler.closeConnection();
				multiproxySocketHandler = null;
			}

			multiproxySocketHandler = new SocketHandler(getVersion(), getMultiProxySocketHostHost(),
					getMultiProxySocketHostPort(), getEncryptionHandler(), getDebug()) {

				@Override
				public void log(String str) {
					logInfo(str);
				}
			};

			multiproxySocketHandler.add(new SocketReceiver() {
				@Override
				public void onReceiveEnvelope(JsonEnvelope envelope) {
					handleEnvelope(envelope);
				}
			});

			multiproxyClientHandles = new HashMap<>();
			for (String s : getMultiProxyServers()) {
				MultiProxyServerSocketConfiguration d = getMultiProxyServersConfiguration(s);
				multiproxyClientHandles.put(s,
						new ClientHandler(d.getHost(), d.getPort(), getEncryptionHandler(), getDebug()));
			}

		} else {
			if (getMultiProxyRedisUseExistingConnection() && getRedisHandler() != null) {
				multiProxyRedis = getRedisHandler();
			} else {
				multiProxyRedis = new RedisHandler(getMultiProxyRedisHost(), getMultiProxyRedisPort(),
						getMultiProxyUsername(), getMultiProxyPassword(), getMultiProxyRedisDbIndex()) {
					@Override
					public void debug(String message) {
						if (getDebug()) {
							logInfo("MultiProxyRedis: " + message);
						}
					}
				};
			}

			runAsnc(() -> {
				RedisListener listener = multiProxyRedis.createEnvelopeListener(
						"VotingPluginProxy_" + getMultiProxyServerName(), (ch, env) -> handleEnvelope(env));
				multiProxyRedis.loadListener(listener);
			});
		}

		logInfo("Loaded multi-proxy support: " + getMultiProxyMethod().toString());
	}

	/**
	 * Handles player login.
	 *
	 * @param uuid the player UUID
	 * @param playerName the player name
	 */
	public void login(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled() || getPrimaryServer()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.login(playerName, uuid, getMultiProxyServerName()));
	}

	/**
	 * Sends a clear vote message.
	 *
	 * @param uuid the player UUID
	 * @param playerName the player name
	 */
	public void sendClearVote(String uuid, String playerName) {
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.clearVote(uuid, playerName, getMultiProxyServerName()));
	}

	/**
	 * Sends a status message.
	 */
	public void sendStatus() {
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		sendMultiProxyEnvelope(VotingPluginWire.status(getMultiProxyServerName()));
	}

	/**
	 * Sends a multi-proxy envelope.
	 *
	 * @param envelope the envelope to send
	 */
	public void sendMultiProxyEnvelope(JsonEnvelope envelope) {
		if (envelope == null) {
			return;
		}
		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (multiproxyClientHandles == null) {
				return;
			}
			for (ClientHandler h : multiproxyClientHandles.values()) {
				h.sendEnvelope(envelope);
			}
		} else if (getMultiProxyMethod().equals(MultiProxyMethod.REDIS)) {
			if (multiProxyRedis == null) {
				return;
			}
			for (String server : getProxyServers()) {
				multiProxyRedis.publishEnvelope("VotingPluginProxy_" + server, envelope);
			}
		}
	}

	private void handleEnvelope(JsonEnvelope envelope) {
		if (envelope == null) {
			return;
		}

		final String sub = envelope.getSubChannel() == null ? "" : envelope.getSubChannel();

		Map<String, String> f = envelope.getFields();
		if (f == null) {
			f = new HashMap<>();
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_STATUS)) {
			logInfo("Multi-proxy status message received");
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_CLEAR_VOTE)) {
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String server = f.getOrDefault(VotingPluginWire.K_SERVER, "");

			if (!uuid.isEmpty()) {
				clearVote(uuid);
				if (getPrimaryServer()) {
					sendMultiProxyEnvelope(VotingPluginWire.clearVotePrimary(uuid, player, server));
				}
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_CLEAR_VOTE_PRIMARY)) {
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			if (!uuid.isEmpty()) {
				clearVote(uuid);
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_LOGIN)) {
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			if (!player.isEmpty() && !uuid.isEmpty()) {
				addNonVotedPlayerCache(uuid, player);
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE) || sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE_ONLINE)) {
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			final String service = f.getOrDefault(VotingPluginWire.K_SERVICE, "");
			final String totals = f.getOrDefault(VotingPluginWire.K_TOTALS, "");
			final boolean realVote = Boolean.parseBoolean(f.getOrDefault(VotingPluginWire.K_REAL_VOTE, "false"));

			if (!player.isEmpty() && !uuid.isEmpty() && !service.isEmpty()) {
				triggerVote(player, service, realVote, true, 0L, VoteTotalsSnapshot.parseStorage(totals), uuid);
			}
			return;
		}

		if (getDebug()) {
			logInfo("MultiProxy ignored subchannel: " + sub + " fields=" + f);
		}
	}
}
