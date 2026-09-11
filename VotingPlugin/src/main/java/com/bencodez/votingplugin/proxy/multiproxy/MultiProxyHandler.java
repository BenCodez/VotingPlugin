// File: com/bencodez/votingplugin/proxy/multiproxy/MultiProxyHandler.java
package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

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
	/** Peers that have explicitly advertised the additive durable-ACK protocol. */
	private final Set<String> acknowledgedVoteCapabilityPeers = new LinkedHashSet<>();

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
	public synchronized void close() {
		if (multiproxySocketHandler != null) {
			multiproxySocketHandler.closeConnection();
			multiproxySocketHandler = null;
		}
		if (multiProxyRedis != null && !getMultiProxyRedisUseExistingConnection()) {
			multiProxyRedis.close();
		}
		multiProxyRedis = null;
		stopSocketClients(multiproxyClientHandles);
		multiproxyClientHandles = null;
		acknowledgedVoteCapabilityPeers.clear();
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
	 * Gets whether the multi-proxy Redis connection uses SSL/TLS.
	 *
	 * @return true if SSL/TLS is enabled
	 */
	public abstract boolean getMultiProxyRedisSsl();

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
	 * Triggers a vote using the stable ID carried by a multi-proxy envelope.
	 * Existing implementations retain the legacy callback contract; implementations
	 * that support durable retries should override this overload.
	 */
	public void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid, UUID voteId) {
		triggerVote(player, service, realVote, timeQueue, queueTime, text, uuid);
	}

	/**
	 * Additive reliable-delivery overload. Existing integrations retain the
	 * stable-ID behavior without needing to understand acknowledgements.
	 */
	public void triggerVote(String player, String service, boolean realVote, boolean timeQueue, long queueTime,
			VoteTotalsSnapshot text, String uuid, UUID voteId, String origin) {
		triggerVote(player, service, realVote, timeQueue, queueTime, text, uuid, voteId);
	}

	/** Called when a receiver acknowledges this proxy's stable vote ID. */
	public void onMultiProxyVoteAcknowledged(UUID voteId, String recipient) {
		// Optional for legacy implementations.
	}

	/**
	 * Publishes an acknowledgement after receiver completion is durable. The
	 * broadcast route keeps sockets and Redis compatible; recipients filter it by
	 * the origin field, and duplicate votes cause the receiver to acknowledge again.
	 */
	public void acknowledgeMultiProxyVote(UUID voteId, String origin) {
		if (voteId == null || origin == null || origin.isBlank()) return;
		sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyVoteAck(voteId, origin, getMultiProxyServerName()));
	}

	/** Returns every configured remote proxy recipient, independent of version. */
	public synchronized Set<String> getConfiguredMultiProxyVoteRecipients() {
		Collection<String> source = getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)
				? getMultiProxyServers() : getProxyServers();
		Set<String> recipients = new LinkedHashSet<>();
		if (source == null) return recipients;
		for (String server : source) {
			if (server != null && !server.isBlank()) recipients.add(server.toLowerCase(Locale.ROOT));
		}
		return recipients;
	}

	/** Returns only configured peers that explicitly support durable acknowledgements. */
	public synchronized Set<String> getMultiProxyVoteRecipients() {
		Set<String> recipients = getConfiguredMultiProxyVoteRecipients();
		recipients.retainAll(acknowledgedVoteCapabilityPeers);
		return recipients;
	}

	/** Broadcasts this node's durable-ACK capability to configured peers. */
	public synchronized void announceMultiProxyVoteCapability() {
		sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyCapabilities(getMultiProxyServerName(), 1));
	}

	/**
	 * Loads multi-proxy support.
	 */
	public synchronized void loadMultiProxySupport() {
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
						getMultiProxyUsername(), getMultiProxyPassword(), getMultiProxyRedisDbIndex(),
						getMultiProxyRedisSsl()) {
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
		announceMultiProxyVoteCapability();
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
	public synchronized void sendMultiProxyEnvelope(JsonEnvelope envelope) {
		sendMultiProxyEnvelopeAccepted(envelope);
	}

	/**
	 * Sends an envelope and reports whether the configured transport accepted it.
	 *
	 * <p>The legacy send method is intentionally retained for callers that do not
	 * need delivery fencing. A vote producer must use this result: a missing
	 * client, an unavailable Redis connection, an empty destination list, or a
	 * transport exception must leave the vote retryable instead of claiming that
	 * forwarding completed. Socket clients are fire-and-forget in SimpleAPI, so
	 * acceptance means that the configured client accepted the send invocation;
	 * the stable wire vote ID remains the receiver-side duplicate fence.</p>
	 *
	 * @param envelope the envelope to send
	 * @return true only when every configured destination accepted the envelope
	 */
	public synchronized boolean sendMultiProxyEnvelopeAccepted(JsonEnvelope envelope) {
		return sendMultiProxyEnvelopeAccepted(envelope, getConfiguredMultiProxyVoteRecipients());
	}

	/**
	 * Sends an envelope to a selected subset of configured peers. Reliable senders
	 * use this to retry only ACK-capable peers, avoiding duplicate legacy delivery.
	 */
	public synchronized boolean sendMultiProxyEnvelopeAccepted(JsonEnvelope envelope, Collection<String> recipients) {
		if (envelope == null) return false;
		if (recipients == null || recipients.isEmpty()) return false;
		Set<String> requested = new LinkedHashSet<>();
		for (String recipient : recipients) {
			if (recipient != null && !recipient.isBlank()) requested.add(recipient.toLowerCase(Locale.ROOT));
		}
		if (requested.isEmpty()) return false;
		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (multiproxyClientHandles == null || multiproxyClientHandles.isEmpty()) return false;
			boolean accepted = true;
			int destinations = 0;
			for (Map.Entry<String, ClientHandler> entry : multiproxyClientHandles.entrySet()) {
				if (entry.getKey() == null || !requested.contains(entry.getKey().toLowerCase(Locale.ROOT))) continue;
				ClientHandler h = entry.getValue();
				if (h == null) {
					accepted = false;
					continue;
				}
				destinations++;
				try {
					h.sendEnvelope(envelope);
				} catch (RuntimeException failure) {
					accepted = false;
				}
			}
			return accepted && destinations == requested.size();
		} else if (getMultiProxyMethod().equals(MultiProxyMethod.REDIS)) {
			if (multiProxyRedis == null) return false;
			boolean accepted = true;
			int destinations = 0;
			for (String server : requested) {
				destinations++;
				try {
					multiProxyRedis.publishEnvelope("VotingPluginProxy_" + server, envelope);
				} catch (RuntimeException failure) {
					accepted = false;
				}
			}
			return accepted && destinations == requested.size();
		}
		return false;
	}

	static void stopSocketClients(Map<String, ClientHandler> clients) {
		if (clients == null) return;
		for (ClientHandler client : clients.values()) {
			if (client == null) continue;
			try {
				client.stopConnection();
			} catch (RuntimeException ignored) {
				// Continue closing the remaining clients after an individual failure.
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

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_MULTI_PROXY_VOTE_ACK)) {
			String origin = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_ORIGIN, "");
			String recipient = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_RECIPIENT, "");
			if (!origin.equalsIgnoreCase(getMultiProxyServerName())) return;
			try {
				onMultiProxyVoteAcknowledged(UUID.fromString(f.getOrDefault(VotingPluginWire.K_VOTE_ID, "")), recipient);
			} catch (IllegalArgumentException ignored) {
				// Ignore malformed acknowledgements; they must never clear a sender fence.
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_MULTI_PROXY_CAPABILITIES)) {
			String recipient = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_RECIPIENT, "");
			boolean reply = Boolean.parseBoolean(f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_CAPABILITY_REPLY, "false"));
			boolean replyRequired = false;
			try {
				int version = Integer.parseInt(f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_ACK_VERSION, "0"));
				String normalized = recipient.toLowerCase(Locale.ROOT);
				synchronized (this) {
					if (version >= 1 && getConfiguredMultiProxyVoteRecipients().contains(normalized)) {
						acknowledgedVoteCapabilityPeers.add(normalized);
						replyRequired = !reply;
					}
				}
			} catch (IllegalArgumentException ignored) {
				// A malformed capability must leave the peer on the legacy route.
			}
			if (replyRequired) {
				sendMultiProxyEnvelopeAccepted(
						VotingPluginWire.multiProxyCapabilities(getMultiProxyServerName(), 1, true));
			}
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE) || sub.equalsIgnoreCase(VotingPluginWire.SUB_VOTE_ONLINE)) {
			final VotingPluginWire.Vote wireVote = VotingPluginWire.readVote(envelope);
			final String player = f.getOrDefault(VotingPluginWire.K_PLAYER, "");
			final String uuid = f.getOrDefault(VotingPluginWire.K_UUID, "");
			final String service = f.getOrDefault(VotingPluginWire.K_SERVICE, "");
			final String totals = f.getOrDefault(VotingPluginWire.K_TOTALS, "");
			final boolean realVote = Boolean.parseBoolean(f.getOrDefault(VotingPluginWire.K_REAL_VOTE, "false"));
			final String origin = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_ORIGIN, "");

			if (!player.isEmpty() && !uuid.isEmpty() && !service.isEmpty()) {
				triggerVote(player, service, realVote, true, 0L, VoteTotalsSnapshot.parseStorage(totals), uuid,
						wireVote.voteId, origin);
			}
			return;
		}

		if (getDebug()) {
			logInfo("MultiProxy ignored subchannel: " + sub + " fields=" + f);
		}
	}
}
