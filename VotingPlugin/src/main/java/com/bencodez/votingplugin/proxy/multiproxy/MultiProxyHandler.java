// File: com/bencodez/votingplugin/proxy/multiproxy/MultiProxyHandler.java
package com.bencodez.votingplugin.proxy.multiproxy;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.HashSet;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import com.bencodez.simpleapi.encryption.EncryptionHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.simpleapi.servercomm.redis.RedisHandler;
import com.bencodez.simpleapi.servercomm.redis.RedisListener;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketReceiver;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.redis.VotingPluginRedisChannels;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

import lombok.Getter;

/**
 * Abstract handler for multi-proxy support.
 */
public abstract class MultiProxyHandler {
	private HashMap<String, ClientHandler> multiproxyClientHandles;
	private SocketHandler multiproxySocketHandler;
	/** A renewable lease prevents a restarted/rolled-back peer staying ACK-capable forever. */
	static final long VOTE_CAPABILITY_LEASE_MILLIS = 5 * 60 * 1000L;
	/** Bound retry-driven capability advertisements while a durable outbox is waiting. */
	static final long VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS = 30 * 1000L;
	/**
	 * A discovery reply can arrive asynchronously (notably through Redis). Renew
	 * before the bounded discovery window closes, leaving the final announcement a
	 * real interval to be observed instead of classifying a peer as legacy in the
	 * same retry that sent its last handshake.
	 */
	static final long VOTE_CAPABILITY_DISCOVERY_RENEWAL_MIN_INTERVAL_MILLIS = 10 * 1000L;
	/**
	 * A newly configured peer gets this one bounded opportunity to answer the
	 * capability handshake before it is treated as a legacy one-way peer. The
	 * deadline is durable so a restart cannot extend the window indefinitely.
	 */
	static final long VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS = 30 * 1000L;
	private final Map<String, Long> acknowledgedVoteCapabilityPeers = new HashMap<>();
	/**
	 * Peers which have previously completed the durable-delivery handshake. Retain
	 * this separately from the renewable lease: once a known-capable peer drops
	 * offline, sending it a legacy vote would acknowledge a fire-and-forget publish
	 * as success and lose the sender's durable completion fence.
	 */
	private final Set<String> knownVoteCapabilityPeers = new HashSet<>();
	/** Durable, per-peer deadlines for the initial capability discovery window. */
	private final Map<String, Long> voteCapabilityDiscoveryDeadlines = new HashMap<>();
	/**
	 * We cannot safely classify configured peers after the durable state is corrupt
	 * or cannot be updated.  Do not create a new mixed-version outbox in that
	 * state: an old peer can never acknowledge it, while demoting a formerly
	 * capable peer would lose its sender fence.  Recovery is deliberately an
	 * operator action (repair/remove the named state file, then restart).
	 */
	private boolean voteCapabilityRecoveryBlocked;
	private final AtomicBoolean authenticationFailureLogged = new AtomicBoolean();
	private final AtomicBoolean encryptionFailureLogged = new AtomicBoolean();
	private static final int MAX_UNSIGNED_BRIDGE_ENTRIES = 1024;
	private static final long UNSIGNED_BRIDGE_WINDOW_NANOS = TimeUnit.SECONDS.toNanos(2);
	private final Map<String, UnsignedBridgeCopies> unsignedBridgeCopies = new LinkedHashMap<>();
	private TransportEnvelopeEncryption communicationEncryption;
	private long lastVoteCapabilityAdvertisementMillis = Long.MIN_VALUE;
	/** A newly persisted discovery deadline must cause an initial handshake promptly. */
	private boolean voteCapabilityDiscoveryAnnouncementRequired;
	/** Last wall-clock observation durably associated with discovery deadlines. */
	private long lastVoteCapabilityObservationMillis;

	@Getter
	private RedisHandler multiProxyRedis;

	/**
	 * Constructs a new multi-proxy handler.
	 */
	public MultiProxyHandler() {
	}

	long capabilityNowMillis() {
		return System.currentTimeMillis();
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
		knownVoteCapabilityPeers.clear();
		voteCapabilityDiscoveryDeadlines.clear();
		unsignedBridgeCopies.clear();
		voteCapabilityRecoveryBlocked = false;
		lastVoteCapabilityAdvertisementMillis = Long.MIN_VALUE;
		lastVoteCapabilityObservationMillis = 0L;
	}

	/**
	 * Gets whether debug mode is enabled.
	 *
	 * @return true if debug mode is enabled
	 */
	public abstract boolean getDebug();

	/** Whether complete multi-proxy envelopes require optional authenticated encryption. */
	public boolean getCommunicationEncryption() {
		return false;
	}

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

	/** Configured namespace shared with the ordinary VotingPlugin Redis transport. */
	public abstract String getRedisPrefix();

	/** Authenticator shared by all broker messages owned by this proxy runtime. */
	public abstract SharedTransportEnvelopeAuthenticator getSharedTransportAuthenticator();

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

	/** Called on the origin when a receiver confirms fence retirement. */
	public void onMultiProxyVoteRetirementAcknowledged(UUID voteId, String recipient) {
		// Optional for legacy implementations.
	}

	/** Called on a receiver after a targeted retirement request is authenticated. */
	public void onMultiProxyVoteRetirementRequested(UUID voteId, String origin) {
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

	public void requestMultiProxyVoteRetirement(UUID voteId, String origin, String recipient) {
		if (voteId == null || origin == null || origin.isBlank() || recipient == null || recipient.isBlank()) return;
		sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyVoteRetire(voteId, origin, recipient),
				java.util.Set.of(recipient));
	}

	public void acknowledgeMultiProxyVoteRetirement(UUID voteId, String origin) {
		if (voteId == null || origin == null || origin.isBlank()) return;
		sendMultiProxyEnvelopeAccepted(
				VotingPluginWire.multiProxyVoteRetireAck(voteId, origin, getMultiProxyServerName()));
	}

	/** Returns every configured remote proxy recipient, independent of version. */
	public synchronized Set<String> getConfiguredMultiProxyVoteRecipients() {
		return new LinkedHashSet<>(configuredMultiProxyRecipientNames().values());
	}

	private synchronized Map<String, String> configuredMultiProxyRecipientNames() {
		Collection<String> source = MultiProxyMethod.SOCKETS.equals(getMultiProxyMethod())
				? getMultiProxyServers() : getProxyServers();
		Map<String, String> recipients = new LinkedHashMap<>();
		if (source == null) return recipients;
		for (String server : source) {
			if (server != null && !server.isBlank())
				recipients.putIfAbsent(server.toLowerCase(Locale.ROOT), server);
		}
		return recipients;
	}

	/** Returns only configured peers that explicitly support durable acknowledgements. */
	public synchronized Set<String> getMultiProxyVoteRecipients() {
		long now = capabilityNowMillis();
		acknowledgedVoteCapabilityPeers.entrySet().removeIf(entry -> entry.getValue() <= now);
		Set<String> recipients = new LinkedHashSet<>();
		for (Map.Entry<String, String> configured : configuredMultiProxyRecipientNames().entrySet()) {
			if (acknowledgedVoteCapabilityPeers.containsKey(configured.getKey())) recipients.add(configured.getValue());
		}
		return recipients;
	}

	/**
	 * Returns configured peers which previously advertised durable acknowledgements
	 * but whose renewable lease has expired. A sender must retain these recipients
	 * in its durable outbox and wait for the capability handshake to renew instead
	 * of demoting them to the legacy one-way route.
	 */
	public synchronized Set<String> getMultiProxyVoteRecipientsAwaitingCapabilityRenewal() {
		long now = capabilityNowMillis();
		acknowledgedVoteCapabilityPeers.entrySet().removeIf(entry -> entry.getValue() <= now);
		Set<String> recipients = new LinkedHashSet<>();
		for (Map.Entry<String, String> configured : configuredMultiProxyRecipientNames().entrySet()) {
			if (knownVoteCapabilityPeers.contains(configured.getKey())
					&& !acknowledgedVoteCapabilityPeers.containsKey(configured.getKey())) {
				recipients.add(configured.getValue());
			}
		}
		return recipients;
	}

	/**
	 * Returns never-observed configured peers whose initial capability handshake
	 * is still within its bounded discovery window. Callers retain the vote for a
	 * retry while this set is non-empty, rather than publishing a one-way legacy
	 * copy before a newly upgraded peer can identify itself.
	 *
	 * <p>The deadline is recorded before it is exposed. If recording it fails, we
	 * block forwarding rather than allowing repeated restarts to make this window
	 * unbounded or silently demoting a peer to legacy.</p>
	 */
	public synchronized Set<String> getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery() {
		if (voteCapabilityRecoveryBlocked) return Set.of();
		long now = capabilityNowMillis();
		Map<String, String> configured = configuredMultiProxyRecipientNames();
		Map<String, Long> updated = new HashMap<>(voteCapabilityDiscoveryDeadlines);
		boolean clockRolledBack = lastVoteCapabilityObservationMillis > 0L && now < lastVoteCapabilityObservationMillis;
		// A wall clock moving backwards must never grant a second discovery window.
		// Expire the already-recorded opportunity rather than waiting for the clock
		// to catch up (which can otherwise retain a vote indefinitely after reboot).
		if (clockRolledBack) {
			updated.replaceAll((peer, deadline) -> deadline > now ? now : deadline);
		}
		boolean changed = updated.keySet().removeIf(peer -> !configured.containsKey(peer)
				|| knownVoteCapabilityPeers.contains(peer));
		changed |= clockRolledBack && !updated.equals(voteCapabilityDiscoveryDeadlines);
		boolean addedDiscoveryDeadline = false;
		for (String peer : configured.keySet()) {
			if (knownVoteCapabilityPeers.contains(peer) || updated.containsKey(peer)) continue;
			updated.put(peer, discoveryDeadline(now));
			changed = true;
			addedDiscoveryDeadline = true;
		}
		long observed = Math.max(0L, now);
		boolean activeDiscovery = false;
		boolean discoveryExpiredSinceLastObservation = false;
		for (Long deadline : updated.values()) {
			if (deadline == null) continue;
			if (deadline > now) {
				activeDiscovery = true;
			} else if (deadline > lastVoteCapabilityObservationMillis) {
				// Preserve the first observation after expiry. Without it, a later
				// clock rollback could make this already-consumed window live again.
				discoveryExpiredSinceLastObservation = true;
			}
		}
		// Keep a persisted high-water mark while discovery is live, but do so at
		// the same bounded cadence as advertisements. Once an expired deadline is
		// recorded, repeated legacy forwarding must not fsync this state per vote.
		boolean observationChanged = !updated.isEmpty() && (clockRolledBack
				|| discoveryExpiredSinceLastObservation || (activeDiscovery
						&& (lastVoteCapabilityObservationMillis == 0L
								|| now - lastVoteCapabilityObservationMillis
										>= VOTE_CAPABILITY_DISCOVERY_RENEWAL_MIN_INTERVAL_MILLIS)));
		if ((changed || observationChanged)
				&& !saveVoteCapabilityState(knownVoteCapabilityPeers, updated, observed)) return Set.of();
		if (addedDiscoveryDeadline) voteCapabilityDiscoveryAnnouncementRequired = true;
		Set<String> recipients = new LinkedHashSet<>();
		for (Map.Entry<String, String> configuredPeer : configured.entrySet()) {
			Long deadline = voteCapabilityDiscoveryDeadlines.get(configuredPeer.getKey());
			if (!knownVoteCapabilityPeers.contains(configuredPeer.getKey()) && deadline != null && deadline > now) {
				recipients.add(configuredPeer.getValue());
			}
		}
		return recipients;
	}

	/** Broadcasts this node's durable-ACK capability to configured peers. */
	public synchronized void announceMultiProxyVoteCapability() {
		lastVoteCapabilityAdvertisementMillis = capabilityNowMillis();
		voteCapabilityDiscoveryAnnouncementRequired = false;
		sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyCapabilities(getMultiProxyServerName(), 1));
	}

	/**
	 * Re-advertises durable delivery support for an outbox that is waiting for an
	 * expired peer lease. This is deliberately rate limited: the queue retries
	 * much more frequently than the handshake needs to be broadcast.
	 *
	 * @return whether an advertisement was attempted
	 */
	public synchronized boolean renewMultiProxyVoteCapabilityIfDue() {
		return renewMultiProxyVoteCapabilityIfDue(VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS);
	}

	/**
	 * Re-advertises while an initial discovery deadline is still live. The shorter
	 * cadence guarantees a settling interval before the durable deadline expires;
	 * callers must not invoke this after the discovery query says the window ended.
	 */
	public synchronized boolean renewMultiProxyVoteCapabilityDiscoveryIfDue() {
		return renewMultiProxyVoteCapabilityIfDue(VOTE_CAPABILITY_DISCOVERY_RENEWAL_MIN_INTERVAL_MILLIS);
	}

	private boolean renewMultiProxyVoteCapabilityIfDue(long minimumIntervalMillis) {
		long now = capabilityNowMillis();
		if (!voteCapabilityDiscoveryAnnouncementRequired && lastVoteCapabilityAdvertisementMillis != Long.MIN_VALUE
				&& now >= lastVoteCapabilityAdvertisementMillis
				&& now - lastVoteCapabilityAdvertisementMillis < minimumIntervalMillis) {
			return false;
		}
		lastVoteCapabilityAdvertisementMillis = now;
		voteCapabilityDiscoveryAnnouncementRequired = false;
		sendMultiProxyEnvelopeAccepted(VotingPluginWire.multiProxyCapabilities(getMultiProxyServerName(), 1));
		return true;
	}

	/**
	 * Whether durable peer classification needs an explicit on-disk recovery.
	 * Callers must not start a new multi-proxy forwarding attempt while this is
	 * true, because a legacy peer cannot complete an acknowledgement outbox.
	 */
	public synchronized boolean isMultiProxyVoteCapabilityRecoveryBlocked() {
		return voteCapabilityRecoveryBlocked;
	}

	/** Restores durable peer identities without restoring any expired lease. */
	synchronized void restoreVoteCapabilityPeers() {
		acknowledgedVoteCapabilityPeers.clear();
		knownVoteCapabilityPeers.clear();
		voteCapabilityDiscoveryDeadlines.clear();
		voteCapabilityRecoveryBlocked = false;
		lastVoteCapabilityObservationMillis = 0L;
		Path dataDirectory = capabilityStateDirectory();
		// Third-party MultiProxyHandler integrations predate durable state and may
		// intentionally provide no plugin folder. Preserve their established
		// in-memory handshake behavior; built-in proxy handlers always provide one.
		if (dataDirectory == null) return;
		try {
			MultiProxyCapabilityStore.State restored = MultiProxyCapabilityStore.load(dataDirectory);
			Set<String> configured = configuredMultiProxyRecipientNames().keySet();
			Set<String> restoredPeers = new HashSet<>();
			for (String peer : restored.peers()) if (configured.contains(peer)) restoredPeers.add(peer);
			Map<String, Long> restoredDeadlines = new HashMap<>();
			for (Map.Entry<String, Long> entry : restored.discoveryDeadlines().entrySet()) {
				if (configured.contains(entry.getKey()) && !restoredPeers.contains(entry.getKey())) {
					restoredDeadlines.put(entry.getKey(), entry.getValue());
				}
			}
			long now = capabilityNowMillis();
			boolean clockRolledBack = restored.lastObservedMillis() > 0L && now < restored.lastObservedMillis();
			if (clockRolledBack) {
				restoredDeadlines.replaceAll((peer, deadline) -> deadline > now ? now : deadline);
			}
			long observed = Math.max(0L, clockRolledBack ? now : Math.max(now, restored.lastObservedMillis()));
			boolean pruned = !restoredPeers.equals(restored.peers())
					|| !restoredDeadlines.equals(restored.discoveryDeadlines());
			boolean observationChanged = !restoredDeadlines.isEmpty() && observed != restored.lastObservedMillis();
			// Persist removal before installing the filtered identities. Otherwise a
			// later re-add of the same name could resurrect an ACK-capable identity.
			if ((pruned || observationChanged)
					&& !saveVoteCapabilityState(restoredPeers, restoredDeadlines, observed)) return;
			knownVoteCapabilityPeers.addAll(restoredPeers);
			voteCapabilityDiscoveryDeadlines.putAll(restoredDeadlines);
			lastVoteCapabilityObservationMillis = observed;
		} catch (IOException | IllegalArgumentException stateFailure) {
			blockVoteCapabilityRecovery();
		}
	}

	/**
	 * Makes a peer eligible for durable publishes only after its durable identity is
	 * recorded.  A failed write never silently converts an acknowledged-capable
	 * peer to the legacy route during this process.
	 */
	private boolean acceptVoteCapabilityPeer(String normalized) {
		if (voteCapabilityRecoveryBlocked) return false;
		boolean newPeer = !knownVoteCapabilityPeers.contains(normalized);
		if (!newPeer) return true;
		Set<String> updated = new HashSet<>(knownVoteCapabilityPeers);
		updated.add(normalized);
		Map<String, Long> updatedDeadlines = new HashMap<>(voteCapabilityDiscoveryDeadlines);
		updatedDeadlines.remove(normalized);
		boolean accepted = saveVoteCapabilityState(updated, updatedDeadlines, lastVoteCapabilityObservationMillis);
		if (accepted) voteCapabilityDiscoveryAnnouncementRequired = false;
		return accepted;
	}

	private long discoveryDeadline(long now) {
		return now > Long.MAX_VALUE - VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS ? Long.MAX_VALUE
				: now + VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS;
	}

	/** Saves and installs peer classification atomically from the sender's view. */
	private boolean saveVoteCapabilityState(Collection<String> peers, Map<String, Long> discoveryDeadlines,
			long lastObservedMillis) {
		Path dataDirectory = capabilityStateDirectory();
		try {
			if (dataDirectory != null) MultiProxyCapabilityStore.save(dataDirectory, peers, discoveryDeadlines,
					lastObservedMillis);
			knownVoteCapabilityPeers.clear();
			knownVoteCapabilityPeers.addAll(peers);
			voteCapabilityDiscoveryDeadlines.clear();
			voteCapabilityDiscoveryDeadlines.putAll(discoveryDeadlines);
			lastVoteCapabilityObservationMillis = lastObservedMillis;
			return true;
		} catch (IOException | IllegalArgumentException stateFailure) {
			blockVoteCapabilityRecovery();
			return false;
		}
	}

	private void blockVoteCapabilityRecovery() {
		voteCapabilityRecoveryBlocked = true;
		logInfo("Multi-proxy forwarding is blocked: repair or remove " + MultiProxyCapabilityStore.FILE_NAME
				+ " and restart before forwarding votes");
	}

	private Path capabilityStateDirectory() {
		File dataFolder = getPluginDataFolder();
		return dataFolder == null ? null : dataFolder.toPath();
	}

	/**
	 * Loads multi-proxy support.
	 */
	public synchronized void loadMultiProxySupport() {
		acknowledgedVoteCapabilityPeers.clear();
		knownVoteCapabilityPeers.clear();
		voteCapabilityDiscoveryDeadlines.clear();
		voteCapabilityRecoveryBlocked = false;
		lastVoteCapabilityAdvertisementMillis = Long.MIN_VALUE;
		voteCapabilityDiscoveryAnnouncementRequired = false;
		lastVoteCapabilityObservationMillis = 0L;
		restoreVoteCapabilityPeers();
		if (!getMultiProxySupportEnabled()) {
			return;
		}
		File dataFolder = getPluginDataFolder();
		if (dataFolder != null) try {
			communicationEncryption = TransportEnvelopeEncryption.load(dataFolder.toPath().resolve("secretkey.key"),
					TransportEnvelopeEncryption.Domain.MULTI_PROXY, getCommunicationEncryption());
		} catch (IOException failure) {
			throw new IllegalStateException("Multi-proxy communication encryption initialization failed", failure);
		} else if (getCommunicationEncryption()) {
			throw new IllegalStateException("Multi-proxy communication encryption requires a plugin data folder");
		}
		encryptionFailureLogged.set(false);

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
					acceptEncryptedEnvelope(envelope);
				}
			});

			multiproxyClientHandles = new HashMap<>();
			for (String s : getMultiProxyServers()) {
				MultiProxyServerSocketConfiguration d = getMultiProxyServersConfiguration(s);
				multiproxyClientHandles.put(s,
						new ClientHandler(d.getHost(), d.getPort(), getEncryptionHandler(), getDebug()));
			}

		} else {
			if (getSharedTransportAuthenticator() == null)
				throw new IllegalStateException("Multi-proxy Redis authentication is unavailable");
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
				loadMultiProxyRedisListener(
						VotingPluginRedisChannels.multiProxy(getRedisPrefix(), getMultiProxyServerName()));
				if (useLegacyMultiProxyRedisChannel()) {
					loadMultiProxyRedisListener(VotingPluginRedisChannels.multiProxy("", getMultiProxyServerName()));
				}
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
		Map<String, String> requested = new LinkedHashMap<>();
		Map<String, String> configuredNames = configuredMultiProxyRecipientNames();
		for (String recipient : recipients) {
			if (recipient != null && !recipient.isBlank()) {
				String normalized = recipient.toLowerCase(Locale.ROOT);
				requested.putIfAbsent(normalized, configuredNames.getOrDefault(normalized, recipient));
			}
		}
		if (requested.isEmpty()) return false;
		if (getMultiProxyMethod().equals(MultiProxyMethod.SOCKETS)) {
			if (multiproxyClientHandles == null || multiproxyClientHandles.isEmpty()) return false;
			boolean accepted = true;
			int destinations = 0;
			for (Map.Entry<String, ClientHandler> entry : multiproxyClientHandles.entrySet()) {
				if (entry.getKey() == null || !requested.containsKey(entry.getKey().toLowerCase(Locale.ROOT))) continue;
				ClientHandler h = entry.getValue();
				if (h == null) {
					accepted = false;
					continue;
				}
				destinations++;
				try {
					h.sendEnvelope(encryptEnvelope(envelope));
				} catch (RuntimeException failure) {
					accepted = false;
				}
			}
			return accepted && destinations == requested.size();
		} else if (getMultiProxyMethod().equals(MultiProxyMethod.REDIS)) {
			if (multiProxyRedis == null) return false;
			SharedTransportEnvelopeAuthenticator authenticator = getSharedTransportAuthenticator();
			if (authenticator == null) return false;
			boolean accepted = true;
			int destinations = 0;
			for (String server : requested.values()) {
				destinations++;
				try {
					JsonEnvelope encrypted = encryptEnvelope(envelope);
					String channel = VotingPluginRedisChannels.multiProxy(getRedisPrefix(), server);
					JsonEnvelope signed = authenticator.sign(encrypted, Domain.REDIS_MULTI_PROXY,
							getMultiProxyServerName(), channel);
					multiProxyRedis.publishEnvelope(channel, signed);
					if (useLegacyMultiProxyRedisChannel()) {
						// Publish the identical envelope. An upgraded peer subscribed to both
						// channels rejects the second copy through the replay fence.
						multiProxyRedis.publishEnvelope(VotingPluginRedisChannels.multiProxy("", server), signed);
					}
				} catch (RuntimeException failure) {
					accepted = false;
				}
			}
			return accepted && destinations == requested.size();
		}
		return false;
	}

	private boolean useLegacyMultiProxyRedisChannel() {
		SharedTransportEnvelopeAuthenticator authenticator = getSharedTransportAuthenticator();
		return authenticator != null && authenticator.mode() == SharedTransportEnvelopeAuthenticator.Mode.COMPATIBILITY
				&& getRedisPrefix() != null && !getRedisPrefix().isEmpty();
	}

	private void loadMultiProxyRedisListener(String channel) {
		RedisListener listener = multiProxyRedis.createEnvelopeListener(channel,
				(ch, env) -> acceptRedisEnvelope(env, ch));
		multiProxyRedis.loadListener(listener);
	}

	void acceptRedisEnvelope(JsonEnvelope envelope) {
		acceptRedisEnvelope(envelope, null);
	}

	void acceptRedisEnvelope(JsonEnvelope envelope, String channel) {
		SharedTransportEnvelopeAuthenticator authenticator = getSharedTransportAuthenticator();
		if (authenticator == null) return;
		SharedTransportEnvelopeAuthenticator.Verification verification = authenticator.verify(envelope,
				Domain.REDIS_MULTI_PROXY, channel);
		if (!verification.accepted()) {
			if (authenticationFailureLogged.compareAndSet(false, true)) {
				logInfo("Multi-proxy Redis message rejected by envelope authentication (" + verification.rejection()
						+ ")");
			}
			return;
		}
		JsonEnvelope decrypted = decryptEnvelope(verification.envelope());
		if (decrypted == null) return;
		if (verification.unsignedCompatibility() && suppressUnsignedBridgeCopy(decrypted, channel)) return;
		handleEnvelope(decrypted);
	}

	private void acceptEncryptedEnvelope(JsonEnvelope envelope) {
		JsonEnvelope decrypted = decryptEnvelope(envelope);
		if (decrypted != null) handleEnvelope(decrypted);
	}

	private JsonEnvelope decryptEnvelope(JsonEnvelope envelope) {
		if (communicationEncryption == null) return envelope;
		TransportEnvelopeEncryption.Decryption decrypted = communicationEncryption.decrypt(envelope);
		if (!decrypted.accepted()) {
			if (encryptionFailureLogged.compareAndSet(false, true)) logInfo(
					"Multi-proxy message rejected by encryption policy (" + decrypted.reason() + ")");
			return null;
		}
		return decrypted.envelope();
	}

	private synchronized boolean suppressUnsignedBridgeCopy(JsonEnvelope envelope, String channel) {
		// Vote IDs own their durable replay and acknowledgement fences. Only legacy
		// traffic without that identity needs the short bridge window.
		if (!useLegacyMultiProxyRedisChannel() || channel == null
				|| envelope.getFields().containsKey(VotingPluginWire.K_VOTE_ID)) return false;
		String prefixed = VotingPluginRedisChannels.multiProxy(getRedisPrefix(), getMultiProxyServerName());
		String legacy = VotingPluginRedisChannels.multiProxy("", getMultiProxyServerName());
		boolean onPrefixed = channel.equals(prefixed);
		if (!onPrefixed && !channel.equals(legacy)) return false;
		String fingerprint = unsignedBridgeFingerprint(envelope);
		long now = unsignedBridgeNowNanos();
		UnsignedBridgeCopies copies = unsignedBridgeCopies.get(fingerprint);
		if (copies == null || copies.expiresAtNanos <= now) {
			if (unsignedBridgeCopies.size() >= MAX_UNSIGNED_BRIDGE_ENTRIES)
				unsignedBridgeCopies.remove(unsignedBridgeCopies.keySet().iterator().next());
			copies = new UnsignedBridgeCopies(now + UNSIGNED_BRIDGE_WINDOW_NANOS);
			unsignedBridgeCopies.put(fingerprint, copies);
		}
		// Count copies per channel so two identical legitimate publications on the
		// same channel still run twice, even when both bridge copies arrive later.
		if (onPrefixed) return ++copies.prefixed <= copies.legacy;
		return ++copies.legacy <= copies.prefixed;
	}

	long unsignedBridgeNowNanos() {
		return System.nanoTime();
	}

	private static String unsignedBridgeFingerprint(JsonEnvelope envelope) {
		try {
			byte[] bytes = JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8);
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
		} catch (NoSuchAlgorithmException impossible) {
			throw new IllegalStateException("SHA-256 is unavailable", impossible);
		}
	}

	private static final class UnsignedBridgeCopies {
		private final long expiresAtNanos;
		private int prefixed;
		private int legacy;

		private UnsignedBridgeCopies(long expiresAtNanos) {
			this.expiresAtNanos = expiresAtNanos;
		}
	}

	private JsonEnvelope encryptEnvelope(JsonEnvelope envelope) {
		return communicationEncryption == null ? envelope : communicationEncryption.encrypt(envelope);
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

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_MULTI_PROXY_VOTE_RETIRE)) {
			String origin = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_ORIGIN, "");
			String recipient = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_RECIPIENT, "");
			if (!recipient.equalsIgnoreCase(getMultiProxyServerName()) || origin.isBlank()
					|| !configuredMultiProxyRecipientNames().containsKey(origin.toLowerCase(Locale.ROOT))) return;
			try {
				onMultiProxyVoteRetirementRequested(
						UUID.fromString(f.getOrDefault(VotingPluginWire.K_VOTE_ID, "")), origin);
			} catch (IllegalArgumentException ignored) { }
			return;
		}

		if (sub.equalsIgnoreCase(VotingPluginWire.SUB_MULTI_PROXY_VOTE_RETIRE_ACK)) {
			String origin = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_ORIGIN, "");
			String recipient = f.getOrDefault(VotingPluginWire.K_MULTI_PROXY_RECIPIENT, "");
			if (!origin.equalsIgnoreCase(getMultiProxyServerName())) return;
			try {
				onMultiProxyVoteRetirementAcknowledged(
						UUID.fromString(f.getOrDefault(VotingPluginWire.K_VOTE_ID, "")), recipient);
			} catch (IllegalArgumentException ignored) { }
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
					if (version >= 1 && configuredMultiProxyRecipientNames().containsKey(normalized)) {
						if (acceptVoteCapabilityPeer(normalized)) {
							acknowledgedVoteCapabilityPeers.put(normalized,
									capabilityNowMillis() + VOTE_CAPABILITY_LEASE_MILLIS);
						}
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
			if (!origin.isBlank() && wireVote.voteId == null) {
				// Reliable envelopes must never enter receiver processing without the
				// stable identity used by its durable completion fence.
				return;
			}

			if (!player.isEmpty() && !uuid.isEmpty() && !service.isEmpty()) {
				if (origin.isBlank()) {
					triggerVote(player, service, realVote, true, 0L, VoteTotalsSnapshot.parseStorage(totals), uuid);
				} else {
					triggerVote(player, service, realVote, true, 0L, VoteTotalsSnapshot.parseStorage(totals), uuid,
							wireVote.voteId, origin);
				}
			}
			return;
		}

		if (getDebug()) {
			logInfo("MultiProxy ignored subchannel: " + sub + " fields=" + f);
		}
	}
}
