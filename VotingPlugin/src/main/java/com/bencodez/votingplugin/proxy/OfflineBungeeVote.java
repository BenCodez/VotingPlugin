package com.bencodez.votingplugin.proxy;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Locale;

import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents an offline bungee vote.
 */
public class OfflineBungeeVote {

	@Getter
	private String playerName;
	@Getter
	private boolean realVote;
	@Getter
	private String service;
	@Getter
	@Setter
	private String text;
	@Getter
	private long time;
	@Getter
	private String uuid;
	@Getter
	private UUID voteId;
	@Getter
	@Setter
	private boolean broadcastForwarded;
	@Getter
	@Setter
	private boolean proxyBroadcastHandled;
	@Getter
	private Set<String> broadcastTargets;
	@Getter
	private Set<String> broadcastForwardedServers;
	@Getter
	@Setter
	private boolean rewardDelivered;
	@Getter
	@Setter
	private boolean deliveryStateDirty;
	/** Stable HTTP delivery IDs that must be reused for each target server. */
	private final Map<String, String> httpDeliveryIds;
	/** Stable HTTP standalone-broadcast IDs, separate from reward delivery IDs. */
	private final Map<String, String> httpBroadcastDeliveryIds;

	/**
	 * Constructor with UUID voteId.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this(voteId, playerName, uuid, service, time, realVote, text, false);
	}

	/**
	 * Constructor with UUID voteId and proxy broadcast delivery state.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded whether the proxy already forwarded the broadcast
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded) {
		this(voteId, playerName, uuid, service, time, realVote, text, broadcastForwarded, false,
				Collections.emptySet(), Collections.emptySet(), false);
	}

	/**
	 * Constructor with full proxy broadcast delivery state.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded legacy aggregate forwarded state
	 * @param proxyBroadcastHandled whether standalone proxy routing was selected
	 * @param broadcastTargets original standalone broadcast targets
	 * @param broadcastForwardedServers targets that accepted standalone delivery
	 * @param rewardDelivered whether the cached reward vote was already delivered
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered) {
		this(voteId, playerName, uuid, service, time, realVote, text, broadcastForwarded, proxyBroadcastHandled,
				broadcastTargets, broadcastForwardedServers, rewardDelivered, Collections.emptyMap());
	}

	/**
	 * Constructor with full proxy state and stable HTTP delivery IDs.
	 * @param httpDeliveryIds stable HTTP delivery ID by target server
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered, Map<String, String> httpDeliveryIds) {
		this(voteId, playerName, uuid, service, time, realVote, text, broadcastForwarded, proxyBroadcastHandled,
				broadcastTargets, broadcastForwardedServers, rewardDelivered, httpDeliveryIds, Collections.emptyMap());
	}

	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered, Map<String, String> httpDeliveryIds,
			Map<String, String> httpBroadcastDeliveryIds) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
		this.voteId = voteId;
		this.broadcastForwarded = broadcastForwarded;
		this.proxyBroadcastHandled = proxyBroadcastHandled;
		setBroadcastTargets(broadcastTargets);
		setBroadcastForwardedServers(broadcastForwardedServers);
		this.rewardDelivered = rewardDelivered;
		this.httpDeliveryIds = new LinkedHashMap<>();
		if (httpDeliveryIds != null) {
			httpDeliveryIds.forEach(this::setHttpDeliveryId);
		}
		this.httpBroadcastDeliveryIds = new LinkedHashMap<>();
		if (httpBroadcastDeliveryIds != null) httpBroadcastDeliveryIds.forEach(this::setHttpBroadcastDeliveryId);
	}
	
	/**
	 * Constructor with String voteId.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this(voteId, playerName, uuid, service, time, realVote, text, false);
	}

	/**
	 * Constructor with String voteId and proxy broadcast delivery state.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded whether the proxy already forwarded the broadcast
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded);
	}

	/**
	 * Constructor with String voteId and full proxy broadcast delivery state.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 * @param broadcastForwarded legacy aggregate forwarded state
	 * @param proxyBroadcastHandled whether standalone proxy routing was selected
	 * @param broadcastTargets original standalone broadcast targets
	 * @param broadcastForwardedServers targets that accepted standalone delivery
	 * @param rewardDelivered whether the cached reward vote was already delivered
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded,
				proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, rewardDelivered);
	}

	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered, Map<String, String> httpDeliveryIds) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded,
				proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, rewardDelivered, httpDeliveryIds);
	}

	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text, boolean broadcastForwarded, boolean proxyBroadcastHandled, Set<String> broadcastTargets,
			Set<String> broadcastForwardedServers, boolean rewardDelivered, Map<String, String> httpDeliveryIds,
			Map<String, String> httpBroadcastDeliveryIds) {
		this(parseVoteId(voteId), playerName, uuid, service, time, realVote, text, broadcastForwarded,
				proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, rewardDelivered, httpDeliveryIds,
				httpBroadcastDeliveryIds);
	}

	private static UUID parseVoteId(String voteId) {
		return voteId == null || voteId.isEmpty() ? null : UUID.fromString(voteId);
	}

	/**
	 * Replaces the original standalone broadcast targets.
	 * @param targets target servers
	 */
	public void setBroadcastTargets(Set<String> targets) {
		broadcastTargets = targets == null ? new LinkedHashSet<>() : new LinkedHashSet<>(targets);
	}

	/**
	 * Replaces the targets that accepted standalone broadcast delivery.
	 * @param forwardedServers delivered target servers
	 */
	public void setBroadcastForwardedServers(Set<String> forwardedServers) {
		broadcastForwardedServers = forwardedServers == null ? new LinkedHashSet<>()
				: new LinkedHashSet<>(forwardedServers);
	}

	/**
	 * Checks whether every original standalone target accepted delivery.
	 * @return true when standalone routing is complete
	 */
	public boolean isProxyBroadcastComplete() {
		return proxyBroadcastHandled && broadcastForwardedServers.containsAll(broadcastTargets);
	}

	/**
	 * Checks whether this cached vote still needs to broadcast on a server.
	 * @param server backend server receiving the cached vote
	 * @return true when this server is an original undelivered target
	 */
	public boolean needsBroadcastOn(String server) {
		if (!proxyBroadcastHandled) {
			return !broadcastForwarded;
		}
		return server != null && broadcastTargets.contains(server) && !broadcastForwardedServers.contains(server);
	}

	/**
	 * Returns the stable HTTP delivery ID for a target, if one is pending.
	 * @param server target server
	 * @return stable delivery ID or null
	 */
	public String getHttpDeliveryId(String server) {
		return server == null ? null : httpDeliveryIds.get(server.toLowerCase(Locale.ROOT));
	}

	/** Returns a copy of stable reward delivery IDs by normalized target key. */
	public Map<String, String> getHttpDeliveryIds() {
		return new LinkedHashMap<>(httpDeliveryIds);
	}

	/**
	 * Stores or removes a stable HTTP delivery ID for a target.
	 * @param server target server
	 * @param deliveryId stable delivery ID, or null to remove
	 */
	public void setHttpDeliveryId(String server, String deliveryId) {
		if (server == null || server.isBlank()) return;
		String key = server.toLowerCase(Locale.ROOT);
		if (deliveryId == null || deliveryId.isBlank()) httpDeliveryIds.remove(key);
		else httpDeliveryIds.put(key, deliveryId);
	}

	public String getHttpBroadcastDeliveryId(String server) {
		return server == null ? null : httpBroadcastDeliveryIds.get(server.toLowerCase(Locale.ROOT));
	}

	public void setHttpBroadcastDeliveryId(String server, String deliveryId) {
		if (server == null || server.isBlank()) return;
		String key = server.toLowerCase(Locale.ROOT);
		if (deliveryId == null || deliveryId.isBlank()) httpBroadcastDeliveryIds.remove(key);
		else httpBroadcastDeliveryIds.put(key, deliveryId);
	}

	/**
	 * Returns a copy of pending standalone HTTP delivery IDs for cache handoff.
	 * @return pending standalone delivery IDs by target server
	 */
	public Map<String, String> getHttpBroadcastDeliveryIds() {
		return new LinkedHashMap<>(httpBroadcastDeliveryIds);
	}

	/** Returns whether any reward or standalone HTTP delivery is still pending. */
	public boolean hasPendingHttpDeliveryIds() {
		return !httpDeliveryIds.isEmpty() || !httpBroadcastDeliveryIds.isEmpty();
	}

	/**
	 * Encodes stable delivery IDs for JSON/SQL cache storage.
	 * @return bounded delimiter-safe encoding
	 */
	public String encodeHttpDeliveryIds() {
		return encodeDeliveryIds(httpDeliveryIds);
	}

	public String encodeHttpBroadcastDeliveryIds() {
		return encodeDeliveryIds(httpBroadcastDeliveryIds);
	}

	private static String encodeDeliveryIds(Map<String, String> values) {
		StringBuilder encoded = new StringBuilder();
		for (Map.Entry<String, String> entry : values.entrySet()) {
			if (encoded.length() > 0) encoded.append('.');
			encoded.append(Base64.getUrlEncoder().withoutPadding()
					.encodeToString(entry.getKey().getBytes(StandardCharsets.UTF_8)));
			encoded.append('~');
			encoded.append(Base64.getUrlEncoder().withoutPadding()
					.encodeToString(entry.getValue().getBytes(StandardCharsets.UTF_8)));
		}
		return encoded.toString();
	}

	/**
	 * Decodes stable delivery IDs from cache storage. Malformed entries are ignored.
	 * @param encoded encoded map
	 * @return decoded map
	 */
	public static Map<String, String> decodeHttpDeliveryIds(String encoded) {
		return decodeDeliveryIds(encoded);
	}

	public static Map<String, String> decodeHttpBroadcastDeliveryIds(String encoded) {
		return decodeDeliveryIds(encoded);
	}

	private static Map<String, String> decodeDeliveryIds(String encoded) {
		Map<String, String> decoded = new LinkedHashMap<>();
		if (encoded == null || encoded.isBlank()) return decoded;
		for (String entry : encoded.split("\\.", -1)) {
			int separator = entry.indexOf('~');
			if (separator <= 0 || separator == entry.length() - 1) continue;
			try {
				String server = new String(Base64.getUrlDecoder().decode(entry.substring(0, separator)), StandardCharsets.UTF_8);
				String deliveryId = new String(Base64.getUrlDecoder().decode(entry.substring(separator + 1)), StandardCharsets.UTF_8);
				if (!server.isBlank() && deliveryId.matches("[0-9a-fA-F-]{36}"))
					decoded.put(server.toLowerCase(Locale.ROOT), deliveryId);
			} catch (IllegalArgumentException ignored) {
				// Ignore corrupt optional delivery state and retain the vote itself.
			}
		}
		return decoded;
	}

	/**
	 * Encodes the original broadcast targets for cache storage.
	 * @return encoded target set
	 */
	public String encodeBroadcastTargets() {
		return VoteTimeQueue.encodeBroadcastServers(broadcastTargets);
	}

	/**
	 * Encodes delivered broadcast targets for cache storage.
	 * @return encoded delivered target set
	 */
	public String encodeBroadcastForwardedServers() {
		return VoteTimeQueue.encodeBroadcastServers(broadcastForwardedServers);
	}

	@Override
	public String toString() {
		return "VoteCache:" + playerName + "/" + uuid + "/" + service + "/" + time + "/" + realVote + "/" + text + "/"
				+ voteId + "/" + broadcastForwarded + "/" + proxyBroadcastHandled + "/" + broadcastTargets + "/"
				+ broadcastForwardedServers + "/" + rewardDelivered;
	}

}
