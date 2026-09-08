package com.bencodez.votingplugin.timequeue;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a vote delayed while a proxy time change is active.
 */
public class VoteTimeQueue {
	@Getter
	@Setter
	private String name;
	@Getter
	@Setter
	private String service;
	@Getter
	@Setter
	private long time;
	@Getter
	@Setter
	private UUID voteId;
	@Getter
	@Setter
	private String uuid;
	@Getter
	@Setter
	private boolean proxyBroadcastHandled;
	@Getter
	@Setter
	private String totals;
	@Getter
	@Setter
	private boolean processed;
	@Getter
	@Setter
	private boolean deliveryStateDirty;
	@Getter
	private Set<String> broadcastTargets;
	@Getter
	private Set<String> broadcastForwardedServers;
	/** Stable HTTP standalone-broadcast delivery IDs by target server. */
	private final Map<String, String> httpBroadcastDeliveryIds;

	/**
	 * Creates a legacy-compatible queued vote without an identifier.
	 *
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 */
	public VoteTimeQueue(String name, String service, long time) {
		this(null, name, service, time, false, Collections.emptySet(), Collections.emptySet(), "", false, "");
	}

	/**
	 * Creates a queued vote with its original identifier.
	 *
	 * @param voteId unique vote identifier
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 */
	public VoteTimeQueue(UUID voteId, String name, String service, long time) {
		this(voteId, name, service, time, false, Collections.emptySet(), Collections.emptySet(), "", false, "");
	}

	/**
	 * Creates a queued vote with standalone proxy broadcast delivery state.
	 *
	 * @param voteId unique vote identifier
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 * @param proxyBroadcastHandled whether standalone forwarding was handled before queueing
	 * @param broadcastForwardedServers backend servers that received the standalone broadcast
	 */
	public VoteTimeQueue(UUID voteId, String name, String service, long time, boolean proxyBroadcastHandled,
			Set<String> broadcastForwardedServers) {
		this(voteId, name, service, time, proxyBroadcastHandled, Collections.emptySet(), broadcastForwardedServers, "",
				false, "");
	}

	/**
	 * Creates a queued vote with the original proxy broadcast routing state.
	 *
	 * @param voteId unique vote identifier
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 * @param proxyBroadcastHandled whether standalone forwarding was handled before queueing
	 * @param broadcastTargets original backend broadcast targets
	 * @param broadcastForwardedServers backend servers that received the standalone broadcast
	 */
	public VoteTimeQueue(UUID voteId, String name, String service, long time, boolean proxyBroadcastHandled,
			Set<String> broadcastTargets, Set<String> broadcastForwardedServers) {
		this(voteId, name, service, time, proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, "",
				false, "");
	}

	/**
	 * Creates a queued vote with all durable replay state.
	 *
	 * @param voteId unique vote identifier
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 * @param proxyBroadcastHandled whether standalone forwarding was handled before queueing
	 * @param broadcastTargets original backend broadcast targets
	 * @param broadcastForwardedServers backend servers that received the standalone broadcast
	 * @param totals incoming multi-proxy totals snapshot
	 * @param processed whether normal replay processing completed
	 */
	public VoteTimeQueue(UUID voteId, String name, String service, long time, boolean proxyBroadcastHandled,
			Set<String> broadcastTargets, Set<String> broadcastForwardedServers, String totals, boolean processed) {
		this(voteId, name, service, time, proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, totals,
				processed, "");
	}

	public VoteTimeQueue(UUID voteId, String name, String service, long time, boolean proxyBroadcastHandled,
			Set<String> broadcastTargets, Set<String> broadcastForwardedServers, String totals, boolean processed,
			String uuid) {
		this(voteId, name, service, time, proxyBroadcastHandled, broadcastTargets, broadcastForwardedServers, totals,
				processed, uuid, Collections.emptyMap());
	}

	/** Creates a queued vote with persisted HTTP standalone-broadcast IDs. */
	public VoteTimeQueue(UUID voteId, String name, String service, long time, boolean proxyBroadcastHandled,
			Set<String> broadcastTargets, Set<String> broadcastForwardedServers, String totals, boolean processed,
			String uuid, Map<String, String> httpBroadcastDeliveryIds) {
		this.voteId = voteId;
		this.uuid = uuid == null ? "" : uuid;
		this.name = name;
		this.service = service;
		this.time = time;
		this.proxyBroadcastHandled = proxyBroadcastHandled;
		this.totals = totals == null ? "" : totals;
		this.processed = processed;
		this.broadcastTargets = new LinkedHashSet<>();
		if (broadcastTargets != null) {
			this.broadcastTargets.addAll(broadcastTargets);
		}
		this.broadcastForwardedServers = new LinkedHashSet<>();
		if (broadcastForwardedServers != null) {
			this.broadcastForwardedServers.addAll(broadcastForwardedServers);
		}
		this.httpBroadcastDeliveryIds = new LinkedHashMap<>();
		if (httpBroadcastDeliveryIds != null) {
			httpBroadcastDeliveryIds.forEach(this::setHttpBroadcastDeliveryId);
		}
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

	public Map<String, String> getHttpBroadcastDeliveryIds() {
		return new LinkedHashMap<>(httpBroadcastDeliveryIds);
	}

	/** Returns whether any standalone HTTP broadcast still has to be delivered. */
	public boolean hasPendingHttpBroadcastDeliveryIds() {
		return !httpBroadcastDeliveryIds.isEmpty();
	}

	public String encodeHttpBroadcastDeliveryIds() {
		StringBuilder encoded = new StringBuilder();
		for (Map.Entry<String, String> entry : httpBroadcastDeliveryIds.entrySet()) {
			if (encoded.length() > 0) encoded.append('.');
			encoded.append(Base64.getUrlEncoder().withoutPadding()
					.encodeToString(entry.getKey().getBytes(StandardCharsets.UTF_8)));
			encoded.append('~');
			encoded.append(Base64.getUrlEncoder().withoutPadding()
					.encodeToString(entry.getValue().getBytes(StandardCharsets.UTF_8)));
		}
		return encoded.toString();
	}

	public static Map<String, String> decodeHttpBroadcastDeliveryIds(String encoded) {
		Map<String, String> decoded = new LinkedHashMap<>();
		if (encoded == null || encoded.isBlank()) return decoded;
		for (String entry : encoded.split("\\.", -1)) {
			int separator = entry.indexOf('~');
			if (separator <= 0 || separator == entry.length() - 1) continue;
			try {
				String server = new String(Base64.getUrlDecoder().decode(entry.substring(0, separator)),
						StandardCharsets.UTF_8);
				String deliveryId = new String(Base64.getUrlDecoder().decode(entry.substring(separator + 1)),
						StandardCharsets.UTF_8);
				if (!server.isBlank() && deliveryId.matches("[0-9a-fA-F-]{36}")) {
					decoded.put(server.toLowerCase(Locale.ROOT), deliveryId);
				}
			} catch (IllegalArgumentException ignored) {
				// Ignore corrupt optional delivery state and retain the queued vote.
			}
		}
		return decoded;
	}

	/**
	 * Encodes original broadcast targets for JSON and SQL cache storage.
	 *
	 * @return encoded target set
	 */
	public String encodeBroadcastTargets() {
		return encodeBroadcastServers(broadcastTargets);
	}

	/**
	 * Encodes forwarded server names for JSON and SQL cache storage.
	 *
	 * @return encoded server set
	 */
	public String encodeBroadcastForwardedServers() {
		return encodeBroadcastServers(broadcastForwardedServers);
	}

	/**
	 * Encodes backend server names for cache storage.
	 *
	 * @param servers server names to encode
	 * @return encoded server set
	 */
	public static String encodeBroadcastServers(Set<String> servers) {
		Base64.Encoder encoder = Base64.getUrlEncoder().withoutPadding();
		StringBuilder encoded = new StringBuilder();
		if (servers == null) {
			return "";
		}
		for (String server : servers) {
			if (server == null || server.isEmpty()) {
				continue;
			}
			if (encoded.length() > 0) {
				encoded.append(',');
			}
			encoded.append(encoder.encodeToString(server.getBytes(StandardCharsets.UTF_8)));
		}
		return encoded.toString();
	}

	/**
	 * Decodes forwarded server names from JSON or SQL cache storage.
	 *
	 * @param encoded encoded server set
	 * @return decoded server names
	 */
	public static Set<String> decodeBroadcastForwardedServers(String encoded) {
		Set<String> servers = new LinkedHashSet<>();
		if (encoded == null || encoded.isEmpty()) {
			return servers;
		}
		Base64.Decoder decoder = Base64.getUrlDecoder();
		for (String value : encoded.split(",")) {
			try {
				servers.add(new String(decoder.decode(value), StandardCharsets.UTF_8));
			} catch (IllegalArgumentException ignored) {
				// Ignore malformed cache entries and keep the broadcast pending.
			}
		}
		return servers;
	}
}
