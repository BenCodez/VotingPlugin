package com.bencodez.votingplugin.timequeue;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashSet;
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
