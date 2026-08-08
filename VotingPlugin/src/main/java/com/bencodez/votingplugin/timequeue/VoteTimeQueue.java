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
	private boolean proxyBroadcastHandled;
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
		this(null, name, service, time, false, Collections.emptySet());
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
		this(voteId, name, service, time, false, Collections.emptySet());
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
		this.voteId = voteId;
		this.name = name;
		this.service = service;
		this.time = time;
		this.proxyBroadcastHandled = proxyBroadcastHandled;
		this.broadcastForwardedServers = new LinkedHashSet<>();
		if (broadcastForwardedServers != null) {
			this.broadcastForwardedServers.addAll(broadcastForwardedServers);
		}
	}

	/**
	 * Encodes forwarded server names for JSON and SQL cache storage.
	 *
	 * @return encoded server set
	 */
	public String encodeBroadcastForwardedServers() {
		Base64.Encoder encoder = Base64.getUrlEncoder().withoutPadding();
		StringBuilder encoded = new StringBuilder();
		for (String server : broadcastForwardedServers) {
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
