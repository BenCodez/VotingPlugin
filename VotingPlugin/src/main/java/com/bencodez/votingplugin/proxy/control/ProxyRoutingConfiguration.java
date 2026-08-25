package com.bencodez.votingplugin.proxy.control;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/** Typed, non-secret configuration domain supported by Control. */
public record ProxyRoutingConfiguration(boolean sendVotesToAllServers, List<String> blockedServers) {
	public ProxyRoutingConfiguration {
		blockedServers = blockedServers == null ? List.of() : blockedServers.stream().map(String::trim).toList();
		if (blockedServers.size() > 256) throw new IllegalArgumentException("too many blocked servers");
		Set<String> unique = new HashSet<>();
		for (String server : blockedServers) {
			if (server.isBlank() || server.length() > 100 || !unique.add(server.toLowerCase(Locale.ROOT))) {
				throw new IllegalArgumentException("blocked server names must be unique and 1 to 100 characters");
			}
		}
		blockedServers = List.copyOf(blockedServers);
	}

	public String revision() {
		String canonical = sendVotesToAllServers + "\n" + String.join("\n", blockedServers.stream()
				.map(value -> value.toLowerCase(Locale.ROOT)).sorted().toList());
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
					.digest(canonical.getBytes(StandardCharsets.UTF_8)));
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable", e);
		}
	}

	public List<String> changesFrom(ProxyRoutingConfiguration current) {
		List<String> changes = new ArrayList<>();
		if (sendVotesToAllServers != current.sendVotesToAllServers) {
			changes.add("sendVotesToAllServers: " + current.sendVotesToAllServers + " -> " + sendVotesToAllServers);
		}
		if (!blockedServers.equals(current.blockedServers)) {
			changes.add("blockedServers: " + current.blockedServers + " -> " + blockedServers);
		}
		return List.copyOf(changes);
	}
}
