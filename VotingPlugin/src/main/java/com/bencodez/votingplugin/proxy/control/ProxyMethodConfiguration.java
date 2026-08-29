package com.bencodez.votingplugin.proxy.control;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;

import com.bencodez.votingplugin.proxy.BungeeMethod;

/** Small, revisioned proxy transport selection exposed to Control. */
public record ProxyMethodConfiguration(BungeeMethod method) {
	public ProxyMethodConfiguration {
		if (method == null) throw new IllegalArgumentException("proxy method is required");
	}

	public String revision() {
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
					.digest(method.name().getBytes(StandardCharsets.UTF_8)));
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable", e);
		}
	}

	public List<String> changesFrom(ProxyMethodConfiguration current) {
		return method == current.method ? List.of() : List.of("change proxy method from "
				+ current.method.name() + " to " + method.name());
	}
}
