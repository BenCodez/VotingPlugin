package com.bencodez.votingplugin.proxy.redis;

/** Canonical Redis channel names for one configured VotingPlugin namespace. */
public final class VotingPluginRedisChannels {
	private VotingPluginRedisChannels() {
	}

	public static String proxy(String prefix) {
		return prefix(prefix) + "VotingPlugin";
	}

	public static String backend(String prefix, String server) {
		return prefix(prefix) + "VotingPlugin_" + server;
	}

	public static String multiProxy(String prefix, String proxyServer) {
		return prefix(prefix) + "VotingPluginProxy_" + proxyServer;
	}

	private static String prefix(String prefix) {
		return prefix == null ? "" : prefix;
	}
}
