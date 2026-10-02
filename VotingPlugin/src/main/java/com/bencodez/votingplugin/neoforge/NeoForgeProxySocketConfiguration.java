package com.bencodez.votingplugin.neoforge;

import java.util.Locale;
import java.util.Objects;

import org.spongepowered.configurate.ConfigurationNode;

/** Validated NeoForge subset of the existing BungeeSettings socket configuration. */
record NeoForgeProxySocketConfiguration(boolean enabled, String server, String proxyName,
        String proxyHost, int proxyPort, String backendHost, int backendPort,
        String authenticationKeyFile, boolean communicationEncryption, boolean debug) {
    static NeoForgeProxySocketConfiguration load(ConfigurationNode root) {
        Objects.requireNonNull(root, "root");
        boolean useProxy = root.node("UseBungeecord").getBoolean(
                root.node("UseBungeecoord").getBoolean(false));
        String method = root.node("BungeeMethod").getString("PLUGINMESSAGING");
        if (!useProxy || !"SOCKETS".equalsIgnoreCase(method)) {
            return new NeoForgeProxySocketConfiguration(false, "", "", "", 0, "", 0, "", false, false);
        }
        String server = trimmed(root.node("Server").getString("PleaseSet"));
        String proxyName = trimmed(root.node("BungeeServer", "Name").getString(""));
        String proxyHost = trimmed(root.node("BungeeServer", "Host").getString(""));
        int proxyPort = root.node("BungeeServer", "Port").getInt(1297);
        String backendHost = trimmed(root.node("SpigotServer", "Host").getString("0.0.0.0"));
        int backendPort = root.node("SpigotServer", "Port").getInt(1298);
        String authenticationKeyFile = trimmed(root.node("SocketAuthenticationKeyFile").getString(""));
        if (server.isEmpty() || "pleaseset".equals(server.toLowerCase(Locale.ROOT))
                || !server.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
            throw new IllegalArgumentException("Server must name a configured backend");
        }
        if (proxyHost.isEmpty()) {
            throw new IllegalArgumentException("BungeeServer.Host is required for SOCKETS");
        }
        if (proxyName.isEmpty() || !proxyName.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
            throw new IllegalArgumentException("BungeeServer.Name must match ProxyServerName");
        }
        if (authenticationKeyFile.isEmpty()) {
            throw new IllegalArgumentException("SocketAuthenticationKeyFile is required for NeoForge SOCKETS");
        }
        if (backendHost.isEmpty()) backendHost = "0.0.0.0";
        validatePort(proxyPort, "BungeeServer.Port");
        validatePort(backendPort, "SpigotServer.Port");
        return new NeoForgeProxySocketConfiguration(true, server, proxyName, proxyHost, proxyPort,
                backendHost, backendPort, authenticationKeyFile,
                root.node("CommunicationEncryption").getBoolean(false),
                root.node("BungeeDebug").getBoolean(false));
    }

    private static String trimmed(String value) {
        return value == null ? "" : value.trim();
    }

    private static void validatePort(int port, String path) {
        if (port < 1 || port > 65535) throw new IllegalArgumentException(path + " must be between 1 and 65535");
    }
}
