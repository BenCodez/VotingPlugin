package com.bencodez.votingplugin.neoforge;

import java.math.BigDecimal;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Locale;
import java.util.Objects;

import org.spongepowered.configurate.ConfigurationNode;

import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;

/** Captured transport settings; loading this view never opens a connection or changes user storage. */
record NeoForgeProxyTransportConfiguration(Method method, String server, String proxyName,
        Mode sharedAuthentication, boolean communicationEncryption, boolean debug,
        NeoForgeProxySocketConfiguration socket, Redis redis, Mqtt mqtt, Mysql mysql) {
    enum Method { DISABLED, SOCKETS, REDIS, MQTT, MYSQL }

    record Redis(String host, int port, String username, String password, int database,
            boolean ssl, String prefix) {
        @Override public String toString() { return "Redis[configured]"; }
    }

    record Mqtt(String brokerUrl, String clientId, String username, String password, String prefix) {
        @Override public String toString() { return "Mqtt[configured]"; }
    }

    record Mysql(String host, int port, String database, String username, String password,
            boolean ssl, boolean publicKeyRetrieval, int maxConnections) {
        @Override public String toString() { return "Mysql[configured]"; }
    }

    static NeoForgeProxyTransportConfiguration load(ConfigurationNode bungee, ConfigurationNode config) {
        Objects.requireNonNull(bungee, "bungee");
        Objects.requireNonNull(config, "config");
        // Preserve the existing selector and its historical misspelling.
        if (!bungee.node("UseBungeecord").getBoolean(bungee.node("UseBungeecoord").getBoolean(false))) {
            return new NeoForgeProxyTransportConfiguration(Method.DISABLED, "", "", Mode.REQUIRED,
                    false, false, null, null, null, null);
        }
        Method method;
        try {
            method = Method.valueOf(text(bungee.node("BungeeMethod"), "PLUGINMESSAGING", 32)
                    .toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            throw invalid("BungeeMethod must select SOCKETS, REDIS, MQTT or MYSQL");
        }
        if (method == Method.DISABLED) throw invalid("BungeeMethod must select an enabled transport");
        if (method == Method.SOCKETS) {
            var socket = NeoForgeProxySocketConfiguration.load(bungee);
            return new NeoForgeProxyTransportConfiguration(method, socket.server(), socket.proxyName(),
                    Mode.REQUIRED, socket.communicationEncryption(), socket.debug(), socket, null, null, null);
        }
        String server = identity(bungee.node("Server"), "Server");
        String proxy = method == Method.MYSQL ? "" : identity(bungee.node("BungeeServer", "Name"), "BungeeServer.Name");
        Mode authentication = Mode.REQUIRED;
        if (method == Method.REDIS || method == Method.MQTT) {
            authentication = Mode.parse(text(bungee.node("SharedTransportAuthentication"), "COMPATIBILITY", 32));
        }
        boolean encryption = bool(bungee.node("CommunicationEncryption"), false);
        boolean debug = bool(bungee.node("BungeeDebug"), false);
        Redis redis = null;
        Mqtt mqtt = null;
        Mysql mysql = null;
        switch (method) {
            case REDIS -> {
                var node = bungee.node("Redis");
                redis = new Redis(required(node.node("Host"), "Redis.Host", 253),
                        integer(node.node("Port"), 6379, 1, 65535),
                        credential(node.node("Username")), credential(node.node("Password")),
                        integer(node.node("Db-Index"), 0, 0, Integer.MAX_VALUE),
                        bool(node.node("SSL"), false), text(node.node("Prefix"), "", 256));
            }
            case MQTT -> {
                var node = bungee.node("MQTT");
                String url = text(node.node("BrokerURL"), "tcp://localhost:1883", 4096).trim();
                validateBrokerUrl(url);
                String client = text(node.node("ClientID"), "", 128);
                mqtt = new Mqtt(url, client.isEmpty() ? server : client,
                        credential(node.node("Username")), credential(node.node("Password")),
                        text(node.node("Prefix"), "", 256));
            }
            case MYSQL -> {
                if (server.length() > 36) throw invalid("MySQL transport server identity exceeds queue schema limit");
                // Transport credentials are independent of the SQLITE user-storage selection.
                var node = config.node("Database");
                String type = text(node.node("DbType"), "MYSQL", 32).trim().toUpperCase(Locale.ROOT);
                if (!type.equals("MYSQL") && !type.equals("MARIADB")) {
                    throw invalid("Database.DbType must support the MySQL transport protocol");
                }
                String username = credential(node.node("Username"));
                if (username.isEmpty()) throw invalid("Database.Username is required");
                mysql = new Mysql(required(node.node("Host"), "Database.Host", 253),
                        integer(node.node("Port"), 3306, 1, 65535),
                        required(node.node("Database"), "Database.Database", 64),
                        username, credential(node.node("Password")), bool(node.node("UseSSL"), false),
                        bool(node.node("PublicKeyRetrieval"), false), integer(node.node("MaxConnections"), 1, 1, 64));
            }
            default -> throw invalid("Unsupported proxy transport");
        }
        return new NeoForgeProxyTransportConfiguration(method, server, proxy, authentication,
                encryption, debug, null, redis, mqtt, mysql);
    }

    boolean enabled() { return method != Method.DISABLED; }

    @Override public String toString() {
        // A record's generated representation would otherwise include connection credentials.
        return "NeoForgeProxyTransportConfiguration[method=" + method + "]";
    }

    private static String identity(ConfigurationNode node, String path) {
        String value = required(node, path, 64);
        if (value.equalsIgnoreCase("PleaseSet") || !value.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
            throw invalid(path + " must name a configured peer");
        }
        return value;
    }

    private static String required(ConfigurationNode node, String path, int limit) {
        String value = text(node, "", limit).trim();
        if (value.isEmpty()) throw invalid(path + " is required");
        return value;
    }

    private static String text(ConfigurationNode node, String fallback, int limit) {
        Object raw = node.rawScalar();
        if (raw == null && node.virtual()) return fallback;
        if (!(raw instanceof String value) || value.length() > limit
                || value.chars().anyMatch(c -> c == 0 || c == '\r' || c == '\n')) {
            throw invalid("Invalid transport text setting at " + node.path());
        }
        return value;
    }

    private static String credential(ConfigurationNode node) {
        if (node.virtual()) return "";
        Object raw = node.rawScalar();
        if (!(raw instanceof String value) || value.length() > 4096) {
            throw invalid("Invalid transport credential setting at " + node.path());
        }
        // Broker/JDBC credentials are opaque data; do not trim or alter their contents.
        return value;
    }

    private static int integer(ConfigurationNode node, int fallback, int minimum, int maximum) {
        if (node.virtual()) return fallback;
        Object raw = node.rawScalar();
        int value;
        try {
            if (raw instanceof Number number) value = new BigDecimal(number.toString()).intValueExact();
            else if (raw instanceof String text && text.matches("-?[0-9]{1,10}")) value = Integer.parseInt(text);
            else throw invalid("Transport port/count must be an integer at " + node.path());
        } catch (NumberFormatException | ArithmeticException failure) {
            throw invalid("Transport port/count must be an integer at " + node.path());
        }
        if (value < minimum || value > maximum) throw invalid("Transport port/count is outside its supported range at " + node.path());
        return value;
    }

    private static boolean bool(ConfigurationNode node, boolean fallback) {
        if (node.virtual()) return fallback;
        Object raw = node.rawScalar();
        if (raw instanceof Boolean value) return value;
        if (raw instanceof String value && (value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false"))) {
            return Boolean.parseBoolean(value);
        }
        throw invalid("Transport flag must be true or false at " + node.path());
    }

    private static void validateBrokerUrl(String value) {
        try {
            URI uri = new URI(value);
            String scheme = uri.getScheme();
            if (scheme == null || !java.util.Set.of("tcp", "ssl", "ws", "wss").contains(scheme)
                    || uri.getHost() == null || uri.getRawUserInfo() != null
                    || uri.getFragment() != null || uri.getPort() == 0 || uri.getPort() > 65535) {
                throw invalid("MQTT.BrokerURL must select a supported broker URI without embedded credentials");
            }
        } catch (URISyntaxException failure) {
            throw invalid("MQTT.BrokerURL is invalid");
        }
    }

    private static IllegalArgumentException invalid(String message) { return new IllegalArgumentException(message); }
}
