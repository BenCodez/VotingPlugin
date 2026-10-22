package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

class NeoForgeProxyTransportConfigurationTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration";
    @TempDir Path directory;

    @Test void disabledProxyIgnoresInactiveInvalidConnectionSettings() throws Exception {
        var view = load("""
                UseBungeecord: false
                BungeeMethod: REDIS
                Redis:
                  Port: invalid
                """, "Database: invalid");
        assertEquals("DISABLED", field(view, "method").toString());
        assertEquals(false, field(view, "enabled"));
    }

    @Test void legacyEnableSpellingStillSelectsRedis() throws Exception {
        var view = load(base("REDIS").replace("UseBungeecord", "UseBungeecoord") + "Redis: {Host: redis.example}\n", "{}");
        assertEquals("REDIS", field(view, "method").toString());
        assertEquals(true, field(view, "enabled"));
    }

    @Test void explicitModernDisableOverridesLegacyEnable() throws Exception {
        var view = load("UseBungeecord: false\nUseBungeecoord: true\nBungeeMethod: REDIS\n", "{}");
        assertEquals(false, field(view, "enabled"));
    }

    @Test void existingSocketsKeepTheirConfigurationContract() throws Exception {
        var view = load(base("sOcKeTs").replace("BungeeServer: {Name: proxy-test}\n", "") + """
                BungeeServer:
                  Name: proxy-test
                  Host: proxy.example
                  Port: 1297
                SpigotServer:
                  Host: backend.example
                  Port: 1298
                SocketAuthenticationKeyFile: socket-auth.key
                CommunicationEncryption: true
                """, "{}");
        Object socket = field(view, "socket");
        assertEquals(true, field(socket, "enabled"));
        assertEquals("backend-test", field(socket, "server"));
        assertEquals("proxy-test", field(socket, "proxyName"));
        assertEquals(1298, field(socket, "backendPort"));
        assertEquals("socket-auth.key", field(socket, "authenticationKeyFile"));
        assertEquals(true, field(view, "communicationEncryption"));
    }

    @Test void redisUsesCapturedCredentialsAndPreservesRoutingPrefixExactly() throws Exception {
        var view = load(base("REDIS") + """
                SharedTransportAuthentication: REQUIRED
                Redis:
                  Host: redis.example
                  Port: '6380'
                  Username: fixture-user
                  Password: fixture-secret
                  Db-Index: 2
                  SSL: true
                  Prefix: ' network:'
                MQTT: {BrokerURL: invalid}
                """, "Database: invalid");
        Object redis = field(view, "redis");
        assertEquals("redis.example", field(redis, "host"));
        assertEquals(6380, field(redis, "port"));
        assertEquals(2, field(redis, "database"));
        assertEquals(true, field(redis, "ssl"));
        assertEquals(" network:", field(redis, "prefix"));
        assertEquals("REQUIRED", field(view, "sharedAuthentication").toString());
        assertEquals("fixture-secret", field(redis, "password"));
        assertFalse(view.toString().contains("fixture-secret"));
        assertFalse(redis.toString().contains("fixture-secret"));
    }

    @Test void mqttUsesExistingBrokerKeysAndServerDefaultClientIdentity() throws Exception {
        var view = load(base("MQTT") + """
                MQTT:
                  BrokerURL: ssl://broker.example:8883
                  Username: fixture-user
                  Password: fixture-secret
                  Prefix: network/
                """, "{}");
        Object mqtt = field(view, "mqtt");
        assertEquals("ssl://broker.example:8883", field(mqtt, "brokerUrl"));
        assertEquals("backend-test", field(mqtt, "clientId"));
        assertEquals("network/", field(mqtt, "prefix"));
        assertEquals("COMPATIBILITY", field(view, "sharedAuthentication").toString());
        assertFalse(mqtt.toString().contains("fixture-secret"));
    }

    @Test void mysqlTransportDoesNotSelectOrMutateMysqlUserStorage() throws Exception {
        ConfigurationNode config = yaml("""
                DataStorage: SQLITE
                Database:
                  Host: database.example
                  Database: votes
                  Username: fixture-user
                  Password: fixture-secret
                  Port: 3307
                  UseSSL: true
                  PublicKeyRetrieval: false
                  MaxConnections: 2
                """);
        Object original = config.raw();
        Object view = load(yaml(base("MYSQL")), config);
        Object mysql = field(view, "mysql");
        assertEquals("database.example", field(mysql, "host"));
        assertEquals(3307, field(mysql, "port"));
        assertEquals("votes", field(mysql, "database"));
        assertEquals(2, field(mysql, "maxConnections"));
        assertEquals(true, field(mysql, "ssl"));
        assertEquals(false, field(mysql, "publicKeyRetrieval"));
        assertEquals("SQLITE", config.node("DataStorage").getString());
        assertEquals(original, config.raw());
        assertFalse(mysql.toString().contains("fixture-secret"));
    }

    @Test void mysqlServerIdentityFitsTheExistingQueueSchemaWithoutChangingBrokerLimits() throws Exception {
        String database = "Database: {Host: database.example, Database: votes, Username: fixture-user}\n";
        assertEquals("a".repeat(36), field(load(base("MYSQL").replace("backend-test", "a".repeat(36)), database), "server"));
        assertThrows(IllegalArgumentException.class,
                () -> load(base("MYSQL").replace("backend-test", "a".repeat(37)), database));
        assertEquals("a".repeat(64), field(load(base("REDIS").replace("backend-test", "a".repeat(64))
                + "Redis: {Host: redis.example}\n", "{}"), "server"));
    }

    @Test void opaqueCredentialsAndExplicitClientIdentityRetainTheirExactContents() throws Exception {
        String credentials = "  Username: ' fixture-user '\n  Password: |-\n    fixture\n    secret\n";
        for (String method : new String[] {"REDIS", "MQTT", "MYSQL"}) {
            String bungee = base(method);
            String config = "{}";
            if (method.equals("REDIS")) bungee += "Redis:\n  Host: redis.example\n" + credentials;
            else if (method.equals("MQTT")) bungee += "MQTT:\n  ClientID: ' client identity '\n" + credentials;
            else config = "Database:\n  Host: database.example\n  Database: votes\n" + credentials;
            Object view = load(bungee, config);
            Object connection = field(view, method.toLowerCase(java.util.Locale.ROOT));
            assertEquals(" fixture-user ", field(connection, "username"));
            assertEquals("fixture\nsecret", field(connection, "password"));
            if (method.equals("MQTT")) assertEquals(" client identity ", field(connection, "clientId"));
            assertFalse(connection.toString().contains("fixture"));
        }
    }

    @Test void unsupportedEnabledMethodsAreRejected() {
        for (String method : new String[] {"PLUGINMESSAGING", "HTTP", "DISABLED", "unknown", " SOCKETS "}) {
            assertThrows(IllegalArgumentException.class, () -> load(base(method), "{}"), method);
        }
    }

    @Test void malformedPortsAndCountersDoNotSilentlyUseDefaults() {
        for (String value : new String[] {"0", "65536", "-1", "1.5", "not-a-port", "true", "[1, 2]", "{value: 2}"}) {
            assertThrows(IllegalArgumentException.class,
                    () -> load(base("REDIS") + "Redis: {Host: redis.example, Port: " + value + "}\n", "{}"), value);
        }
        assertThrows(IllegalArgumentException.class,
                () -> load(base("REDIS") + "Redis: {Host: redis.example, Db-Index: -1}\n", "{}"));
    }

    @Test void validBoundaryPortsAndDatabaseIndicesAreAccepted() throws Exception {
        for (int port : new int[] {1, 65535}) {
            Object view = load(base("REDIS") + "Redis: {Host: redis.example, Port: " + port
                    + ", Db-Index: 2147483647}\n", "{}");
            assertEquals(port, field(field(view, "redis"), "port"));
            assertEquals(Integer.MAX_VALUE, field(field(view, "redis"), "database"));
        }
    }

    @Test void requiredPeersAndTextTypesAreValidated() {
        for (String server : new String[] {"PleaseSet", "invalid/name", "''", "[backend]"}) {
            assertThrows(IllegalArgumentException.class,
                    () -> load(base("REDIS").replace("Server: backend-test", "Server: " + server)
                            + "Redis: {Host: redis.example}\n", "{}"));
        }
        assertThrows(IllegalArgumentException.class,
                () -> load(base("REDIS") + "Redis: {Host: {value: redis.example}}\n", "{}"));
    }

    @Test void brokerUrisRejectMalformedSchemesPortsAndEmbeddedCredentialsWithoutEchoingThem() {
        for (String uri : new String[] {"http://broker.example", "tcp:///missing-host", "tcp://broker.example:65536",
                "tcp://fixture-user:fixture-secret@broker.example:1883", "tcp://broker.example:0", "not a URI"}) {
            var failure = assertThrows(IllegalArgumentException.class,
                    () -> load(base("MQTT") + "MQTT: {BrokerURL: '" + uri + "'}\n", "{}"));
            assertFalse(failure.getMessage().contains(uri));
            assertNull(failure.getCause());
        }
    }

    @Test void invalidTransportBooleansAreNotCoerced() {
        assertThrows(IllegalArgumentException.class,
                () -> load(base("REDIS") + "Redis: {Host: redis.example, SSL: perhaps}\n", "{}"));
        assertThrows(IllegalArgumentException.class,
                () -> load(base("MQTT") + "CommunicationEncryption: [true]\n", "{}"));
    }

    @Test void mysqlRejectsAnotherDatabaseProtocolAndUnboundedPools() {
        for (String extra : new String[] {"DbType: POSTGRESQL", "MaxConnections: 0", "MaxConnections: 65"}) {
            assertThrows(IllegalArgumentException.class, () -> load(base("MYSQL"),
                    "Database: {Host: database.example, Database: votes, Username: fixture-user, " + extra + "}\n"));
        }
    }

    @Test void invalidSettingMessagesNameOnlyTheKeyAndNeverItsValue() throws Exception {
        Object[][] cases = {
                {"Server", 1729, "Server"},
                {"Redis.Prefix", 1729, "Prefix"},
                {"Redis.Password", 1729, "Password"},
                {"Redis.Port", "synthetic-invalid-private-port", "Port"},
                {"Redis.Port", 65536, "Port"},
                {"Redis.SSL", "synthetic-invalid-private-flag", "SSL"}
        };
        for (Object[] example : cases) {
            ConfigurationNode bungee = yaml(base("REDIS") + "Redis: {Host: redis.example}\n");
            bungee.node(((String) example[0]).split("\\.")).set(example[1]);
            var failure = assertThrows(IllegalArgumentException.class, () -> load(bungee, yaml("{}")));
            assertTrue(failure.getMessage().contains((String) example[2]), failure.getMessage());
            assertFalse(failure.getMessage().contains(String.valueOf(example[1])));
            assertNull(failure.getCause());
        }
    }

    private static String base(String method) {
        return "UseBungeecord: true\nBungeeMethod: '" + method
                + "'\nServer: backend-test\nBungeeServer: {Name: proxy-test}\n";
    }

    private Object load(String bungee, String config) throws IOException { return load(yaml(bungee), yaml(config)); }

    private static Object load(ConfigurationNode bungee, ConfigurationNode config) {
        return ReflectiveAccess.call(OWNER, null, "load",
                new String[] {"ConfigurationNode", "ConfigurationNode"},
                new Object[] {bungee, config});
    }

    private ConfigurationNode yaml(String text) throws IOException {
        Path path = Files.createTempFile(directory, "transport-", ".yml");
        Files.writeString(path, text);
        return YamlConfigurationLoader.builder().path(path).build().load();
    }

    private static Object field(Object value, String name) {
        return ReflectiveAccess.call(value.getClass().getName(), value, name, new String[0], new Object[0]);
    }
}
