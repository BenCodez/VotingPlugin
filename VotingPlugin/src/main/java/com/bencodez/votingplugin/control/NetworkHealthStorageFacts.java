package com.bencodez.votingplugin.control;

import com.bencodez.simpleapi.sql.mysql.ConnectionManager;
import com.google.gson.JsonObject;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HexFormat;

/** Existing pool state only: never acquires a connection or initializes storage. */
public final class NetworkHealthStorageFacts {
    private NetworkHealthStorageFacts() { }
    public static void add(JsonObject out, ConnectionManager manager) {
        if (manager == null) { out.addProperty("databaseInitialized", false); return; }
        out.addProperty("databaseInitialized", !manager.isClosed());
        if (manager.getDbType() != null) {
            out.addProperty("databaseType", manager.getDbType().name());
            String override = manager.getMysqlDriver();
            boolean present;
            if (override != null && !override.isBlank()) present = driver(override);
            else present = switch (manager.getDbType()) {
                case MYSQL -> driver("com.mysql.cj.jdbc.Driver");
                case MARIADB -> driver("org.mariadb.jdbc.Driver") || manager.isMariadbFallbackToMysqlDriver() && driver("com.mysql.cj.jdbc.Driver");
                case POSTGRESQL -> driver("org.postgresql.Driver");
            };
            out.addProperty("jdbcDriverAvailable", !manager.isClosed() || present);
            try {
                String identity = "VotingPlugin-storage-v1\0" + (manager.getDbType().isMySqlFamily() ? "MYSQL" : manager.getDbType().name()) + "\0" + manager.getHost()
                        + "\0" + manager.getPort() + "\0" + manager.getDatabase();
                out.addProperty("storageFingerprint", HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(identity.getBytes(StandardCharsets.UTF_8))));
            } catch (java.security.NoSuchAlgorithmException impossible) { throw new IllegalStateException(impossible); }
        }
    }
    private static boolean driver(String name) {
        try { Class.forName(name, false, NetworkHealthStorageFacts.class.getClassLoader()); return true; }
        catch (ClassNotFoundException | LinkageError unavailable) { return false; }
    }
}
