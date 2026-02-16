package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.util.List;
import java.util.function.Consumer;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;

/**
 * MySQL storage for non-voted players.
 */
public abstract class MySQLNonVotedPlayersStorage extends ProxyNonVotedPlayersTable
        implements INonVotedPlayersStorage {

    private final boolean ownsConnection;

    /**
     * Constructor with existing MySQL connection.
     * @param existingMysql existing MySQL connection
     * @param tablePrefix table prefix
     * @param debug enable debug mode
     */
    public MySQLNonVotedPlayersStorage(MySQL existingMysql, String tablePrefix, boolean debug) {
        super(existingMysql, tablePrefix, debug);
        this.ownsConnection = false;
    }

    /**
     * Constructor with new MySQL configuration.
     * @param config MySQL configuration
     * @param debug enable debug mode
     */
    public MySQLNonVotedPlayersStorage(MysqlConfig config, boolean debug) {
        super(config, debug);
        this.ownsConnection = true;
    }

    @Override
    public void upsertPlayer(String uuid, String playerName, long lastTime) {
        super.upsertPlayer(uuid, playerName, lastTime);
    }

    @Override
    public String getUuidByPlayerName(String playerName) {
        return super.getUuidByPlayerName(playerName);
    }

    @Override
    public void removeByPlayerName(String playerName) {
        super.removeByPlayerName(playerName);
    }

    @Override
    public void forEach(Consumer<NonVotedPlayerEntry> consumer) {
        List<NonVotedPlayerRow> rows = getAllRows();
        for (NonVotedPlayerRow row : rows) {
            consumer.accept(new NonVotedPlayerEntry(
                    row.getPlayerName(),
                    row.getUuid(),
                    row.getLastTime()));
        }
    }

    @Override
    public void close() {
        if (ownsConnection) {
            getMysql().disconnect();
        }
    }
}
