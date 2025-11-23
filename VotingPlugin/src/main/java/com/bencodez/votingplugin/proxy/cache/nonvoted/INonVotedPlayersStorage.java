package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.util.function.Consumer;

public interface INonVotedPlayersStorage {

    /** Insert or update a player entry. */
    void upsertPlayer(String uuid, String playerName, long lastTime);

    /** Returns UUID for the player or empty string if not found. */
    String getUuidByPlayerName(String playerName);

    /** Remove a player from storage. */
    void removeByPlayerName(String playerName);

    /** Iterate snapshot of all entries. */
    void forEach(Consumer<NonVotedPlayerEntry> consumer);

    /** Close resources if needed (MySQL); no-op for JSON. */
    void close();

    final class NonVotedPlayerEntry {
        private final String playerName;
        private final String uuid;
        private final long lastTime;

        public NonVotedPlayerEntry(String playerName, String uuid, long lastTime) {
            this.playerName = playerName;
            this.uuid = uuid;
            this.lastTime = lastTime;
        }

        public String getPlayerName() {
            return playerName;
        }

        public String getUuid() {
            return uuid;
        }

        public long getLastTime() {
            return lastTime;
        }
    }
}
