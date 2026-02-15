package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.util.function.Consumer;

/**
 * Interface for storing non-voted players data.
 */
public interface INonVotedPlayersStorage {

    /**
     * Insert or update a player entry.
     *
     * @param uuid the player UUID
     * @param playerName the player name
     * @param lastTime the last recorded time
     */
    void upsertPlayer(String uuid, String playerName, long lastTime);

    /**
     * Returns UUID for the player or empty string if not found.
     *
     * @param playerName the player name
     * @return the UUID or empty string
     */
    String getUuidByPlayerName(String playerName);

    /**
     * Remove a player from storage.
     *
     * @param playerName the player name
     */
    void removeByPlayerName(String playerName);

    /**
     * Iterate snapshot of all entries.
     *
     * @param consumer the consumer to process each entry
     */
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
