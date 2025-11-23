package com.bencodez.votingplugin.proxy.bungee;

import java.io.File;
import java.util.function.Consumer;

import com.bencodez.simpleapi.file.BungeeJsonFile;
import com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage;

public class BungeeJsonNonVotedPlayersCache extends BungeeJsonFile
        implements INonVotedPlayersStorage {

    public BungeeJsonNonVotedPlayersCache(File file) {
        super(file);
    }

    @Override
    public void upsertPlayer(String uuid, String playerName, long lastTime) {
        setString("NonVotedPlayers." + playerName + ".UUID", uuid);
        setLong("NonVotedPlayers." + playerName + ".LastTime", lastTime);
        save();
    }

    @Override
    public String getUuidByPlayerName(String playerName) {
        String uuid = getString("NonVotedPlayers." + playerName + ".UUID", "");
        return uuid != null ? uuid : "";
    }

    @Override
    public void removeByPlayerName(String playerName) {
        remove("NonVotedPlayers." + playerName);
        save();
    }

    @Override
    public void forEach(Consumer<NonVotedPlayerEntry> consumer) {
        for (String player : getKeys("NonVotedPlayers")) {
            long time = getLong("NonVotedPlayers." + player + ".LastTime", 0);
            String uuid = getString("NonVotedPlayers." + player + ".UUID", "");
            consumer.accept(new NonVotedPlayerEntry(player, uuid, time));
        }
    }

    @Override
    public void close() {
    }
}
