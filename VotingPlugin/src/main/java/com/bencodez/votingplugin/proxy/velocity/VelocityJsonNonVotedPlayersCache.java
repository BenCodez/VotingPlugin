package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.util.function.Consumer;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage;

public class VelocityJsonNonVotedPlayersCache extends VelocityJSONFile
        implements INonVotedPlayersStorage {

    public VelocityJsonNonVotedPlayersCache(File file) {
        super(file);
    }

    @Override
    public void upsertPlayer(String uuid, String playerName, long lastTime) {
        getNode("NonVotedPlayers", playerName, "UUID").setValue(uuid);
        getNode("NonVotedPlayers", playerName, "LastTime").setValue(lastTime);
        save();
    }

    @Override
    public String getUuidByPlayerName(String playerName) {
        if (!getNode("NonVotedPlayers", playerName).isEmpty()) {
            return getNode("NonVotedPlayers", playerName, "UUID").getString("");
        }
        return "";
    }

    @Override
    public void removeByPlayerName(String playerName) {
        getNode("NonVotedPlayers", playerName).setValue(null);
        save();
    }

    @Override
    public void forEach(Consumer<NonVotedPlayerEntry> consumer) {
        // If "NonVotedPlayers" doesn't exist, getNode(...) will just be empty and
        // getKeys(...) will be an empty set.
        for (String player : getKeys(getNode("NonVotedPlayers"))) {
            long time = getNode("NonVotedPlayers", player, "LastTime").getLong(0);
            String uuid = getNode("NonVotedPlayers", player, "UUID").getString("");
            consumer.accept(new NonVotedPlayerEntry(player, uuid, time));
        }
    }

    @Override
    public void close() {

    }
}
