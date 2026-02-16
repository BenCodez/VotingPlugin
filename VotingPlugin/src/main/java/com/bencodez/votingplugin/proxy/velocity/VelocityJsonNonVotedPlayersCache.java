package com.bencodez.votingplugin.proxy.velocity;

import java.io.File;
import java.util.function.Consumer;

import com.bencodez.simpleapi.file.velocity.VelocityJSONFile;
import com.bencodez.votingplugin.proxy.cache.nonvoted.INonVotedPlayersStorage;

/**
 * JSON-based non-voted players cache for Velocity proxy.
 */
public class VelocityJsonNonVotedPlayersCache extends VelocityJSONFile implements INonVotedPlayersStorage {

	/**
	 * Constructs a new Velocity JSON non-voted players cache.
	 * @param file the file to store cache data
	 */
	public VelocityJsonNonVotedPlayersCache(File file) {
		super(file);
	}

	@Override
	public void upsertPlayer(String uuid, String playerName, long lastTime) {
		setPath(uuid, "NonVotedPlayers", playerName, "UUID");
		setPath(lastTime, "NonVotedPlayers", playerName, "LastTime");
		save();
	}

	@Override
	public String getUuidByPlayerName(String playerName) {
		if (!getNode("NonVotedPlayers", playerName).empty()) {
			return getNode("NonVotedPlayers", playerName, "UUID").getString("");
		}
		return "";
	}

	@Override
	public void removeByPlayerName(String playerName) {
		remove("NonVotedPlayers", playerName);
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

	/**
	 * Sets a value at a specific path.
	 * @param value the value to set
	 * @param path the path elements
	 */
	private void setPath(Object value, Object... path) {
		set(path, value);
	}

}