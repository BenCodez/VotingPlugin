package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;

/**
 * Cache for players who haven't voted yet.
 */
public abstract class NonVotedPlayersCache {

	private INonVotedPlayersStorage storage;

	// 5 days in millis (same as your current logic)
	private static final long MAX_AGE_MILLIS = TimeUnit.DAYS.toMillis(5);

	/**
	 * Constructor for NonVotedPlayersCache.
	 * @param mysqlConfig MySQL configuration
	 * @param useMysql whether to use MySQL
	 * @param useExistingConnection whether to use existing connection
	 * @param mysql existing MySQL connection
	 * @param storage storage implementation
	 * @param debug enable debug mode
	 */
	public NonVotedPlayersCache(MysqlConfig mysqlConfig, boolean useMysql, boolean useExistingConnection, MySQL mysql,
			INonVotedPlayersStorage storage, boolean debug) {
		if (useMysql) {
			if (useExistingConnection) {
				this.storage = new MySQLNonVotedPlayersStorage(mysql, mysqlConfig.getTablePrefix(), debug) {

					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};
			} else {
				this.storage = new MySQLNonVotedPlayersStorage(mysqlConfig, debug) {

					@Override
					public void logSevere(String string) {
						logSevere1(string);
					}

					@Override
					public void logInfo(String string) {
						logInfo1(string);
					}

					@Override
					public void debug(Exception e) {
						if (debug)
							debug1(e);
					}
				};
			}

		} else {
			this.storage = storage;
		}
	}

	/**
	 * Check if user exists.
	 * @param uuid the player UUID
	 * @return true if user exists
	 */
	public abstract boolean userExists(String uuid);

	/**
	 * Add a player directly by uuid + name, only if they don't already have votes.
	 * @param uuid the player UUID
	 * @param playerName the player name
	 */
	public void addPlayer(String uuid, String playerName) {
		if (uuid == null || uuid.isEmpty()) {
			return;
		}
		if (!userExists(uuid)) {
			debug1("Adding nonvotedplayer: " + playerName + " / " + uuid);
			storage.upsertPlayer(uuid, playerName, System.currentTimeMillis());
		}
	}

	/**
	 * Get all UUIDs in cache.
	 * @return set of UUIDs
	 */
	public abstract Set<String> getAllUUIDs();

	/**
	 * Cleanup: - remove entries older than 5 days - remove entries whose UUID has
	 * now appeared in proxy MySQL - remove entries with empty/invalid UUID
	 */
	public void check() {
		final long now = System.currentTimeMillis();
		Set<String> uuids = getAllUUIDs();

		storage.forEach(entry -> {
			boolean remove = false;

			long time = entry.getLastTime();
			// too old?
			if ((now - time) > MAX_AGE_MILLIS) {
				remove = true;
			} else {
				String uuid = entry.getUuid();
				if (uuid == null || uuid.isEmpty()) {
					remove = true;
				} else if (uuids.contains(uuid)) {
					// player has now voted in the main proxy MySQL
					remove = true;
				}
			}

			if (remove) {
				debug1("Removing nonvotedplayer: " + entry.getPlayerName());
				storage.removeByPlayerName(entry.getPlayerName());
			}
		});
	}

	/**
	 * Returns the UUID for a cached non-voted player, or empty string if not
	 * present.
	 * @param playerName the player name
	 * @return the UUID or empty string
	 */
	public String getUUID(String playerName) {
		return storage.getUuidByPlayerName(playerName);
	}

	/**
	 * Close storage connections.
	 */
	public void close() {
		storage.close();
	}

	/**
	 * Log info message.
	 * @param msg the message
	 */
	public abstract void logInfo1(String msg);

	/**
	 * Log severe message.
	 * @param msg the message
	 */
	public abstract void logSevere1(String msg);

	/**
	 * Debug log exception.
	 * @param e the exception
	 */
	public abstract void debug1(Exception e);

	/**
	 * Debug log message.
	 * @param msg the message
	 */
	public abstract void debug1(String msg);
}
