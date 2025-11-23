package com.bencodez.votingplugin.proxy.cache.nonvoted;

import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.bencodez.simpleapi.sql.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.config.MysqlConfig;

/**
 * Proxy-agnostic non-voted players cache.
 *
 * - Storage is provided by INonVotedPlayersStorage (JSON or MySQL). - Vote
 * existence check is provided via Predicate<String> (uuid -> hasVotes). - Debug
 * logging via Consumer<String>.
 *
 * Works for both Bungee and Velocity since only uuid + playerName are passed
 * in.
 */
public abstract class NonVotedPlayersCache {

	private INonVotedPlayersStorage storage;

	// 5 days in millis (same as your current logic)
	private static final long MAX_AGE_MILLIS = TimeUnit.DAYS.toMillis(5);

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

	public abstract boolean userExists(String uuid);

	/**
	 * Add a player directly by uuid + name, only if they don't already have votes.
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
	 */
	public String getUUID(String playerName) {
		return storage.getUuidByPlayerName(playerName);
	}

	public void close() {
		storage.close();
	}

	public abstract void logInfo1(String msg);

	public abstract void logSevere1(String msg);

	public abstract void debug1(Exception e);

	public abstract void debug1(String msg);
}
