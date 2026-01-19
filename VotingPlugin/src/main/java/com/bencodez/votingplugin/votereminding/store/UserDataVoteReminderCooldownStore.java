package com.bencodez.votingplugin.votereminding.store;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Uses VotingPluginUser.getUserData() cached columns:
 * - VoteRemindersLast (BIGINT)
 * - VoteRemindersMap  (LONGTEXT)  format: name=ms;name=ms
 *
 * NOTE: tryClaimGlobal is best-effort and not proxy-atomic unless your storage layer
 * supports conditional updates.
 */
public final class UserDataVoteReminderCooldownStore implements VoteReminderCooldownStore {

	public static final String KEY_GLOBAL_LAST = "VoteRemindersLast";
	public static final String KEY_MAP = "VoteRemindersMap";

	private final VotingPluginMain plugin;

	private final ConcurrentHashMap<UUID, Map<String, Long>> mapCache = new ConcurrentHashMap<>();
	private final ConcurrentHashMap<UUID, Object> locks = new ConcurrentHashMap<>();

	public UserDataVoteReminderCooldownStore(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@Override
	public boolean tryClaimGlobal(UUID uuid, long nowMs, long globalCooldownMs) {
		if (globalCooldownMs <= 0) {
			return true;
		}

		Object lock = locks.computeIfAbsent(uuid, k -> new Object());
		synchronized (lock) {
			VotingPluginUser user = getUser(uuid);
			if (user == null) {
				return false;
			}

			long last = readLong(user, KEY_GLOBAL_LAST);
			if (last > 0 && (nowMs - last) < globalCooldownMs) {
				return false;
			}

			writeLong(user, KEY_GLOBAL_LAST, nowMs);
			return true;
		}
	}

	@Override
	public Map<String, Long> getPerReminderMap(UUID uuid) {
		Map<String, Long> cached = mapCache.get(uuid);
		if (cached != null) {
			return cached;
		}

		VotingPluginUser user = getUser(uuid);
		if (user == null) {
			return Collections.emptyMap();
		}

		String raw = safe(user.getUserData().getString(KEY_MAP));
		Map<String, Long> parsed = parseMap(raw);

		mapCache.put(uuid, parsed);
		return parsed;
	}

	@Override
	public void setPerReminderLast(UUID uuid, String reminderName, long nowMs) {
		if (reminderName == null || reminderName.isEmpty()) {
			return;
		}

		Object lock = locks.computeIfAbsent(uuid, k -> new Object());
		synchronized (lock) {
			VotingPluginUser user = getUser(uuid);
			if (user == null) {
				return;
			}

			Map<String, Long> map = getPerReminderMap(uuid);
			if (!(map instanceof HashMap)) {
				map = new HashMap<>(map);
				mapCache.put(uuid, map);
			}

			map.put(reminderName, nowMs);

			String encoded = encodeMap(map);
			user.getUserData().setString(KEY_MAP, encoded);
		}
	}

	@Override
	public boolean isPersistent(UUID uuid) {
		VotingPluginUser user = getUser(uuid);
		if (user == null) {
			return false;
		}
		return user.getData().hasData();
	}

	public void clearCache(UUID uuid) {
		mapCache.remove(uuid);
		locks.remove(uuid);
	}

	private VotingPluginUser getUser(UUID uuid) {
		return plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
	}

	private long readLong(VotingPluginUser user, String key) {
		String s = safe(user.getUserData().getString(key));
		if (s.isEmpty() || s.equalsIgnoreCase("null")) {
			return 0L;
		}
		try {
			return Long.parseLong(s);
		} catch (NumberFormatException e) {
			return 0L;
		}
	}

	private void writeLong(VotingPluginUser user, String key, long value) {
		user.getUserData().setString(key, Long.toString(Math.max(0L, value)));
	}

	private String safe(String s) {
		return s == null ? "" : s.trim();
	}

	private Map<String, Long> parseMap(String raw) {
		HashMap<String, Long> out = new HashMap<>();
		if (raw == null || raw.isEmpty()) {
			return out;
		}

		String[] parts = raw.split(";");
		for (String part : parts) {
			if (part == null || part.isEmpty())
				continue;

			int idx = part.indexOf('=');
			if (idx <= 0 || idx >= part.length() - 1)
				continue;

			String name = part.substring(0, idx).trim();
			String msStr = part.substring(idx + 1).trim();
			if (name.isEmpty() || msStr.isEmpty())
				continue;

			try {
				long ms = Long.parseLong(msStr);
				if (ms > 0) {
					out.put(name, ms);
				}
			} catch (NumberFormatException ignore) {
			}
		}

		return out;
	}

	private String encodeMap(Map<String, Long> map) {
		if (map == null || map.isEmpty()) {
			return "";
		}

		StringBuilder sb = new StringBuilder();
		map.entrySet().stream().sorted((a, b) -> String.CASE_INSENSITIVE_ORDER.compare(a.getKey(), b.getKey()))
				.forEach(e -> {
					if (e.getKey() == null)
						return;
					long v = e.getValue() == null ? 0L : e.getValue().longValue();
					if (v <= 0L)
						return;

					if (sb.length() > 0)
						sb.append(';');
					sb.append(e.getKey()).append('=').append(v);
				});

		return sb.toString();
	}
}
