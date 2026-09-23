package com.bencodez.votingplugin.placeholders;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.entity.Player;

/**
 * Immutable snapshots of live Bukkit players captured at player-owned lifecycle
 * boundaries. Storage workers may inspect membership and pass the captured owner
 * to the Folia-aware scheduler, but must not call methods on the player directly.
 */
public final class PlaceholderPlayerPresence {
	private final AtomicReference<Map<UUID, Player>> onlinePlayers = new AtomicReference<>(Map.of());
	private final Object lifecycleLock = new Object();

	public boolean isOnline(UUID uuid) {
		return uuid != null && onlinePlayers.get().containsKey(uuid);
	}

	public Player schedulerOwner(UUID uuid) {
		return uuid == null ? null : onlinePlayers.get().get(uuid);
	}

	/** Resolve the storage key already published for this exact scheduler owner. */
	public UUID storageUuid(Player player) {
		if (player == null) return null;
		for (Map.Entry<UUID, Player> entry : onlinePlayers.get().entrySet()) {
			if (entry.getValue() == player) return entry.getKey();
		}
		return null;
	}

	public void playerOnline(Player player) {
		if (player == null) return;
		playerOnline(player.getUniqueId(), player);
	}

	/** Publish the storage UUID with the captured Bukkit scheduler owner. */
	public void playerOnline(UUID uuid, Player player) {
		if (uuid == null || player == null) return;
		synchronized (lifecycleLock) {
			update(current -> {
				Map<UUID, Player> next = new HashMap<>(current);
				next.entrySet().removeIf(entry -> entry.getValue() == player && !entry.getKey().equals(uuid));
				next.put(uuid, player);
				return Map.copyOf(next);
			});
		}
	}

	public void playerOffline(UUID uuid) {
		if (uuid == null) return;
		synchronized (lifecycleLock) {
			update(current -> {
				if (!current.containsKey(uuid)) return current;
				Map<UUID, Player> next = new HashMap<>(current);
				next.remove(uuid);
				return Map.copyOf(next);
			});
		}
	}

	/** Remove only the retired scheduler owner, preserving a concurrently joined replacement. */
	public void playerOffline(UUID uuid, Player expectedOwner) {
		if (uuid == null || expectedOwner == null) return;
		synchronized (lifecycleLock) {
			update(current -> {
				if (current.get(uuid) != expectedOwner) return current;
				Map<UUID, Player> next = new HashMap<>(current);
				next.remove(uuid);
				return Map.copyOf(next);
			});
		}
	}

	public void replace(Collection<? extends Player> players) {
		Map<UUID, Player> next = new HashMap<>();
		if (players != null) {
			for (Player player : players) {
				if (player != null) next.put(player.getUniqueId(), player);
			}
		}
		synchronized (lifecycleLock) { onlinePlayers.set(Map.copyOf(next)); }
	}

	/** Replace presence with storage UUIDs resolved at a platform-owned boundary. */
	public void replace(Map<UUID, ? extends Player> players) {
		Map<UUID, Player> next = new HashMap<>();
		if (players != null) players.forEach((uuid, player) -> {
			if (uuid != null && player != null) next.put(uuid, player);
		});
		synchronized (lifecycleLock) { onlinePlayers.set(Map.copyOf(next)); }
	}

	public void clear() {
		synchronized (lifecycleLock) { onlinePlayers.set(Map.of()); }
	}

	/** Publish an offline-only result atomically with player lifecycle capture. */
	public boolean runIfOffline(UUID uuid, Runnable publication) {
		if (uuid == null || publication == null) return false;
		synchronized (lifecycleLock) {
			if (onlinePlayers.get().containsKey(uuid)) return false;
			publication.run();
			return true;
		}
	}

	private void update(java.util.function.Function<Map<UUID, Player>, Map<UUID, Player>> operation) {
		Map<UUID, Player> current;
		Map<UUID, Player> next;
		do {
			current = onlinePlayers.get();
			next = operation.apply(current);
			if (next == current) return;
		} while (!onlinePlayers.compareAndSet(current, next));
	}
}
