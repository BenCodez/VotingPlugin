package com.bencodez.votingplugin.placeholders;

import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.bukkit.entity.Player;

/**
 * Immutable snapshots of live Bukkit players captured at player-owned lifecycle
 * boundaries. Storage workers may inspect membership and pass the captured owner
 * to the Folia-aware scheduler, but must not call methods on the player directly.
 */
public final class PlaceholderPlayerPresence {
	private final AtomicReference<Map<UUID, Player>> onlinePlayers = new AtomicReference<>(Map.of());
	private final Object lifecycleLock = new Object();
	private final Map<Player, LiveIdentity> identities = new IdentityHashMap<>();
	private final Map<Player, Object> refreshReservations = new IdentityHashMap<>();

	/** Immutable identity; lookup callers never need to access its Bukkit owner. */
	public record LiveIdentity(UUID serverUuid, String playerName, UUID storageUuid) { }

	/** Capture only on this player's owning thread (join or entity scheduler). */
	public void captureOnline(UUID storageUuid, Player player) {
		if (storageUuid == null || player == null) return;
		LiveIdentity identity = new LiveIdentity(player.getUniqueId(), player.getName(), storageUuid);
		synchronized (lifecycleLock) {
			playerOnline(storageUuid, player);
			identities.put(player, identity);
		}
	}

	/** Rekey only the current owner. Delayed login cannot resurrect a retired session. */
	public boolean updateStorageUuid(UUID uuid, Player player) {
		if (uuid == null || player == null) return false;
		synchronized (lifecycleLock) {
			if (storageUuid(player) == null) return false;
			LiveIdentity identity = identities.get(player);
			playerOnline(uuid, player);
			if (identity != null) identities.put(player,
					new LiveIdentity(identity.serverUuid(), identity.playerName(), uuid));
			return true;
		}
	}

	/** Snapshot-only lookup, with exact storage identities taking precedence over aliases. */
	public LiveIdentity liveIdentity(UUID uuid) {
		if (uuid == null) return null;
		synchronized (lifecycleLock) {
			Player exact = onlinePlayers.get().get(uuid);
			if (exact != null) return identities.get(exact);
			LiveIdentity found = null;
			for (LiveIdentity identity : identities.values()) {
				if (!uuid.equals(identity.serverUuid())) continue;
				if (found != null) return null; // Uncertain or ambiguous: preserve the supplied UUID.
				found = identity;
			}
			return found;
		}
	}

	/**
	 * Refresh membership without reading another region's players. Each scheduled
	 * capture is fenced by its reservation; quit, join, another refresh and clear
	 * invalidate it before it can publish stale identity data.
	 */
	public void refresh(Supplier<? extends Collection<? extends Player>> playersSupplier,
			BiConsumer<Player, Consumer<UUID>> scheduleCapture) {
		Map<Player, Object> reservations = new IdentityHashMap<>();
		synchronized (lifecycleLock) {
			Collection<? extends Player> players = playersSupplier.get();
			Map<Player, Boolean> members = new IdentityHashMap<>();
			if (players != null) for (Player player : players) {
				if (player != null) members.put(player, Boolean.TRUE);
			}
			Map<UUID, Player> retained = new HashMap<>(onlinePlayers.get());
			retained.entrySet().removeIf(entry -> !members.containsKey(entry.getValue()));
			onlinePlayers.set(Map.copyOf(retained));
			identities.keySet().removeIf(player -> !retained.containsValue(player));
			refreshReservations.clear();
			members.keySet().forEach(player -> {
				Object reservation = new Object();
				refreshReservations.put(player, reservation);
				reservations.put(player, reservation);
			});
		}
		reservations.forEach((player, reservation) -> scheduleCapture.accept(player, storageUuid -> {
			synchronized (lifecycleLock) {
				if (refreshReservations.get(player) != reservation) return;
				UUID serverUuid = player.getUniqueId();
				if (identities.entrySet().stream().anyMatch(entry -> entry.getKey() != player
						&& serverUuid.equals(entry.getValue().serverUuid()))) return;
				captureOnline(storageUuid, player);
			}
		}));
	}

	private void retainCurrentIdentities() {
		identities.entrySet().removeIf(entry -> onlinePlayers.get().get(entry.getValue().storageUuid()) != entry.getKey());
	}

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
			refreshReservations.remove(player);
			update(current -> {
				Map<UUID, Player> next = new HashMap<>(current);
				next.entrySet().removeIf(entry -> entry.getValue() == player && !entry.getKey().equals(uuid));
				next.put(uuid, player);
				return Map.copyOf(next);
			});
			retainCurrentIdentities();
		}
	}

	public void playerOffline(UUID uuid) {
		if (uuid == null) return;
		synchronized (lifecycleLock) {
			Player owner = onlinePlayers.get().get(uuid);
			if (owner != null) refreshReservations.remove(owner);
			update(current -> {
				if (!current.containsKey(uuid)) return current;
				Map<UUID, Player> next = new HashMap<>(current);
				next.remove(uuid);
				return Map.copyOf(next);
			});
			retainCurrentIdentities();
		}
	}

	/** Remove only the retired scheduler owner, preserving a concurrently joined replacement. */
	public boolean playerOffline(UUID uuid, Player expectedOwner) {
		if (uuid == null || expectedOwner == null) return false;
		synchronized (lifecycleLock) {
			refreshReservations.remove(expectedOwner);
			Map<UUID, Player> current = onlinePlayers.get();
			if (current.get(uuid) != expectedOwner) return false;
			Map<UUID, Player> next = new HashMap<>(current);
			next.remove(uuid);
			onlinePlayers.set(Map.copyOf(next));
			retainCurrentIdentities();
			return true;
		}
	}

	/** Rebuild online owners while retaining storage UUIDs already established by login. */
	public void replace(Collection<? extends Player> players) {
		replace(players, Player::getUniqueId);
	}

	/** Resolve and replace one presence snapshot under the same lifecycle boundary. */
	public void replace(Collection<? extends Player> players, Function<Player, UUID> storageUuidResolver) {
		replace(() -> players, storageUuidResolver);
	}

	/** Capture, resolve, and replace one presence snapshot under the same lifecycle boundary. */
	public void replace(Supplier<? extends Collection<? extends Player>> playersSupplier,
			Function<Player, UUID> storageUuidResolver) {
		synchronized (lifecycleLock) {
			Collection<? extends Player> players = playersSupplier == null ? null : playersSupplier.get();
			Map<Player, UUID> knownStorageUuids = new IdentityHashMap<>();
			Map<UUID, UUID> storageUuidByPlayerUuid = new HashMap<>();
			onlinePlayers.get().forEach((uuid, player) -> {
				knownStorageUuids.put(player, uuid);
				storageUuidByPlayerUuid.put(player.getUniqueId(), uuid);
			});
			Map<UUID, Player> next = new HashMap<>();
			if (players != null) {
				for (Player player : players) {
					if (player == null) continue;
					UUID uuid = knownStorageUuids.get(player);
					UUID playerUuid = player.getUniqueId();
					if (uuid == null) uuid = storageUuidByPlayerUuid.get(playerUuid);
					if (uuid == null && storageUuidResolver != null) uuid = storageUuidResolver.apply(player);
					next.put(uuid == null ? playerUuid : uuid, player);
				}
			}
			onlinePlayers.set(Map.copyOf(next));
			refreshReservations.clear();
			retainCurrentIdentities();
		}
	}

	/** Replace presence with storage UUIDs resolved at a platform-owned boundary. */
	public void replace(Map<UUID, ? extends Player> players) {
		Map<UUID, Player> next = new HashMap<>();
		if (players != null) players.forEach((uuid, player) -> {
			if (uuid != null && player != null) next.put(uuid, player);
		});
		synchronized (lifecycleLock) { onlinePlayers.set(Map.copyOf(next)); refreshReservations.clear(); retainCurrentIdentities(); }
	}

	public void clear() {
		synchronized (lifecycleLock) { onlinePlayers.set(Map.of()); identities.clear(); refreshReservations.clear(); }
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
