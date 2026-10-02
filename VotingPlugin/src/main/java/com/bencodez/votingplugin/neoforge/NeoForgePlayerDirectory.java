package com.bencodez.votingplugin.neoforge;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Tracks identity and online state from NeoForge player lifecycle events. */
public final class NeoForgePlayerDirectory {
    private final Map<UUID, OnlinePlayer> online = new ConcurrentHashMap<>();
    private final Map<UUID, String> latestNames = new ConcurrentHashMap<>();

    public void joined(Object player) {
        joinedIdentity(player);
    }

    SharedVoteIdentity joinedIdentity(Object player) {
        SharedVoteIdentity identity = identity(player);
        latestNames.put(identity.uuid(), identity.playerName());
        online.put(identity.uuid(), new OnlinePlayer(identity, player));
        return identity;
    }

    void joined(SharedVoteIdentity identity) {
        Objects.requireNonNull(identity, "identity");
        latestNames.put(identity.uuid(), identity.playerName());
        online.put(identity.uuid(), new OnlinePlayer(identity, null));
    }

    public void left(Object player) {
        online.remove((UUID) invoke(player, "getUUID"));
    }

    public Optional<SharedVoteIdentity> online(UUID uuid) {
        return Optional.ofNullable(online.get(uuid)).map(OnlinePlayer::identity);
    }

    /** Latest UUID-bound name observed during this runtime, including after logout. */
    Optional<String> latestName(UUID uuid) {
        return Optional.ofNullable(latestNames.get(uuid));
    }

    Optional<Object> nativePlayer(UUID uuid) {
        return Optional.ofNullable(online.get(uuid)).map(OnlinePlayer::player);
    }

    public void clear() {
        online.clear();
        latestNames.clear();
    }

    /** NeoForge's universal API omits Minecraft classes on Maven's compile path. */
    static Object playerFromEvent(Object event) {
        return invoke(event, "getEntity");
    }

    private static SharedVoteIdentity identity(Object player) {
        UUID uuid = (UUID) invoke(player, "getUUID");
        Object name = invoke(player, "getName");
        return new SharedVoteIdentity(uuid, (String) invoke(name, "getString"), true);
    }

    private static Object invoke(Object receiver, String method) {
        try {
            return receiver.getClass().getMethod(method).invoke(receiver);
        } catch (ReflectiveOperationException e) {
            Throwable cause = e instanceof InvocationTargetException ? e.getCause() : e;
            throw new IllegalStateException("NeoForge player bridge cannot call " + method, cause);
        }
    }

    private record OnlinePlayer(SharedVoteIdentity identity, Object player) { }
}
