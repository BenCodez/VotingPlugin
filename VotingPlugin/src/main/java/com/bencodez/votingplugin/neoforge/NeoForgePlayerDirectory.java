package com.bencodez.votingplugin.neoforge;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;

/** Tracks identity and online state from NeoForge player lifecycle events. */
public final class NeoForgePlayerDirectory {
    private final Map<UUID, SharedVoteIdentity> online = new ConcurrentHashMap<>();

    public void joined(Object player) {
        SharedVoteIdentity identity = identity(player);
        online.put(identity.uuid(), identity);
    }

    public void left(Object player) {
        online.remove((UUID) invoke(player, "getUUID"));
    }

    public Optional<SharedVoteIdentity> online(UUID uuid) {
        return Optional.ofNullable(online.get(uuid));
    }

    public void clear() {
        online.clear();
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
}
