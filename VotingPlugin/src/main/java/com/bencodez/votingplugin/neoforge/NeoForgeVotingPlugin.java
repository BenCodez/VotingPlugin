package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.neoforged.fml.common.Mod;
import net.neoforged.fml.loading.FMLPaths;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.server.ServerStartedEvent;
import net.neoforged.neoforge.event.server.ServerStoppingEvent;
import net.neoforged.neoforge.event.tick.ServerTickEvent;

/** Native NeoForge entry point. */
@Mod("votingplugin")
public final class NeoForgeVotingPlugin {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeVotingPlugin.class.getName());
    private NeoForgeRuntime runtime;

    public NeoForgeVotingPlugin() {
        NeoForge.EVENT_BUS.addListener(ServerStartedEvent.class, this::started);
        NeoForge.EVENT_BUS.addListener(ServerStoppingEvent.class, this::stopping);
        NeoForge.EVENT_BUS.addListener(ServerTickEvent.Post.class, this::tick);
        NeoForge.EVENT_BUS.addListener(PlayerEvent.PlayerLoggedInEvent.class, this::joined);
        NeoForge.EVENT_BUS.addListener(PlayerEvent.PlayerLoggedOutEvent.class, this::left);
    }

    private void started(ServerStartedEvent ignored) {
        try {
            runtime = NeoForgeRuntime.start(FMLPaths.CONFIGDIR.get().resolve("votingplugin"), serverFromEvent(ignored));
            LOGGER.info("VotingPlugin NeoForge started; reward replay active"
                    + (runtime.proxySocket().isPresent() ? "; proxy delivery active" : ""));
        } catch (IOException failure) {
            throw new IllegalStateException("VotingPlugin NeoForge startup failed", failure);
        }
    }

    private void stopping(ServerStoppingEvent ignored) {
        if (runtime != null) {
            try {
                runtime.close();
                LOGGER.info("VotingPlugin NeoForge stopped");
            } catch (RuntimeException failure) {
                LOGGER.log(Level.SEVERE, "NeoForge shutdown failed", failure);
            } finally {
                runtime = null;
            }
        }
    }

    private void tick(ServerTickEvent.Post ignored) {
        if (runtime != null) runtime.scheduler().onServerTick();
    }

    private void joined(PlayerEvent.PlayerLoggedInEvent event) {
        if (runtime != null) {
            var identity = runtime.players().joined(NeoForgePlayerDirectory.playerFromEvent(event));
            runtime.proxySocket().ifPresent(proxy -> proxy.playerOnline(identity));
        }
    }

    private void left(PlayerEvent.PlayerLoggedOutEvent event) {
        if (runtime != null) {
            runtime.players().left(NeoForgePlayerDirectory.playerFromEvent(event))
                    .ifPresent(identity -> runtime.proxySocket().ifPresent(proxy -> proxy.playerOffline(identity)));
        }
    }

    private static Object serverFromEvent(Object event) {
        try {
            return event.getClass().getMethod("getServer").invoke(event);
        } catch (ReflectiveOperationException failure) {
            Throwable cause = failure instanceof InvocationTargetException ? failure.getCause() : failure;
            throw new IllegalStateException("Cannot access NeoForge server", cause);
        }
    }
}
