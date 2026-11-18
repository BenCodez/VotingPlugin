package com.bencodez.votingplugin.fabric;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.RejectedExecutionException;

import net.fabricmc.api.DedicatedServerModInitializer;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.fabricmc.loader.api.FabricLoader;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerPlayer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Native entry point for the dedicated-server bootstrap. */
public final class FabricVotingPlugin implements DedicatedServerModInitializer {
    private static final Logger LOGGER = LoggerFactory.getLogger("VotingPlugin");
    private volatile FabricServerRuntime runtime;

    @Override
    public void onInitializeServer() {
        FabricConfiguration configuration;
        try {
            configuration = FabricConfiguration.load(FabricLoader.getInstance().getConfigDir());
        } catch (IOException failure) {
            throw new UncheckedIOException("VotingPlugin Fabric configuration initialization failed", failure);
        }

        ServerLifecycleEvents.SERVER_STARTED.register(server -> {
            runtime = new FabricServerRuntime(server::isSameThread, uuid -> findPlayer(server, uuid));
            if (configuration.logLifecycle()) {
                LOGGER.info("VotingPlugin Fabric started (bootstrap only; voting, persistence, commands, proxy delivery and rewards unavailable)");
            }
        });
        ServerTickEvents.END_SERVER_TICK.register(server -> {
            FabricServerRuntime active = runtime;
            if (active != null) {
                active.tick();
            }
        });
        ServerLifecycleEvents.SERVER_STOPPING.register(server -> {
            FabricServerRuntime active = runtime;
            runtime = null;
            if (active != null) {
                active.close();
                if (configuration.logLifecycle()) {
                    LOGGER.info("VotingPlugin Fabric stopped");
                }
            }
        });
    }

    CompletableFuture<Optional<FabricServerRuntime.PlayerIdentity>> onlinePlayer(UUID uuid) {
        FabricServerRuntime active = runtime;
        if (active == null) {
            return CompletableFuture.failedFuture(new RejectedExecutionException("Fabric server runtime is inactive"));
        }
        return active.onlinePlayer(uuid);
    }

    private static Optional<FabricServerRuntime.PlayerIdentity> findPlayer(MinecraftServer server, UUID uuid) {
        ServerPlayer player = server.getPlayerList().getPlayer(uuid);
        return Optional.ofNullable(player)
                .map(online -> new FabricServerRuntime.PlayerIdentity(online.getUUID(), online.getGameProfile().getName()));
    }
}
