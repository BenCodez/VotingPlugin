package com.bencodez.votingplugin.neoforge;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/** Executes supported reward actions on the NeoForge server tick lane. */
final class NeoForgeNativeRewardActions implements NeoForgeRewardActions {
    private final Object server;
    private final NeoForgeServerScheduler scheduler;
    private final NeoForgePlayerDirectory players;

    NeoForgeNativeRewardActions(Object server, NeoForgeServerScheduler scheduler,
            NeoForgePlayerDirectory players) {
        this.server = Objects.requireNonNull(server, "server");
        this.scheduler = Objects.requireNonNull(scheduler, "scheduler");
        this.players = Objects.requireNonNull(players, "players");
    }

    @Override public CompletableFuture<Void> execute(NeoForgeDeferredVote vote, NeoForgeRewardPlan plan) {
        try {
            return scheduler.executeAsync(() -> {
                try {
                    if (plan.requiresOnline() && players.nativePlayer(vote.playerId()).isEmpty()) {
                        throw new IllegalStateException("Player became unavailable before reward execution");
                    }
                    Object player = plan.actions().stream().anyMatch(action ->
                            action.type() == NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE)
                                    ? players.nativePlayer(vote.playerId()).orElseThrow(
                                            () -> new IllegalStateException(
                                                    "Player became unavailable before reward execution"))
                                    : null;
                    for (NeoForgeRewardPlan.Action action : plan.actions()) {
                        String value = replace(action.value(), vote);
                        if (action.type() == NeoForgeRewardPlan.ActionType.PLAYER_MESSAGE) sendMessage(player, value);
                        else executeCommand(value);
                    }
                } catch (ReflectiveOperationException failure) {
                    throw new IllegalStateException("NeoForge reward action failed", failure);
                }
            });
        } catch (Throwable failure) {
            CompletableFuture<Void> completion = new CompletableFuture<>();
            completion.completeExceptionally(failure);
            return completion;
        }
    }

    private void executeCommand(String command) throws ReflectiveOperationException {
        Object commands = invokeNoArgs(server, "getCommands");
        Object source = invokeNoArgs(server, "createCommandSourceStack");
        Object dispatcher = invokeNoArgs(commands, "getDispatcher");
        Method parse = findCompatibleMethod(dispatcher.getClass(), "parse", String.class, source.getClass());
        Object parsed = invoke(dispatcher, parse, command.startsWith("/") ? command.substring(1) : command, source);
        Object exceptions = invokeNoArgs(parsed, "getExceptions");
        if (exceptions instanceof java.util.Map<?, ?> errors && !errors.isEmpty()) {
            throw new IllegalStateException("NeoForge reward command is invalid");
        }
        Method execute = findMethod(commands.getClass(), "performPrefixedCommand", 2);
        Object result = invokeCommand(commands, execute, source,
                command.startsWith("/") ? command.substring(1) : command);
        if (result instanceof Number number && number.intValue() == 0) {
            throw new UncertainRewardOutcomeException("NeoForge reward command returned an ambiguous zero result");
        }
    }

    private static void sendMessage(Object player, String message) throws ReflectiveOperationException {
        ClassLoader loader = player.getClass().getClassLoader();
        Class<?> componentType = Class.forName("net.minecraft.network.chat.Component", false, loader);
        Object component = componentType.getMethod("literal", String.class).invoke(null, message);
        Method send = findAssignableMethod(player.getClass(), "sendSystemMessage", componentType);
        invoke(player, send, component);
    }

    private static String replace(String configured, NeoForgeDeferredVote vote) {
        return configured.replace("%player%", vote.playerName()).replace("%Player%", vote.playerName())
                .replace("%ServiceSite%", vote.serviceSite()).replace("%servicesite%", vote.serviceSite());
    }

    private static Object invokeNoArgs(Object receiver, String name) throws ReflectiveOperationException {
        return invoke(receiver, receiver.getClass().getMethod(name));
    }

    private static Method findMethod(Class<?> type, String name, int parameters) throws NoSuchMethodException {
        for (Method method : type.getMethods()) {
            if (method.getName().equals(name) && method.getParameterCount() == parameters) return method;
        }
        throw new NoSuchMethodException(type.getName() + "." + name);
    }

    private static Method findCompatibleMethod(Class<?> type, String name, Class<?>... arguments)
            throws NoSuchMethodException {
        for (Method method : type.getMethods()) {
            if (!method.getName().equals(name) || method.getParameterCount() != arguments.length) continue;
            boolean compatible = true;
            for (int index = 0; index < arguments.length; index++) {
                compatible &= method.getParameterTypes()[index].isAssignableFrom(arguments[index]);
            }
            if (compatible) return method;
        }
        throw new NoSuchMethodException(type.getName() + "." + name);
    }

    private static Method findAssignableMethod(Class<?> type, String name, Class<?> argument)
            throws NoSuchMethodException {
        for (Method method : type.getMethods()) {
            if (method.getName().equals(name) && method.getParameterCount() == 1
                    && method.getParameterTypes()[0].isAssignableFrom(argument)) return method;
        }
        throw new NoSuchMethodException(type.getName() + "." + name);
    }

    private static Object invoke(Object receiver, Method method, Object... arguments)
            throws ReflectiveOperationException {
        try {
            return method.invoke(receiver, arguments);
        } catch (InvocationTargetException failure) {
            Throwable cause = failure.getCause();
            if (cause instanceof ReflectiveOperationException reflection) throw reflection;
            if (cause instanceof RuntimeException runtime) throw runtime;
            if (cause instanceof Error error) throw error;
            throw new IllegalStateException("NeoForge reward action failed", cause);
        }
    }

    private static Object invokeCommand(Object receiver, Method method, Object... arguments)
            throws ReflectiveOperationException {
        try {
            return method.invoke(receiver, arguments);
        } catch (InvocationTargetException failure) {
            throw new UncertainRewardOutcomeException(
                    "NeoForge reward command threw after dispatch began", failure.getCause());
        }
    }

    static final class UncertainRewardOutcomeException extends IllegalStateException {
        private static final long serialVersionUID = 1L;
        UncertainRewardOutcomeException(String message) { super(message); }
        UncertainRewardOutcomeException(String message, Throwable cause) { super(message, cause); }
    }
}
