package com.bencodez.votingplugin.experimental;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.plugin.Plugin;
import com.bencodez.votingplugin.VotingPluginMain;
import io.github.projectunified.unidialog.core.payload.DialogPayload;

/** Lazy host adapter over already bundled UniDialog; owns its own callbacks and event hook. */
final class NativeExperimentalDialog implements ExperimentalDialogs.Backend {
    private static final String NAMESPACE = "votingplugin_experimental";
    private record Platform(String manager, Class<?> event, boolean paper, Method close) {}
    private final Object manager;
    private final Platform platform;

    private static Platform probe() throws ReflectiveOperationException {
        ClassLoader loader = NativeExperimentalDialog.class.getClassLoader();
        try {
            Class<?> dialog = Class.forName("io.papermc.paper.dialog.Dialog", false, loader);
            Class<?> event = Class.forName("io.papermc.paper.event.player.PlayerCustomClickEvent", false, loader);
            event.getMethod("getIdentifier");
            event.getMethod("getCommonConnection");
            Class<?> dialogLike = Class.forName("net.kyori.adventure.dialog.DialogLike", false, loader);
            Player.class.getMethod("showDialog", dialogLike);
            Method close = Player.class.getMethod("closeDialog");
            Class.forName("io.papermc.paper.registry.data.dialog.ActionButton", false, loader);
            if (!dialogLike.isAssignableFrom(dialog)) throw new NoSuchMethodException("Incompatible Paper dialog API");
            return new Platform("io.github.projectunified.unidialog.paper.PaperDialogManager", event, true, close);
        } catch (ClassNotFoundException | NoSuchMethodException noPaper) {
            Class<?> dialog = Class.forName("net.md_5.bungee.api.dialog.Dialog", false, loader);
            Class<?> event = Class.forName("org.bukkit.event.player.PlayerCustomClickEvent", false, loader);
            event.getMethod("getId");
            event.getMethod("getPlayer");
            Player.class.getMethod("showDialog", dialog);
            return new Platform("io.github.projectunified.unidialog.spigot.SpigotDialogManager", event, false,
                    Player.class.getMethod("clearDialog"));
        }
    }
    static boolean supported() {
        try { probe(); return true; }
        catch (ReflectiveOperationException | LinkageError unavailable) { return false; }
    }
    @SuppressWarnings("unchecked")
    NativeExperimentalDialog(VotingPluginMain plugin) {
        try {
            platform = probe();
            Class<?> type = Class.forName(platform.manager(), true, getClass().getClassLoader());
            manager = type.getConstructor(Plugin.class, String.class).newInstance(plugin, NAMESPACE);
            Method dispatch = type.getMethod("onCustomClick", platform.event());
            // Do not register the library listener: all its HashMap accesses use this private lock.
            plugin.getServer().getPluginManager().registerEvent((Class<? extends Event>) platform.event(),
                    new Listener() {}, EventPriority.NORMAL, (listener, event) -> {
                        if (!platform.event().isInstance(event)) return;
                        synchronized (manager) {
                            try { dispatch.invoke(manager, event); }
                            catch (ReflectiveOperationException failure) {
                                plugin.getLogger().warning("Experimental dialog callback failed: " + failure.getClass().getSimpleName());
                            }
                        }
                    }, plugin, false);
        } catch (ReflectiveOperationException | LinkageError failure) {
            throw new IllegalStateException("Native dialog API unavailable", failure);
        }
    }
    @Override public Runnable show(Player player, String title, String body, List<ExperimentalDialogs.Button> buttons,
            BiConsumer<UUID, String> action) {
        List<String> ids = new ArrayList<>();
        synchronized (manager) {
            try {
                Object dialog = call(manager, "createMultiActionDialog", new Class<?>[0]);
                call(dialog, "title", new Class<?>[] {String.class}, title);
                call(dialog, "canCloseWithEscape", new Class<?>[] {boolean.class}, true);
                call(dialog, "pause", new Class<?>[] {boolean.class}, false);
                call(dialog, "afterAction", new Class<?>[] {io.github.projectunified.unidialog.core.dialog.Dialog.AfterAction.class},
                        io.github.projectunified.unidialog.core.dialog.Dialog.AfterAction.CLOSE);
                call(dialog, "columns", new Class<?>[] {int.class}, 1);
                call(dialog, "body", new Class<?>[] {Consumer.class}, (Consumer<Object>) builder -> {
                    Object text = call(builder, "text", new Class<?>[0]);
                    call(text, "text", new Class<?>[] {String.class}, body);
                });
                for (var button : buttons) {
                    String id = UUID.randomUUID().toString();
                    if (button.action() != null) {
                        Consumer<DialogPayload> callback = payload -> action.accept(payload.owner(), button.action());
                        call(manager, "registerCustomAction", new Class<?>[] {String.class, String.class, Consumer.class},
                                NAMESPACE, id, callback);
                        ids.add(id);
                    }
                    call(dialog, "action", new Class<?>[] {Consumer.class}, (Consumer<Object>) builder -> {
                        call(builder, "label", new Class<?>[] {String.class}, button.label());
                        call(builder, "tooltip", new Class<?>[] {String.class}, button.tooltip());
                        if (button.url() != null) call(builder, "openUrl", new Class<?>[] {String.class}, button.url());
                        else call(builder, "dynamicCustom", new Class<?>[] {String.class, String.class}, NAMESPACE, id);
                    });
                }
                Object opener = call(dialog, "opener", new Class<?>[0]);
                Class<?> audience = platform.paper()
                        ? Class.forName("net.kyori.adventure.audience.Audience", false, getClass().getClassLoader()) : Player.class;
                call(opener, "open", new Class<?>[] {audience}, player);
            } catch (RuntimeException | LinkageError | ClassNotFoundException failure) {
                release(ids);
                throw new IllegalStateException("Native dialog could not open", failure);
            }
        }
        List<String> owned = List.copyOf(ids);
        return () -> { synchronized (manager) { release(owned); } };
    }
    private void release(List<String> ids) {
        for (String id : ids) call(manager, "unregisterCustomAction", new Class<?>[] {String.class, String.class}, NAMESPACE, id);
    }
    @Override public void close(Player player) {
        try { platform.close().invoke(player); }
        catch (ReflectiveOperationException failure) { throw new IllegalStateException("Native dialog could not close", failure); }
    }
    private static Object call(Object target, String name, Class<?>[] types, Object... arguments) {
        try { return target.getClass().getMethod(name, types).invoke(target, arguments); }
        catch (ReflectiveOperationException failure) { throw new IllegalStateException("Native dialog method unavailable: " + name, failure); }
    }
}
