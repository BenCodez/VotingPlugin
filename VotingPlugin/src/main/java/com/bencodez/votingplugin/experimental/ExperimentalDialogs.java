package com.bencodez.votingplugin.experimental;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;
import com.bencodez.votingplugin.hologram.HologramVoteModel;

/** Bounded, session-owned native dialogs. No shared production dialog registrations. */
final class ExperimentalDialogs {
    record Button(String label, String tooltip, String url, String action) {
        Button {
            if (label == null || label.length() > 256 || tooltip == null || tooltip.length() > 512)
                throw new IllegalArgumentException("Invalid dialog text");
            if (url != null && HologramVoteModel.votingUrl(url).isEmpty())
                throw new IllegalArgumentException("Invalid voting URL");
            if ((url == null) == (action == null) || action != null && action.length() > 128)
                throw new IllegalArgumentException("Exactly one dialog action is required");
        }
    }
    interface Backend {
        Runnable show(Player player, String title, String body, List<Button> buttons,
                java.util.function.BiConsumer<UUID, String> action);
        void close(Player player);
    }
    private static final class Owned {
        final ExperimentalSessions.Session session;
        final Player player;
        volatile Frame frame;
        Owned(ExperimentalSessions.Session session, Player player) { this.session = session; this.player = player; }
    }
    private static final class Frame {
        final AtomicBoolean selected = new AtomicBoolean();
        volatile Runnable release = () -> {};
    }
    private final VotingPluginMain plugin;
    private final HologramMenuScheduler scheduler;
    private final ExperimentalSessions sessions;
    private final Map<UUID, Owned> resources = new ConcurrentHashMap<>();
    private final Map<UUID, Owned> players = new ConcurrentHashMap<>();
    private Backend backend;

    ExperimentalDialogs(VotingPluginMain plugin, HologramMenuScheduler scheduler, ExperimentalSessions sessions) {
        this.plugin = plugin; this.scheduler = scheduler; this.sessions = sessions;
    }
    ExperimentalDialogs(VotingPluginMain plugin, HologramMenuScheduler scheduler, ExperimentalSessions sessions, Backend backend) {
        this(plugin, scheduler, sessions); this.backend = backend;
    }
    static boolean supported() { return NativeExperimentalDialog.supported(); }

    // Invoked only on the player scheduler after asynchronous user snapshot completion.
    void render(ExperimentalSessions.Session session, Player player, String title, String body,
            List<Button> buttons, Consumer<String> selection) {
        if (!sessions.current(session)) return;
        if (buttons.size() > 8 || title.length() > 256 || body.length() > 4096)
            throw new IllegalArgumentException("Dialog presentation exceeds bounds");
        Owned owned = resources.get(session.id);
        if (owned == null) {
            synchronized (resources) {
                if (resources.size() >= 64) throw new IllegalStateException("Dialog capacity reached");
                owned = resources.get(session.id);
                if (owned == null) {
                    owned = new Owned(session, player);
                    resources.put(session.id, owned);
                    players.put(session.player, owned);
                    Owned captured = owned;
                    session.own(() -> retire(captured), failure -> plugin.getLogger().warning("Experimental dialog cleanup failed: " + failure));
                }
            }
        }
        Owned captured = owned;
        Frame frame = new Frame();
        Frame previous = owned.frame;
        owned.frame = frame;
        if (previous != null) previous.release.run();
        synchronized (this) { if (backend == null) backend = new NativeExperimentalDialog(plugin); }
        try {
            frame.release = backend.show(player, title, body, List.copyOf(buttons), (owner, action) -> {
                if (!session.player.equals(owner) || captured.frame != frame || !sessions.current(session)) return;
                scheduler.player(player, () -> {
                    if (captured.frame != frame || !sessions.current(session) || !plugin.isEnabled()
                            || !player.isOnline() || !(player.hasPermission("VotingPlugin.Admin")
                            || player.hasPermission("VotingPlugin.Commands.AdminVote." + session.type.command()))
                            || !frame.selected.compareAndSet(false, true)) return;
                    try { selection.accept(action); }
                    catch (RuntimeException failure) { sessions.close(session); }
                }, () -> sessions.close(session));
            });
            if (!sessions.current(session) || captured.frame != frame) frame.release.run();
            else session.activate();
        } catch (RuntimeException | LinkageError failure) { sessions.close(session); throw failure; }
    }

    private void retire(Owned owned) {
        resources.remove(owned.session.id, owned);
        Frame frame = owned.frame;
        owned.frame = null;
        if (frame != null) frame.release.run();
        // Supported native APIs cannot identify the currently displayed dialog. An
        // unconditional close here could dismiss a production dialog opened afterward.
        // Native close buttons/Escape dismiss the screen; retirement invalidates callbacks.
        players.remove(owned.session.player, owned);
    }
}
