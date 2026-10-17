package com.bencodez.votingplugin.session;

import java.util.List;
import java.util.UUID;
import java.util.function.BiConsumer;
import org.bukkit.entity.Player;

/** A player-owned native view; cleanup releases only this view's action handlers. */
interface SessionDialogView {
    record Button(String label, String tooltip, String action, String url) { }
    Runnable show(Player player, String title, String body, List<Button> buttons,
            BiConsumer<UUID, String> action);
}
