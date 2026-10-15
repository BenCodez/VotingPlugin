package com.bencodez.votingplugin.npc;

import java.util.Arrays;
import java.util.UUID;
import java.util.concurrent.CompletionStage;

import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Normal /av command handlers; no separate command map or player GUI registry. */
public final class VotingNPCCommands {
    private static final String PERMISSION = "VotingPlugin.Commands.AdminVote.NPC";
    private VotingNPCCommands() { }

    public static void register(VotingPluginMain plugin) {
        add(plugin, new String[] { "NPC", "Create", "(string)" }, false);
        add(plugin, new String[] { "NPC", "Create", "(string)", "(list)" }, false);
        add(plugin, new String[] { "NPC", "List" }, true);
        add(plugin, new String[] { "NPC", "Inspect", "(npcid)" }, true);
        add(plugin, new String[] { "NPC", "Remove", "(npcid)" }, true);
    }

    private static void add(VotingPluginMain plugin, String[] pattern, boolean console) {
        plugin.getAdminVoteCommand().add(new CommandHandler(plugin, pattern, PERMISSION + "|VotingPlugin.Admin",
                "Manage saved vanilla voting NPCs", console) {
            @Override public void execute(CommandSender sender, String[] args) {
                // CommandHandler invokes execute asynchronously. Capture location,
                // permission and messages only after reaching the sender's owner.
                String[] captured = args.clone();
                onSender(plugin, sender, () -> executeOwned(plugin, sender, captured));
            }
        });
    }

    private static boolean allowed(CommandSender sender) {
        return (!(sender instanceof Player player) || player.isOnline())
                && (sender.hasPermission(PERMISSION) || sender.hasPermission("VotingPlugin.Admin"));
    }

    private static void onSender(VotingPluginMain plugin, CommandSender sender, Runnable action) {
        Player player = sender instanceof Player candidate ? candidate : null;
        BukkitCompletionScheduler.run(plugin, player, action, player == null ? action : () -> { }, () -> { });
    }

    private static void executeOwned(VotingPluginMain plugin, CommandSender sender, String[] args) {
        if (!allowed(sender)) return;
        VotingNPCManager manager = plugin.getVotingNPCManager();
        if (manager == null || !manager.available()) {
            sender.sendMessage("Voting NPCs are disabled, unsupported, or waiting for valid storage. Check VotingNPCs.Enabled in Config.yml and reload.");
            return;
        }
        switch (args[1].toLowerCase(java.util.Locale.ROOT)) {
        case "create":
            if (!(sender instanceof Player player)) { sender.sendMessage("Create a voting NPC from in game."); return; }
            try {
                Location at = player.getLocation();
                String name = args.length > 3 ? String.join(" ", Arrays.copyOfRange(args, 3, args.length)) : "&aVote";
                VotingNPCDefinition definition = new VotingNPCDefinition(args[2], UUID.randomUUID(), at.getWorld().getUID(),
                        at.getX(), at.getY(), at.getZ(), at.getYaw(), at.getPitch(), name);
                confirm(plugin, sender, manager.create(definition),
                        ignored -> "Saved voting NPC '" + definition.id() + "'. Its villager appears when the area is loaded and available.");
            } catch (IllegalArgumentException invalid) {
                sender.sendMessage("Use a lowercase ID of 1-40 letters, digits, underscores or hyphens, and a name of 1-128 characters without controls.");
            }
            break;
        case "remove":
            confirm(plugin, sender, manager.remove(args[2]), removed -> removed
                    ? "Removed saved voting NPC '" + args[2] + "'; exact entity cleanup has been requested."
                    : "No voting NPC has that ID.");
            break;
        case "inspect":
            manager.definitions().stream().filter(value -> value.id().equals(args[2])).findFirst().ifPresentOrElse(value ->
                    sender.sendMessage("Voting NPC " + value.id() + ": world " + value.world() + ", "
                            + value.x() + ", " + value.y() + ", " + value.z() + "; " + manager.runtimeState(value.id())),
                    () -> sender.sendMessage("No voting NPC has that ID."));
            break;
        case "list":
            sender.sendMessage("Saved voting NPCs: " + manager.definitions().size() + "/64; tracked runtime slots: " + manager.runtimeCount());
            for (var value : manager.definitions()) sender.sendMessage(value.id() + ": " + manager.runtimeState(value.id()));
            break;
        default:
            sender.sendMessage("Use /av npc create <id> [name], list, inspect <id>, or remove <id>.");
        }
    }

    private static <T> void confirm(VotingPluginMain plugin, CommandSender sender, CompletionStage<T> completion,
            java.util.function.Function<T, String> message) {
        completion.whenComplete((value, failure) -> onSender(plugin, sender, () -> {
            if (!allowed(sender)) return;
            sender.sendMessage(failure == null ? message.apply(value)
                    : "Voting NPC change was not confirmed. Check storage and reload before retrying; no success is assumed.");
        }));
    }
}
