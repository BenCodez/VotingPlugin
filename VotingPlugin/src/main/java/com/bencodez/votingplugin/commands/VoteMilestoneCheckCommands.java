package com.bencodez.votingplugin.commands;

import java.util.ArrayList;
import java.util.UUID;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;
import com.bencodez.votingplugin.util.MinecraftUsernameValidator;

/** An explicit local eligibility check; never a synthetic vote. */
final class VoteMilestoneCheckCommands {
    static final String PERMISSION = "VotingPlugin.Commands.AdminVote.CheckMilestones";

    private VoteMilestoneCheckCommands() { }

    static void register(VotingPluginMain plugin, String adminPermission) {
        TabCompleteHandler.getInstance().addTabCompleteOption("(milestone-player)", new ArrayList<>());
        CommandHandler handler = new CommandHandler(plugin,
                new String[] { "User", "(milestone-player)", "CheckMilestones" },
                PERMISSION + "|" + adminPermission, "Check current local vote totals for milestone rewards") {
            // Admit directly in command order instead of racing the SDK's generic async dispatch.
            @Override public boolean runCommand(CommandSender sender, String[] args) {
                if (args.length != 3 || !args[0].equalsIgnoreCase("User")
                        || !args[2].equalsIgnoreCase("CheckMilestones")) return false;
                if (!hasPerm(sender)) { sender.sendMessage("You do not have permission to check milestones."); return true; }
                execute(sender, args.clone());
                return true;
            }

            @Override public void execute(CommandSender sender, String[] args) {
                if (args.length != 3) return;
                String target = args[1];
                boolean uuid = target.matches("[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}");
                if (!uuid && !MinecraftUsernameValidator.isValid(target, plugin.getOptions().getBedrockPlayerPrefix())) {
                    reply(plugin, sender, "Use an exact player name or UUID."); return;
                }
                try {
                    plugin.getUserManager().getDataManager().getTimer().execute(() -> {
                        try {
                            if (!plugin.isEnabled()) return;
                            if (plugin.getBungeeSettings().isUseBungeecoord()) {
                                reply(plugin, sender, "Milestone checks are unavailable in proxy mode; local totals are not authoritative.");
                                return;
                            }
                            var user = uuid ? plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(target), false)
                                    : plugin.getVotingPluginUserManager().getVotingPluginUser(target);
                            plugin.getVoteMilestonesManager().checkCurrentVoteTotals(user);
                            reply(plugin, sender, "Milestone eligibility checked at current totals. This does not confirm reward delivery.");
                        } catch (RuntimeException failure) {
                            plugin.debug(failure);
                            reply(plugin, sender, "Milestone check failed; rewards may have been submitted. Check console before retrying.");
                        }
                    });
                } catch (RuntimeException failure) {
                    plugin.debug(failure);
                    reply(plugin, sender, "Unable to schedule milestone check; no check was performed.");
                }
            }
        };
        handler.setAllowMultiplePermissions(true);
        plugin.getAdminVoteCommand().add(handler);
    }

    private static void reply(VotingPluginMain plugin, CommandSender sender, String message) {
        BukkitCompletionScheduler.run(plugin, sender instanceof Player p ? p : null,
                () -> sender.sendMessage(message),
                sender instanceof Player ? () -> { } : () -> sender.sendMessage(message),
                () -> plugin.getLogger().warning("Milestone check response could not be scheduled."));
    }
}
