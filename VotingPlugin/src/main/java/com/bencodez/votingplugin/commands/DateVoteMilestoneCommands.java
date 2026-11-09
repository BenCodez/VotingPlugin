package com.bencodez.votingplugin.commands;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones.AdminAction;
import org.bukkit.command.CommandSender;

/** Exact-target administration; storage and confirmed completion belong to the manager. */
final class DateVoteMilestoneCommands {
    private DateVoteMilestoneCommands() { }

    static void register(VotingPluginMain plugin, String adminPermission) {
        // The SDK only matches registered variable tokens; its built-in player token also rewrites fuzzy names.
        for (String token : new String[] { "(player-id)", "(date-event)", "(vote-count)", "(date-milestone)" })
            TabCompleteHandler.getInstance().addTabCompleteOption(token, new java.util.ArrayList<>());
        for (AdminAction action : AdminAction.values()) {
            String[] arguments = switch (action) {
                case Status, Preview -> new String[] { "DateVoteMilestone&DVM", action.name(), "(player-id)", "(date-event)" };
                case SetVotes -> new String[] { "DateVoteMilestone&DVM", action.name(), "(player-id)", "(date-event)", "(vote-count)" };
                case Force, ForceNoLimits -> new String[] { "DateVoteMilestone&DVM", action.name(), "(player-id)", "(date-event)", "(date-milestone)" };
            };
            String description = switch (action) {
                case Status, Preview -> "Inspect date event counts and reward fences without submitting rewards";
                case SetVotes -> "Set date event count (0-4096), preserving receipt IDs and reward fences";
                case Force -> "Force selected date milestones, preserving prior submission and uncertain reservation fences";
                case ForceNoLimits -> "Repeat submitted date milestones; uncertain reservations remain blocked";
            };
            CommandHandler handler = new CommandHandler(plugin, arguments,
                    "VotingPlugin.Commands.AdminVote.DateVoteMilestone." + action.name() + "|" + adminPermission, description) {
                @Override public void execute(CommandSender sender, String[] args) {
                    var manager = plugin.getDateVoteMilestones();
                    if (manager == null) { sender.sendMessage("Date milestones are unavailable."); return; }
                    manager.administer(sender, action, args[2], args[3], args.length > 4 ? args[4] : null);
                }
            };
            handler.setAllowMultiplePermissions(true);
            plugin.getAdminVoteCommand().add(handler);
        }
    }
}
