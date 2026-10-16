package com.bencodez.votingplugin.streaks;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.votingplugin.VotingPluginMain;

public final class VoteStreaksCommand {
    private VoteStreaksCommand() { }
    public static void register(VotingPluginMain plugin) {
        plugin.getVoteCommand().add(new CommandHandler(plugin, new String[] { "Streaks" },
                VoteStreaksMenu.PERMISSION + "|VotingPlugin.Player", "View your vote streak progression", false) {
            @Override public void execute(CommandSender sender, String[] args) {
                if (sender instanceof Player player && plugin.getVoteStreaksMenu() != null) {
                    plugin.getVoteStreaksMenu().open(player);
                }
            }
        });
    }
}
