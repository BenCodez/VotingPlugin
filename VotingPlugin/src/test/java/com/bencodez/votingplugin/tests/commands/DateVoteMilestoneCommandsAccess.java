package com.bencodez.votingplugin.tests.commands;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

/** Calls the actual package-private command registrar. */
public final class DateVoteMilestoneCommandsAccess {
    private DateVoteMilestoneCommandsAccess() { }
    public static void register(VotingPluginMain plugin, String adminPermission) {
        ReflectiveAccess.call("com.bencodez.votingplugin.commands.DateVoteMilestoneCommands", null,
                "register", new String[]{"VotingPluginMain", "String"}, new Object[]{plugin, adminPermission});
    }
}
