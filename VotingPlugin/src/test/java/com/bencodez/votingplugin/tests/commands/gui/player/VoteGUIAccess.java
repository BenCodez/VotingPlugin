package com.bencodez.votingplugin.tests.commands.gui.player;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.commands.gui.player.*;
import com.bencodez.votingplugin.commands.gui.player.VoteGUI.*;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Typed test-only access to package-private VoteGUI seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VoteGUIAccess {
    private VoteGUIAccess() { }
    public static String getRequiredPermission(VotingPluginMain plugin, Player player, VotingPluginUser viewedUser) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteGUI", null, "getRequiredPermission", new String[]{"VotingPluginMain", "Player", "VotingPluginUser"}, new Object[]{plugin, player, viewedUser}); }
    public static boolean isAuthorized(VotingPluginMain plugin, Player player, VotingPluginUser viewedUser) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.player.VoteGUI", null, "isAuthorized", new String[]{"VotingPluginMain", "Player", "VotingPluginUser"}, new Object[]{plugin, player, viewedUser}); }
}
