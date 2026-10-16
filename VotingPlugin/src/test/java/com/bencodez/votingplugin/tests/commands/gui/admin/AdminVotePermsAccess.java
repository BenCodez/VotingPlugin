package com.bencodez.votingplugin.tests.commands.gui.admin;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.commands.gui.admin.*;
import com.bencodez.votingplugin.commands.gui.admin.AdminVotePerms.*;
import java.util.ArrayList;
import org.bukkit.command.CommandSender;
import com.bencodez.advancedcore.api.command.CommandHandler;

/** Typed test-only access to package-private AdminVotePerms seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class AdminVotePermsAccess {
    private AdminVotePermsAccess() { }
    public static ArrayList<String> additionalPermissions(CommandHandler handle) { return (ArrayList<String>) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.admin.AdminVotePerms", null, "additionalPermissions", new String[]{"CommandHandler"}, new Object[]{handle}); }
    public static boolean hasEffectivePermission(CommandSender sender, String permission) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.commands.gui.admin.AdminVotePerms", null, "hasEffectivePermission", new String[]{"CommandSender", "String"}, new Object[]{sender, permission}); }
}
