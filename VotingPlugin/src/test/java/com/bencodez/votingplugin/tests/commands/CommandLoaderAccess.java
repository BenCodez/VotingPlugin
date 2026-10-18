package com.bencodez.votingplugin.tests.commands;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.commands.*;
import com.bencodez.votingplugin.commands.CommandLoader.*;
import java.util.List;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.PointTransferResult;

/** Typed test-only access to package-private CommandLoader seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class CommandLoaderAccess {
    public static void registerOfflineVoteClearCommands(CommandLoader target, String adminPerm) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "registerOfflineVoteClearCommands", new String[]{"String"}, new Object[]{adminPerm}); }
    public static void runConfirmedBulkStorageMutation(CommandLoader target, CommandSender sender, java.util.function.BooleanSupplier mutation, Runnable success) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "runConfirmedBulkStorageMutation", new String[]{"CommandSender", "BooleanSupplier", "Runnable"}, new Object[]{sender, mutation, success}); }
    private CommandLoaderAccess() { }
    public static void runForCommandSender(CommandLoader target, CommandSender sender, Runnable task) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "runForCommandSender", new String[]{"CommandSender", "Runnable"}, new Object[]{sender, task}); }
    public static void runForVotingUser(CommandLoader target, VotingPluginUser user, Runnable task) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "runForVotingUser", new String[]{"VotingPluginUser", "Runnable"}, new Object[]{user, task}); }

    public static void runBulkStorageMutation(CommandLoader target, CommandSender sender, Runnable mutation, Runnable success) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "runBulkStorageMutation", new String[]{"CommandSender", "Runnable", "Runnable"}, new Object[]{sender, mutation, success}); }

    public static void loadAllVotingUsersAsync(CommandLoader target, CommandSender sender, java.util.function.Consumer<CommandLoaderAccess.BulkVotingUsers> success) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "loadAllVotingUsersAsync", new String[]{"CommandSender", "Consumer"}, new Object[]{sender, (java.util.function.Consumer<Object>) value -> success.accept(BulkVotingUsers.view(value))}); }
    public static String transferFailureMessage(CommandLoader target, PointTransferResult result) { return (String) ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", target, "transferFailureMessage", new String[]{"PointTransferResult"}, new Object[]{result}); }
    public static PlayerCommandHandler configureAllPermissionOverride(PlayerCommandHandler handler, String adminPermission) { return (PlayerCommandHandler) ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", null, "configureAllPermissionOverride", new String[]{"PlayerCommandHandler", "String"}, new Object[]{handler, adminPermission}); }
    public static void registerAdditionalPermissions(PluginManager manager, CommandHandler handler) { ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader", null, "registerAdditionalPermissions", new String[]{"PluginManager", "CommandHandler"}, new Object[]{manager, handler}); }
    public static final class BulkVotingUsers implements ReflectiveAccess.View {

        private final Object delegate;
        private BulkVotingUsers(Object delegate) { this.delegate = delegate; }
        public static BulkVotingUsers view(Object delegate) { return delegate == null ? null : new BulkVotingUsers(delegate); }
        @Override public Object delegate() { return delegate; }
        @Override public boolean equals(Object other) { return other instanceof BulkVotingUsers value && delegate.equals(value.delegate); }
        @Override public int hashCode() { return delegate.hashCode(); }

        public java.util.List<VotingPluginUser> users() { return (java.util.List<VotingPluginUser>) ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader$BulkVotingUsers", delegate, "users", new String[]{}, new Object[]{}); }
        public java.util.IdentityHashMap<VotingPluginUser, Player> players() { return (java.util.IdentityHashMap<VotingPluginUser, Player>) ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader$BulkVotingUsers", delegate, "players", new String[]{}, new Object[]{}); }
        public Player player(VotingPluginUser user) { return (Player) ReflectiveAccess.call("com.bencodez.votingplugin.commands.CommandLoader$BulkVotingUsers", delegate, "player", new String[]{"VotingPluginUser"}, new Object[]{user}); }
    }

}
