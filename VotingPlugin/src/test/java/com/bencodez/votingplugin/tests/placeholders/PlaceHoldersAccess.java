package com.bencodez.votingplugin.tests.placeholders;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.placeholders.*;
import com.bencodez.votingplugin.placeholders.PlaceHolders.*;
import org.bukkit.OfflinePlayer;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Typed test-only access to package-private PlaceHolders seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class PlaceHoldersAccess {
    private PlaceHoldersAccess() { }
    public static PlaceHolder<VotingPluginUser> platformOwned(PlaceHolders target, PlaceHolder<VotingPluginUser> placeholder) { return (PlaceHolder<VotingPluginUser>) ReflectiveAccess.call("com.bencodez.votingplugin.placeholders.PlaceHolders", target, "platformOwned", new String[]{"PlaceHolder"}, new Object[]{placeholder}); }
    public static PlaceHolder<VotingPluginUser> platformOwnedWithOfflineWorkerFallback(PlaceHolders target, PlaceHolder<VotingPluginUser> placeholder) { return (PlaceHolder<VotingPluginUser>) ReflectiveAccess.call("com.bencodez.votingplugin.placeholders.PlaceHolders", target, "platformOwnedWithOfflineWorkerFallback", new String[]{"PlaceHolder"}, new Object[]{placeholder}); }
    public static void publishUserDataChangePlaceholders(PlaceHolders target) { ReflectiveAccess.call("com.bencodez.votingplugin.placeholders.PlaceHolders", target, "publishUserDataChangePlaceholders", new String[]{}, new Object[]{}); }
    public static void onUserDataChange(PlaceHolders target, AdvancedCoreUser user, String... keys) { ReflectiveAccess.call("com.bencodez.votingplugin.placeholders.PlaceHolders", target, "onUserDataChange", new String[]{"AdvancedCoreUser", "String[]"}, new Object[]{user, keys}); }
    public static VotingPluginUser resolvePlaceholderUser(PlaceHolders target, OfflinePlayer player) { return (VotingPluginUser) ReflectiveAccess.call("com.bencodez.votingplugin.placeholders.PlaceHolders", target, "resolvePlaceholderUser", new String[]{"OfflinePlayer"}, new Object[]{player}); }
}
