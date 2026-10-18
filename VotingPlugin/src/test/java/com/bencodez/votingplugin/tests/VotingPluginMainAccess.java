package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.*;
import com.bencodez.votingplugin.VotingPluginMain.*;
import java.util.UUID;
import org.bukkit.entity.Player;
import com.bencodez.votingplugin.backendproxy.BackendProxyHandler;

/** Typed test-only access to package-private VotingPluginMain seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginMainAccess {
    private VotingPluginMainAccess() { }
    public static void closePublishedPreviousBackendProxyHandler(VotingPluginMain target, BackendProxyHandler previous, String phase) { ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", target, "closePublishedPreviousBackendProxyHandler", new String[]{"BackendProxyHandler", "String"}, new Object[]{previous, phase}); }
    public static void publishBackendProxyHandler(VotingPluginMain target, BackendProxyHandler previous, BackendProxyHandler replacement) { ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", target, "publishBackendProxyHandler", new String[]{"BackendProxyHandler", "BackendProxyHandler"}, new Object[]{previous, replacement}); }
    public static UUID placeholderStorageUuid(Player player, boolean onlineMode) { return (UUID) ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", null, "placeholderStorageUuid", new String[]{"Player", "boolean"}, new Object[]{player, onlineMode}); }
    public static void reloadBackendProxyRuntime(VotingPluginMain target, boolean updateActiveRuntime, boolean userStorage) { ReflectiveAccess.call("com.bencodez.votingplugin.VotingPluginMain", target, "reloadBackendProxyRuntime", new String[]{"boolean", "boolean"}, new Object[]{updateActiveRuntime, userStorage}); }
}
