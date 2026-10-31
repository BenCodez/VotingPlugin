package com.bencodez.votingplugin.tests.commands;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.placeholders.PlaceHolders;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.placeholders.PlaceholderPlayerPresence;
import com.bencodez.votingplugin.user.VotingPluginUser;

class RejectedPointDebitPlaceholderTest {
    @Test void rejectedDebitRetiresStalePointPlaceholderBeforeCallback() throws Exception { debitOutcomeAndRefillRecovery(false, false); }
    @Test void confirmedDebitRecoversAfterRefillFailure() throws Exception { debitOutcomeAndRefillRecovery(true, true); }
    @Test void rejectedDebitRecoversAfterRefillFailure() throws Exception { debitOutcomeAndRefillRecovery(false, true); }

    private void debitOutcomeAndRefillRecovery(boolean confirmed, boolean failInitialRefill) throws Exception {
        var uuid = UUID.fromString("00000000-0000-0000-0000-000000000001");
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        var timer = mock(ScheduledExecutorService.class);
        var pending = new java.util.ArrayDeque<Runnable>();
        doAnswer(call -> { pending.addLast(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
        var user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        var field = VotingPluginUser.class.getDeclaredField("plugin");
        field.setAccessible(true);
        field.set(user, plugin);
        doReturn(uuid.toString()).when(user).getUUID();
        doReturn(uuid).when(user).getJavaUUID();
        doReturn("Points").when(user).getPointsPath();
        doReturn(false).when(user).isCached();
        doNothing().when(user).cache();
        doReturn(mock(Player.class)).when(user).getPlayer();
        doReturn(4).when(user).getPoints();
        when(plugin.getStorageType()).thenReturn(UserStorage.MYSQL);
        when(plugin.getTimer()).thenReturn(timer);
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTOALL);
        when(plugin.getConfigFile().getCachedPlaceholders()).thenReturn(new ArrayList<>(List.of("points")));
        when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>());
        when(plugin.getPlaceholderPlayerPresence()).thenReturn(new PlaceholderPlayerPresence());
        var offline = mock(OfflinePlayer.class);
        when(offline.getUniqueId()).thenReturn(uuid);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(offline)).thenReturn(user);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("TestUser")).thenReturn(user);
        var cache = new UserDataCache(null, uuid);
        cache.updateCache(new HashMap<>(Map.of("Points", new DataValueInt(20))));
        var manager = plugin.getUserManager().getDataManager();
        when(manager.getUserDataCache()).thenReturn(new ConcurrentHashMap<>(Map.of(uuid, cache)));
        var refillFails = new java.util.concurrent.atomic.AtomicBoolean(failInitialRefill);
        doAnswer(call -> {
            if (refillFails.get()) throw new IllegalStateException("synthetic refill unavailable");
            cache.updateSharedSnapshot(new HashMap<>(Map.of("Points", new DataValueInt(4))), cache.getSharedSnapshotVersion());
            return null;
        }).when(manager).cacheUser(uuid, null);
        var table = mock(MySQL.class);
        var sql = mock(com.bencodez.simpleapi.sql.mysql.MySQL.class, RETURNS_DEEP_STUBS);
        var connection = mock(Connection.class);
        var statement = mock(PreparedStatement.class);
        when(statement.executeUpdate()).thenReturn(confirmed ? 1 : 0);
        var read = mock(PreparedStatement.class);
        var total = mock(ResultSet.class);
        when(total.next()).thenReturn(true);
        when(total.getInt(1)).thenReturn(4);
        when(read.executeQuery()).thenReturn(total);
        var lookup = mock(PreparedStatement.class);
        when(lookup.executeQuery()).thenReturn(mock(ResultSet.class));
        when(plugin.getMysql()).thenReturn(table);
        when(table.getTableName()).thenReturn("VotingPlugin_Users");
        when(table.qi(anyString())).thenAnswer(call -> "`" + call.getArgument(0) + "`");
        when(table.getMysql()).thenReturn(sql);
        when(sql.getConnectionManager().getConnection()).thenReturn(connection);
        when(connection.prepareStatement(anyString())).thenAnswer(call ->
                call.<String>getArgument(0).startsWith("SELECT `player_uuid`") ? lookup
                        : call.<String>getArgument(0).startsWith("SELECT `Points` FROM") ? read : statement);
        var commands = new ArrayList<CommandHandler>();
        when(plugin.getAdminVoteCommand()).thenReturn(commands);
        var oldInstance = AdvancedCorePlugin.getInstance();
        var oldPlugin = VotingPluginMain.plugin;
        AdvancedCorePlugin.setInstance(plugin);
        VotingPluginMain.plugin = plugin;
        try (var coreCommands = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            var helper = mock(com.bencodez.advancedcore.command.CommandLoader.class);
            coreCommands.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(helper);
            when(helper.getBasicAdminCommands("VotingPlugin")).thenReturn(new ArrayList<>());
            var placeholders = new PlaceHolders(plugin);
            when(plugin.getPlaceholders()).thenReturn(placeholders);
            placeholders.load();
            var points = placeholders.getPlaceholders().stream().filter(p -> p.matches("points")).findFirst().orElseThrow();
            points.getCache().get("points").put(uuid, "20");
            try (var bukkit = mockStatic(Bukkit.class)) {
                bukkit.when(Bukkit::isPrimaryThread).thenReturn(true);
                assertEquals("20", placeholders.getPlaceHolder(offline, "points", false));
                var register = CommandLoader.class.getDeclaredMethod("loadAdminVoteCommand");
                register.setAccessible(true);
                register.invoke(new CommandLoader(plugin));
                var command = (PlayerCommandHandler) commands.stream()
                        .filter(c -> Arrays.equals(c.getArgs(), new String[] {"User", "(player)", "RemovePoints", "(number)"}))
                        .findFirst().orElseThrow();
                command.executeSinglePlayer(mock(CommandSender.class), new String[] {"User", "TestUser", "RemovePoints", "16"});
                assertEquals("...", placeholders.getPlaceHolder(offline, "points", false));
                verify(user, never()).cache();
                verify(sql.getConnectionManager(), never()).getConnection();
                assertEquals(1, pending.size());
                pending.removeFirst().run();
                if (failInitialRefill) {
                    assertNull(cache.snapshotIfPublished());
                    assertEquals("...", placeholders.getPlaceHolder(offline, "points_nocache", false));
                    assertEquals(1, pending.size(), "normal request must admit recovery once");
                    refillFails.set(false);
                    pending.removeFirst().run();
                } else {
                    assertEquals("...", placeholders.getPlaceHolder(offline, "points", false));
                    assertEquals(1, pending.size());
                    pending.removeFirst().run();
                }
                assertEquals(4, cache.snapshotIfPublished().get("Points").getInt());
                assertEquals("4", placeholders.getPlaceHolder(offline, "points", false));
                assertTrue(pending.isEmpty());
            }
        } finally {
            AdvancedCorePlugin.setInstance(oldInstance);
            VotingPluginMain.plugin = oldPlugin;
        }
    }
}
