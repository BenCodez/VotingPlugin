package com.bencodez.votingplugin.tests.topvoter;

import com.bencodez.votingplugin.topvoter.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import org.bukkit.Bukkit;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.simpleapi.skull.SkullCache;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.backgroundtask.VotingPluginBackgroundTask;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.user.VotingPluginUser;

class TopVoterDisplayCompatibilityTest {
    @Test void backgroundRefreshPreservesBlacklistBanAndIgnorePermissionBeforeDisplayProjection() {
        VotingPluginMain saved = VotingPluginMain.plugin;
        try {
            for (boolean ignorePermission : new boolean[] {true, false}) {
                var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); VotingPluginMain.plugin = plugin;
                var config = mock(Config.class); when(plugin.getConfigFile()).thenReturn(config);
                when(plugin.isEnabled()).thenReturn(true);
                when(config.getLoadTopVoter(TopVoter.Monthly)).thenReturn(true);
                when(config.isTopVoterIgnorePermission()).thenReturn(ignorePermission);
                when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
                when(config.getBlackList()).thenReturn(new ArrayList<>(List.of("SyntheticBlacklisted")));
                when(plugin.getPlaceholders().getCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTOALL);
                var storage = mock(ScheduledExecutorService.class);
                when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(storage);
                doAnswer(call -> { call.getArgument(0, Runnable.class).run(); return null; }).when(storage).execute(any(Runnable.class));
                doAnswer(call -> { call.getArgument(0, Consumer.class).accept(java.util.Map.of()); return null; })
                        .when(plugin).captureOnlineTopVoterIgnore(any(), any());
                var users = new LinkedHashMap<UUID, VotingPluginUser>();
                String[] names = {"SyntheticNever", "SyntheticJoined", "SyntheticUnknown", "SyntheticIgnored", "SyntheticBlacklisted", "SyntheticBanned"};
                for (int index = 0; index < names.length; index++) {
                    UUID uuid = new UUID(0, index + 1); var user = mock(VotingPluginUser.class);
                    when(user.getPlayerName()).thenReturn(names[index]);
                    when(user.getTopVoterPlayer()).thenReturn(new TopVoterPlayer(uuid, names[index], 1L));
                    when(user.getTotal(TopVoter.Monthly)).thenReturn(100 - index);
                    when(user.getLastVotes()).thenReturn(new HashMap<>());
                    when(user.isTopVoterIgnore()).thenReturn(index == 3);
                    when(user.isBanned()).thenReturn(index == 5);
                    when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);
                    users.put(uuid, user);
                }
                var userManager = plugin.getUserManager();
                doAnswer(call -> {
                    BiConsumer<UUID, ArrayList<Column>> each = call.getArgument(0);
                    Consumer<Integer> finished = call.getArgument(1);
                    users.forEach((uuid, user) -> each.accept(uuid, new ArrayList<>()));
                    finished.accept(users.size()); return null;
                }).when(userManager).forEachUserKeys(any(), any());
                var rankings = new AtomicReference<LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>>>();
                var handler = plugin.getTopVoterHandler();
                doAnswer(call -> { rankings.set(call.getArgument(0)); return null; })
                        .when(handler).updateTopVoters(any());
                try (var bukkit = mockStatic(Bukkit.class); var skulls = mockStatic(SkullCache.class)) {
                    var task = new VotingPluginBackgroundTask(plugin); task.setRequested(true); task.run();
                    assertFalse(task.isRunning());
                }
                assertNotNull(rankings.get(), "Actual refresh must finish and publish rankings");
                var raw = rankings.get().get(TopVoter.Monthly);
                assertEquals(ignorePermission ? 3 : 4, raw.size());
                List<String> rawNames = raw.keySet().stream().map(TopVoterPlayer::getPlayerName).toList();
                assertFalse(rawNames.contains("SyntheticBlacklisted")); assertFalse(rawNames.contains("SyntheticBanned"));
                assertEquals(!ignorePermission, rawNames.contains("SyntheticIgnored"));
                assertTrue(rawNames.contains("SyntheticNever"));
                try (var display = new TopVoterDisplayFilter(row -> CompletableFuture.completedStage(
                        row.getPlayerName().equals("SyntheticNever") ? PlayerPresence.NEVER_JOINED
                        : row.getPlayerName().equals("SyntheticUnknown") ? PlayerPresence.UNKNOWN : PlayerPresence.JOINED))) {
                    var visible = display.project(raw, true);
                    assertEquals(raw.size() - 1, visible.size());
                    assertTrue(visible.keySet().stream().anyMatch(row -> row.getPlayerName().equals("SyntheticUnknown")));
                    assertEquals(100, raw.values().iterator().next());
                }
                users.values().forEach(user -> verify(user).clearTempCache());
                verify(plugin, never()).checkPlayerPresenceIdentitiesAsync(anyCollection());
            }
        } finally { VotingPluginMain.plugin = saved; }
    }

    @Test void hiddenDisplayWinnerStillReceivesItsOriginalDailyAward() {
        VotingPluginMain saved = VotingPluginMain.plugin;
        try {
            var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); VotingPluginMain.plugin = plugin;
            when(plugin.getConfigFile().isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
            when(plugin.getSpecialRewardsConfig().isEnableDailyRewards()).thenReturn(true);
            when(plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces()).thenReturn(java.util.Set.of("1"));
            when(plugin.getShopFile().getShopIdentifiers()).thenReturn(java.util.Set.of());
            var user = mock(VotingPluginUser.class);
            var winner = new TopVoterPlayer(new UUID(0, 1), "SyntheticNever", 1L);
            when(plugin.getVotingPluginUserManager().getVotingPluginUser(winner.getUuid(), winner.getPlayerName())).thenReturn(user);
            var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(winner, 100);
            when(plugin.getTopVoter(TopVoter.Daily)).thenReturn(raw);
            try (var display = new TopVoterDisplayFilter(row -> CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED))) {
                assertTrue(display.project(raw, true).isEmpty());
                var handler = spy(new TopVoterHandler(plugin)); doNothing().when(handler).resetTotals(TopVoter.Daily);
                handler.onDayChange(mock(DayChangeEvent.class));
                verify(user).giveDailyTopVoterAward(1, "1");
                verify(plugin, never()).getTopVoterDisplay(any());
                assertEquals(100, raw.get(winner));
            }
        } finally { VotingPluginMain.plugin = saved; }
    }

    @Test void replacingAndFailedReplacementCannotReviveOldNegativeCompletion() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        var generationField = VotingPluginMain.class.getDeclaredField("topVoterDisplayGeneration");
        generationField.setAccessible(true); generationField.set(plugin, new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled(); var config = mock(Config.class);
        when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
        var options = mock(AdvancedCoreConfigOptions.class); when(options.isOnlineMode()).thenReturn(true);
        doReturn(options).when(plugin).getOptions(); doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        doReturn(CompletableFuture.completedStage(PlayerPresence.JOINED)).when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(), eq(true));
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(new TopVoterPlayer(UUID.fromString("d4519fc4-a782-4d11-8235-617209238441"), "Synthetic", 1L), 100);
        var state = new TopVoterState(); state.getTopVoters().put(TopVoter.Monthly, raw);
        var pending = new CompletableFuture<PlayerPresence>(); var predecessor = new TopVoterDisplayFilter(row -> pending);
        for (var entry : java.util.Map.of("configFile", config, "topVoterState", state, "topVoterDisplayFilter", predecessor).entrySet()) {
            var field = VotingPluginMain.class.getDeclaredField(entry.getKey()); field.setAccessible(true); field.set(plugin, entry.getValue());
        }
        assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
        var cache = mock(UuidLookup.class); when(cache.getCachedUUID(anyString())).thenReturn("");
        try (var lookup = mockStatic(UuidLookup.class)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);
            plugin.initializeTopVoterDisplayFilter(); pending.complete(PlayerPresence.NEVER_JOINED);
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            doThrow(new IllegalStateException("synthetic registration failure")).when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
            assertThrows(IllegalStateException.class, plugin::initializeTopVoterDisplayFilter);
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            assertSame(raw, plugin.getTopVoter(TopVoter.Monthly));
        }
    }
}
