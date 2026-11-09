package com.bencodez.votingplugin.tests.placeholders;

import com.bencodez.votingplugin.placeholders.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import org.bukkit.OfflinePlayer;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.topvoter.*;
import com.bencodez.votingplugin.user.VotingPluginUser;

class TopVoterPositionFreshnessTest {
    @Test
    void everyRankBypassesOldResultCacheAcrossCompletionExpiryAndJoin() throws Exception {
        for (PlaceholderCacheLevel level : PlaceholderCacheLevel.values()) {
        for (String period : List.of("all", "month", "lastmonth", "week", "daily")) {
            var plugin = mock(VotingPluginMain.class); var config = mock(Config.class);
            when(plugin.getConfigFile()).thenReturn(config); when(config.getPlaceholderCacheLevel()).thenReturn(level);
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
            var user = mock(VotingPluginUser.class); var viewer = mock(OfflinePlayer.class);
            UUID id = new UUID(0, 2); when(user.getJavaUUID()).thenReturn(id);
            var placeholders = spy(new PlaceHolders(plugin)); PlaceHoldersAccess.resolvePlaceholderUser(doReturn(user).when(placeholders), viewer);
            var raw = new LinkedHashMap<TopVoterPlayer,Integer>();
            var absent = new TopVoterPlayer(new UUID(0, 1), "SyntheticNever", 1L);
            var target = new TopVoterPlayer(id, "SyntheticJoined", 1L); raw.put(absent,100);raw.put(target,90);
            var clock = new AtomicLong(); var pending = new AtomicReference<>(new CompletableFuture<PlayerPresence>());
            try (var filter = new TopVoterDisplayFilter(row -> row.getUuid().equals(absent.getUuid()) ? pending.get()
                    : CompletableFuture.completedStage(PlayerPresence.JOINED), clock::get)) {
                String key = "top_" + period + "_position";
                var placeholder = new PlaceHolder<VotingPluginUser>(key) {
                    @Override public String placeholderRequest(VotingPluginUser ignored, String identifier) {
                        return Integer.toString(new ArrayList<>(filter.project(raw,true).keySet()).indexOf(target)+1);
                    }
                };
                placeholder.setUseCache(true,key);placeholder.getCache().get(key).put(id,"2");
                placeholders.getPlaceholders().add(placeholder);
                var field = PlaceHolders.class.getDeclaredField("cachedPlaceholders");field.setAccessible(true);
                ((ArrayList<String>)field.get(placeholders)).add(key);
                assertEquals("2",placeholders.getPlaceholderValue(viewer,key,false,false,true));
                pending.get().complete(PlayerPresence.NEVER_JOINED);
                assertEquals("1",placeholder.placeholderRequest(user,key),"Fixture must actually omit the certified row");
                assertEquals("1",placeholders.getPlaceholderValue(viewer,key,false,false,true),period+" completion");
                pending.set(new CompletableFuture<>());clock.set(TimeUnit.SECONDS.toNanos(31));
                assertEquals("2",placeholders.getPlaceholderValue(viewer,key,false,false,true),period+" expired");
                pending.get().complete(PlayerPresence.NEVER_JOINED);
                assertEquals("1",placeholder.placeholderRequest(user,key),"Fixture must actually omit the certified row");
                assertEquals("1",placeholders.getPlaceholderValue(viewer,key,false,false,true));
                pending.set(CompletableFuture.completedFuture(PlayerPresence.JOINED));
                filter.joined(absent.getUuid(),absent.getPlayerName());
                assertEquals("2",placeholders.getPlaceholderValue(viewer,key,false,false,true),period+" joined");
                filter.close();assertEquals("2",placeholders.getPlaceholderValue(viewer,key,false,false,true));
            }
        }
    }

    }

    @Test void disabledFeatureAndUnrelatedPlaceholdersKeepConfiguredResultCaching() throws Exception {
        for (String key : List.of("top_month_position","points")) {
            var plugin=mock(VotingPluginMain.class);var config=mock(Config.class);
            when(plugin.getConfigFile()).thenReturn(config);when(config.getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTO);
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(key.equals("points"));
            var user=mock(VotingPluginUser.class);var viewer=mock(OfflinePlayer.class);var id=new UUID(0,2);
            when(user.getJavaUUID()).thenReturn(id);var placeholders=spy(new PlaceHolders(plugin));
            PlaceHoldersAccess.resolvePlaceholderUser(doReturn(user).when(placeholders), viewer);
            var placeholder=new PlaceHolder<VotingPluginUser>(key) {
                @Override public String placeholderRequest(VotingPluginUser ignored,String identifier) {return "fresh";}
            };
            placeholder.setUseCache(true,key);placeholder.getCache().get(key).put(id,"cached");placeholders.getPlaceholders().add(placeholder);
            var field=PlaceHolders.class.getDeclaredField("cachedPlaceholders");field.setAccessible(true);
            ((ArrayList<String>)field.get(placeholders)).add(key);
            assertEquals("cached",placeholders.getPlaceholderValue(viewer,key,false,false,true));
        }
    }
}
