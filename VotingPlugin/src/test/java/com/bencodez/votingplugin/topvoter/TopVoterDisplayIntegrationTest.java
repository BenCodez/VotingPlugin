package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.lang.reflect.Field;
import java.time.YearMonth;
import java.util.LinkedHashMap;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.advancedcore.api.player.UuidLookup;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;

class TopVoterDisplayIntegrationTest {
    @org.junit.jupiter.api.io.TempDir java.nio.file.Path historyDirectory;
    private static final UUID NEVER = UUID.fromString("d4519fc4-a782-4d11-8235-617209238441");
    private static final UUID JOINED = UUID.fromString("b3f2bd99-5325-4a51-a642-12674e147381");

    @Test void shippedAndAbsentConfigurationKeepFilteringDisabled() throws Exception {
        try (var input = getClass().getClassLoader().getResourceAsStream("Config.yml")) {
            assertNotNull(input);
            var yaml = org.bukkit.configuration.file.YamlConfiguration.loadConfiguration(
                    new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8));
            assertFalse(yaml.getBoolean("HideTopVoterIfNotPlayedBefore"));
        }
        Config absent = mock(Config.class, CALLS_REAL_METHODS);
        assertFalse(absent.isHideTopVoterIfNotPlayedBefore());
    }

    private void field(Object target, String name, Object value) throws Exception {
        Field field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(target, value);
    }

    @Test void realPluginBudgetsEvidenceForCurrentLastAndPreviousMonthRankings() throws Exception {
        var plugin=mock(VotingPluginMain.class,CALLS_REAL_METHODS);
        field(plugin,"topVoterDisplayGeneration",new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled();var config=mock(Config.class);
        when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);field(plugin,"configFile",config);
        var options=mock(AdvancedCoreConfigOptions.class);when(options.isOnlineMode()).thenReturn(true);
        doReturn(options).when(plugin).getOptions();doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        var state=new TopVoterState();field(plugin,"topVoterState",state);
        var current=state.getTopVoters(TopVoter.Monthly);var last=state.getLastMonthTopVoters();
        var previous=new LinkedHashMap<TopVoterPlayer,Integer>();var month=YearMonth.of(2024,1);
        state.getPreviousMonthsTopVoters().put(month,previous);
        for(int id=0;id<3000;id++) {
            current.put(new TopVoterPlayer(new UUID(0x4000L,Long.MIN_VALUE|id),"Current"+id,1L),id);
            last.put(new TopVoterPlayer(new UUID(0x4000L,Long.MIN_VALUE|(id+3000)),"Last"+id,1L),id);
            previous.put(new TopVoterPlayer(new UUID(0x4000L,Long.MIN_VALUE|(id+6000)),"Previous"+id,1L),id);
        }
        var cache=mock(UuidLookup.class);when(cache.getCachedUUID(anyString())).thenReturn("");
        var calls=new java.util.concurrent.atomic.AtomicInteger();
        doAnswer(call->{java.util.Collection<UUID> aliases=call.getArgument(0);assertFalse(aliases.contains(null));
            calls.incrementAndGet();return CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED);})
                .when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(),eq(true));
        try(var lookup=mockStatic(UuidLookup.class)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);plugin.initializeTopVoterDisplayFilter();
            for(int round=0;round<3;round++) {
                assertTrue(plugin.getTopVoterDisplay(TopVoter.Monthly).isEmpty());
                assertTrue(plugin.getLastMonthTopVoterDisplay().isEmpty());
                assertTrue(plugin.getPreviousMonthTopVoterDisplay(month).isEmpty());
            }
            assertEquals(9000,calls.get());assertEquals(3000,current.size());
            assertEquals(3000,last.size());assertEquals(3000,previous.size());
        }
    }

    private byte[] nativeHistory(UUID identity) throws Exception {
        var raw = new java.io.ByteArrayOutputStream();
        try (var data = new java.io.DataOutputStream(raw)) {
            data.writeByte(10); data.writeUTF(""); data.writeByte(3); data.writeUTF("DataVersion"); data.writeInt(1);
            data.writeByte(11); data.writeUTF("UUID"); data.writeInt(4);
            data.writeInt((int) (identity.getMostSignificantBits() >>> 32)); data.writeInt((int) identity.getMostSignificantBits());
            data.writeInt((int) (identity.getLeastSignificantBits() >>> 32)); data.writeInt((int) identity.getLeastSignificantBits());
            data.writeByte(10); data.writeUTF("bukkit"); data.writeByte(8); data.writeUTF("lastKnownName");
            data.writeUTF("FormerSynthetic\r\n歴史"); data.writeByte(0); data.writeByte(0);
        }
        var compressed = new java.io.ByteArrayOutputStream();
        try (var gzip = new java.util.zip.GZIPOutputStream(compressed)) { gzip.write(raw.toByteArray()); }
        return compressed.toByteArray();
    }

    @Test void actualNativeMigrationHistoryPreventsHidingRenamedOnlineRowButOrdinaryOnlineAbsenceWorks() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        field(plugin, "topVoterDisplayGeneration", new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled(); var config = mock(Config.class);
        when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true); field(plugin, "configFile", config);
        var options = mock(AdvancedCoreConfigOptions.class); when(options.isOnlineMode()).thenReturn(true);
        doReturn(options).when(plugin).getOptions(); doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        var state = new TopVoterState(); field(plugin, "topVoterState", state);
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(new TopVoterPlayer(NEVER, "RenamedSynthetic", 1L), 100);
        state.getTopVoters().put(TopVoter.Monthly, raw);
        var cache = mock(UuidLookup.class); when(cache.getCachedUUID(anyString())).thenReturn(NEVER.toString());
        UUID former = UUID.nameUUIDFromBytes("OfflinePlayer:FormerSynthetic".getBytes(java.nio.charset.StandardCharsets.UTF_8));
        byte[] retained = nativeHistory(former);
        var tasks = new java.util.ArrayDeque<Runnable>();
        try (var lookup = mockStatic(UuidLookup.class);
                var nativeLookup = new com.bencodez.advancedcore.api.user.presence.BukkitPlayerPresenceLookup(historyDirectory);
                var service = new com.bencodez.advancedcore.api.user.presence.PlayerPresenceService(tasks::add, nativeLookup)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);
            doAnswer(call -> service.checkIdentitiesAsync(call.getArgument(0), true))
                    .when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(), eq(true));
            plugin.initializeTopVoterDisplayFilter();
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            while (!tasks.isEmpty()) tasks.remove().run();
            assertTrue(plugin.getTopVoterDisplay(TopVoter.Monthly).isEmpty(), "Online-only retained scope permits checked negatives");
            java.nio.file.Path oldRecord = java.nio.file.Files.write(historyDirectory.resolve(former + ".dat"), retained);
            plugin.initializeTopVoterDisplayFilter();
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            while (!tasks.isEmpty()) tasks.remove().run();
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly), "Current name cache cannot certify former offline names");
            assertSame(raw, plugin.getTopVoter(TopVoter.Monthly));
            assertArrayEquals(retained, java.nio.file.Files.readAllBytes(oldRecord));
        }
    }

    @Test void offlineExactNameNegativesRemainSupportedButKnownOnlineMigrationRequiresCompleteHistory() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        field(plugin, "topVoterDisplayGeneration", new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled(); var config = mock(Config.class);
        when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true); field(plugin, "configFile", config);
        var options = mock(AdvancedCoreConfigOptions.class); when(options.isOnlineMode()).thenReturn(false);
        doReturn(options).when(plugin).getOptions(); doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        var state = new TopVoterState(); field(plugin, "topVoterState", state);
        UUID offline = UUID.nameUUIDFromBytes("OfflinePlayer:RenamedSynthetic".getBytes(java.nio.charset.StandardCharsets.UTF_8));
        UUID former = UUID.nameUUIDFromBytes("OfflinePlayer:FormerSynthetic".getBytes(java.nio.charset.StandardCharsets.UTF_8));
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(new TopVoterPlayer(offline, "RenamedSynthetic", 1L), 100);
        state.getTopVoters().put(TopVoter.Monthly, raw);
        var cache = mock(UuidLookup.class); when(cache.getCachedUUID(anyString())).thenReturn("");
        byte[] retained = nativeHistory(former);
        var oldRecord = java.nio.file.Files.write(historyDirectory.resolve(former + ".dat"), retained);
        var tasks = new java.util.ArrayDeque<Runnable>();
        try (var lookup = mockStatic(UuidLookup.class);
                var nativeLookup = new com.bencodez.advancedcore.api.user.presence.BukkitPlayerPresenceLookup(historyDirectory);
                var service = new com.bencodez.advancedcore.api.user.presence.PlayerPresenceService(tasks::add, nativeLookup)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);
            doAnswer(call -> service.checkIdentitiesAsync(call.getArgument(0)))
                    .when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection());
            doAnswer(call -> service.checkIdentitiesAsync(call.getArgument(0), true))
                    .when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(), eq(true));
            plugin.initializeTopVoterDisplayFilter(); plugin.getTopVoterDisplay(TopVoter.Monthly);
            while (!tasks.isEmpty()) tasks.remove().run();
            assertTrue(plugin.getTopVoterDisplay(TopVoter.Monthly).isEmpty(), "Pure offline exact-name absence remains checkable");
            when(cache.getCachedUUID(anyString())).thenReturn(NEVER.toString());
            plugin.initializeTopVoterDisplayFilter(); plugin.getTopVoterDisplay(TopVoter.Monthly);
            while (!tasks.isEmpty()) tasks.remove().run();
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly), "Known online migration must account for former offline names");
            assertSame(raw, plugin.getTopVoter(TopVoter.Monthly));
            assertArrayEquals(retained, java.nio.file.Files.readAllBytes(oldRecord));
        }
    }

    @Test void olderSdkFallbackPreservesPositiveButNeverCertifiesWiderOnlineAbsence() throws Exception {
        var plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        field(plugin, "topVoterDisplayGeneration", new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled(); var config = mock(Config.class);
        when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true); field(plugin, "configFile", config);
        var options = mock(AdvancedCoreConfigOptions.class); when(options.isOnlineMode()).thenReturn(true);
        doReturn(options).when(plugin).getOptions(); doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        var state = new TopVoterState(); field(plugin, "topVoterState", state);
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(new TopVoterPlayer(NEVER, "Synthetic", 1L), 100);
        state.getTopVoters().put(TopVoter.Monthly, raw);
        doThrow(new NoSuchMethodError("synthetic legacy SDK")).when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(), eq(true));
        var cache = mock(UuidLookup.class); when(cache.getCachedUUID(anyString())).thenReturn(NEVER.toString());
        try (var lookup = mockStatic(UuidLookup.class)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);
            for (PlayerPresence legacy : new PlayerPresence[] {PlayerPresence.NEVER_JOINED, PlayerPresence.JOINED}) {
                doReturn(CompletableFuture.completedStage(legacy)).when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection());
                plugin.initializeTopVoterDisplayFilter(); assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            }
            verify(plugin, times(2)).checkPlayerPresenceIdentitiesAsync(anyCollection());
        }
    }

    @Test void runtimeFactoryAndAllDisplayAccessorsKeepAwardMapsUnchanged() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        field(plugin, "topVoterDisplayGeneration", new java.util.concurrent.atomic.AtomicLong());
        doReturn(true).when(plugin).isEnabled();
        Config config = mock(Config.class); when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
        field(plugin, "configFile", config);
        var options = mock(AdvancedCoreConfigOptions.class); when(options.isOnlineMode()).thenReturn(true);
        doReturn(options).when(plugin).getOptions();
        doNothing().when(plugin).registerEvents(any(TopVoterDisplayFilter.class));
        doAnswer(call -> {
            java.util.Collection<UUID> ids = call.getArgument(0);
            assertFalse(ids.contains(null), "Captured online mode must remain coherent until replacement");
            return CompletableFuture.completedStage(ids.iterator().next().equals(NEVER)
                    ? PlayerPresence.NEVER_JOINED : PlayerPresence.JOINED);
        }).when(plugin).checkPlayerPresenceIdentitiesAsync(anyCollection(), eq(true));
        TopVoterState state = new TopVoterState(); field(plugin, "topVoterState", state);
        var first = new TopVoterPlayer(NEVER, "SyntheticNever", 1L);
        var second = new TopVoterPlayer(JOINED, "SyntheticJoined", 2L);
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(first, 100); raw.put(second, 90);
        for (TopVoter period : TopVoter.values()) state.getTopVoters().put(period, raw);
        state.setLastMonthTopVoters(raw); YearMonth historical = YearMonth.of(2026, 1);
        state.getPreviousMonthsTopVoters().put(historical, raw);
        UuidLookup cache = mock(UuidLookup.class); when(cache.getCachedUUID(anyString())).thenReturn("");
        try (var lookup = mockStatic(UuidLookup.class)) {
            lookup.when(UuidLookup::getInstance).thenReturn(cache);
            plugin.initializeTopVoterDisplayFilter();
            when(options.isOnlineMode()).thenReturn(false);
            for (TopVoter period : TopVoter.values()) {
                assertEquals(java.util.List.of(second), java.util.List.copyOf(plugin.getTopVoterDisplay(period).keySet()));
                assertSame(raw, plugin.getTopVoter(period));
            }
            assertEquals(java.util.List.of(second), java.util.List.copyOf(plugin.getLastMonthTopVoterDisplay().keySet()));
            assertEquals(java.util.List.of(second), java.util.List.copyOf(plugin.getPreviousMonthTopVoterDisplay(historical).keySet()));
            assertSame(raw, plugin.getLastMonthTopVoter()); assertSame(raw, plugin.getPreviousMonthsTopVoters().get(historical));
            assertEquals(100, raw.get(first)); assertEquals(2, raw.size());
            verify(plugin, never()).getServer(); verify(plugin, never()).getUserManager();
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(false);
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
            doReturn(false).when(plugin).isEnabled();
            assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
        }
    }

    @Test void chatPagesUseVisibleRanksConfiguredPageSizeAndSafeBounds() {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        Config config = mock(Config.class); when(plugin.getConfigFile()).thenReturn(config);
        when(config.getFormatPageSize()).thenReturn(3);
        when(config.getFormatCommandsVoteTopLine()).thenReturn("%num%:%player%:%votes%");
        when(config.getFormatCommandVoteTopTitle()).thenReturn("%page%/%maxpages%/%Top%");
        when(config.getFormatTopVoterMonthly()).thenReturn("Monthly");
        var visible = new LinkedHashMap<TopVoterPlayer, Integer>();
        for (int index = 1; index <= 7; index++) visible.put(new TopVoterPlayer(new UUID(0, index), "synthetic" + index, 1L), index);
        when(plugin.getTopVoterDisplay(any(TopVoter.class))).thenReturn(visible);
        var handler = new TopVoterHandler(plugin);
        assertArrayEquals(new String[] {"2/3/Monthly", "4:synthetic4:4", "5:synthetic5:5", "6:synthetic6:6"}, handler.topVoterMonthly(2));
        assertEquals(4, handler.topVoterMonthly(0).length);
        assertEquals(1, handler.topVoterMonthly(Integer.MAX_VALUE).length);
        verify(plugin, never()).getTopVoter(any(TopVoter.class));
    }

    @Test void failedAndUnavailableRuntimeEvidencePreservesDisplayAndRawState() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        doReturn(true).when(plugin).isEnabled();
        Config config = mock(Config.class); when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
        field(plugin, "configFile", config);
        var state = new TopVoterState(); field(plugin, "topVoterState", state);
        var raw = new LinkedHashMap<TopVoterPlayer, Integer>(); raw.put(new TopVoterPlayer(NEVER, "Synthetic", 1L), 1);
        state.getTopVoters().put(TopVoter.Monthly, raw);
        assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
        field(plugin, "topVoterDisplayFilter", new TopVoterDisplayFilter(row -> {
            throw new IllegalStateException("synthetic rejected provider");
        }));
        assertEquals(raw, plugin.getTopVoterDisplay(TopVoter.Monthly));
        assertSame(raw, plugin.getTopVoter(TopVoter.Monthly));
    }
}
