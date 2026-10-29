package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Server;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.Event;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import java.nio.file.Path;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.data.DataValueString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.cooldown.CoolDownCheck;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class CoolDownRewardIntegrationTest {
    @TempDir Path tempDir;
    private static final UUID UUID_VALUE = UUID.fromString("00000000-0000-0000-0000-000000000123");

    @Test void singleSiteCommandListUsesCapturedOfflineName() throws Exception { exercise(false, "OfflinePlayer", false, false, false, false, false); }
    @Test void allSiteCommandUsesCapturedOfflineName() throws Exception { exercise(true, "OfflinePlayer", false, false, false, false, false); }
    @Test void missingIdentityRejectsPlayerTargetAndLogsFailure() throws Exception { exercise(false, null, false, true, false, false, false); }
    @Test void missingIdentityAllowsLiteralCommand() throws Exception { exercise(false, null, true, false, false, false, false); }

    @Test void onlineIdentityUsesPlayerOwnerScheduler() throws Exception { exercise(false, "OldName", false, false, true, false, false); }

    @Test void missingIdentityDoesNotReplayCompletedEffect() throws Exception { exercise(false, null, false, true, false, true, false); }

    @Test void missingOfflineNameCannotTargetUnrelatedPlayer() throws Exception { exercise(false, null, true, true, false, false, true); }

    private void exercise(boolean allSites, String cachedName, boolean literal, boolean reject, boolean online, boolean partial, boolean mixed) throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        Config config = mock(Config.class);
        VoteSiteManager sites = mock(VoteSiteManager.class);
        VoteSite site = mock(VoteSite.class);
        when(plugin.getDataFolder()).thenReturn(tempDir.toFile());
        RewardHandler rewards = new RewardHandler(plugin);
        com.bencodez.advancedcore.api.rewards.builtin.RewardCommands.registerCommand(rewards, plugin);
        com.bencodez.advancedcore.api.rewards.builtin.RewardCommands.registerCommands(rewards, plugin);
        java.util.concurrent.atomic.AtomicInteger completed = new java.util.concurrent.atomic.AtomicInteger();
        if (partial) rewards.getInjectedRewards().add(0, new com.bencodez.advancedcore.api.rewards.injected.RewardInject("Marker") {
            @Override public Object onRewardRequest(com.bencodez.advancedcore.api.rewards.Reward reward,
                    AdvancedCoreUser recipient, org.bukkit.configuration.ConfigurationSection data,
                    HashMap<String, String> placeholders) { completed.incrementAndGet(); return null; }
        });
        SpecialRewardsConfig special = mock(SpecialRewardsConfig.class);
        Server server = mock(Server.class);
        PluginManager events = mock(PluginManager.class);
        YamlConfiguration data = new YamlConfiguration();
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getOptions().getFormatRewardTimeFormat()).thenReturn("yyyy-MM-dd");
        var oldInstance = com.bencodez.advancedcore.AdvancedCorePlugin.getInstance();
        com.bencodez.advancedcore.AdvancedCorePlugin.setInstance(plugin);
        var misc = com.bencodez.advancedcore.api.misc.MiscUtils.getInstance();
        Field miscPlugin = misc.getClass().getDeclaredField("plugin"); miscPlugin.setAccessible(true);
        Object oldMiscPlugin = miscPlugin.get(misc); miscPlugin.set(misc, plugin);
        var scheduler = mock(com.bencodez.simpleapi.scheduler.BukkitScheduler.class);
        java.util.ArrayDeque<Runnable> queued = new java.util.ArrayDeque<>();
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);
        doAnswer(call -> { queued.add(call.getArgument(1)); return null; }).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class));
        doAnswer(call -> { queued.add(call.getArgument(1)); return null; }).when(scheduler).executeOrScheduleSync(eq(plugin), any(Runnable.class), any(org.bukkit.entity.Player.class));
        doAnswer(call -> { queued.add(call.getArgument(1)); return null; }).when(scheduler).runTask(eq(plugin), any(Runnable.class));
        doAnswer(call -> { queued.add(call.getArgument(1)); return null; }).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));

        Logger logger = mock(Logger.class);
        when(plugin.getLogger()).thenReturn(logger);
        when(plugin.getStorageType()).thenReturn(UserStorage.SQLITE);
        when(plugin.getConfigFile()).thenReturn(config);
        when(config.isPerSiteCoolDownEvents()).thenReturn(true);
        when(plugin.getVoteSiteManager()).thenReturn(sites);
        when(plugin.getRewardHandler()).thenReturn(rewards);
        when(plugin.getSpecialRewardsConfig()).thenReturn(special);
        when(special.getData()).thenReturn(data);
        when(plugin.getServer()).thenReturn(server);
        when(server.getPluginManager()).thenReturn(events);
        when(site.getKey()).thenReturn("site");
        when(site.getDisplayName()).thenReturn("Site");
        when(site.getVoteURL(false)).thenReturn("https://example.test");
        when(site.getSiteData()).thenReturn(data);
        when(site.isVoteDelayDaily()).thenReturn(true);
        when(sites.getVoteSites()).thenReturn(new ArrayList<>(List.of(site)));

        VotingPluginUser user = mock(VotingPluginUser.class);
        Field pluginField = AdvancedCoreUser.class.getDeclaredField("plugin");
        pluginField.setAccessible(true);
        pluginField.set(user, plugin);
        Field dataField = AdvancedCoreUser.class.getDeclaredField("data");
        dataField.setAccessible(true);
        UserData userData = new UserData(user);
        dataField.set(user, userData);
        when(user.getUserData()).thenReturn(userData);
        when(user.getUUID()).thenReturn(UUID_VALUE.toString());
        when(user.endAsyncActionCollection(any())).thenReturn(java.util.concurrent.CompletableFuture.completedFuture(null));
        when(user.getPlugin()).thenReturn(plugin);
        doCallRealMethod().when(user).getPlayerName();
        doCallRealMethod().when(user).setPlayerName(anyString());
        doCallRealMethod().when(user).userDataFetechMode(any(UserDataFetchMode.class));
        doCallRealMethod().when(user).updateTempCacheWithColumns(any());
        doCallRealMethod().when(user).clearTempCache();
        org.bukkit.entity.Player live = mock(org.bukkit.entity.Player.class);
        if (mixed) {
            when(plugin.getOptions().isOnlineMode()).thenReturn(false);
            doCallRealMethod().when(user).getPlayer();
            doCallRealMethod().when(user).validatePlayerCommandAvailabilityAsync();
        }
        if (online) {
            when(user.isOnline()).thenReturn(true);
            when(user.getPlayer()).thenReturn(live);
            user.setPlayerName("OnlinePlayer");
        }
        when(user.getCoolDownCheckSiteList()).thenReturn(new HashMap<>());
        when(user.canVoteSite(site)).thenReturn(true);
        when(user.getCoolDownCheck()).thenReturn(allSites);
        when(user.canVoteAll()).thenReturn(true);
        when(user.getJavaUUID()).thenReturn(UUID_VALUE);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(UUID_VALUE, false)).thenReturn(user);
        com.bencodez.advancedcore.api.user.UserManager storage = plugin.getUserManager();
        doAnswer(call -> {
            BiConsumer<UUID, ArrayList<Column>> each = call.getArgument(0);
            Consumer<Integer> done = call.getArgument(1);
            ArrayList<Column> columns = new ArrayList<>();
            if (cachedName != null) columns.add(new Column("PlayerName", new DataValueString(cachedName)));
            each.accept(UUID_VALUE, columns);
            done.accept(1);
            return null;
        }).when(storage).forEachUserKeys(any(), any());

        data.set("VoteCoolDownEndedReward.ForceOffline", true);
        data.set("VoteCoolDownEndedReward.Command", literal ? "say ready" : "say %player%");
        data.set("CoolDownEndRewards.ForceOffline", true);
        if (partial) data.set("CoolDownEndRewards.Marker", true);
        if (mixed) {
            data.set("CoolDownEndRewards.Commands.Console", new ArrayList<>(List.of("say ready")));
            data.set("CoolDownEndRewards.Commands.Player", new ArrayList<>(List.of("unrelated-action")));
        } else data.set("CoolDownEndRewards.Commands", new ArrayList<>(List.of(literal ? "say ready" : "say %player%")));
        CoolDownCheck check = new CoolDownCheck(plugin);
        doAnswer(call -> {
            Event event = call.getArgument(0);
            if (event instanceof PlayerVoteCoolDownEndEvent) check.onCoolDownEnd((PlayerVoteCoolDownEndEvent) event);
            if (event instanceof PlayerVoteSiteCoolDownEndEvent) check.onCoolDownEnd((PlayerVoteSiteCoolDownEndEvent) event);
            return null;
        }).when(events).callEvent(any(Event.class));
        try (var bukkit = mockStatic(org.bukkit.Bukkit.class)) {
            var console = mock(org.bukkit.command.ConsoleCommandSender.class);
            var offline = mock(org.bukkit.OfflinePlayer.class);
            var unrelated = mock(org.bukkit.entity.Player.class);
            bukkit.when(org.bukkit.Bukkit::getServer).thenReturn(server);
            bukkit.when(org.bukkit.Bukkit::getPluginManager).thenReturn(events);
            bukkit.when(org.bukkit.Bukkit::getConsoleSender).thenReturn(console);
            bukkit.when(() -> org.bukkit.Bukkit.getOfflinePlayer(anyString())).thenReturn(offline);
            if (online) { when(offline.isOnline()).thenReturn(true); when(offline.getPlayer()).thenReturn(live); }
            if (mixed) bukkit.when(() -> org.bukkit.Bukkit.getPlayer("")).thenReturn(unrelated);
            if (allSites) check.checkAllVoteSites(); else check.checkAllVoteSite(site);
            assertNull(userData.getTempCache());
            assertFalse(queued.isEmpty(), "real reward must be deferred");
            verify(server, never()).dispatchCommand(any(), anyString());
            while (!queued.isEmpty()) queued.remove().run();
            if (reject) {
                verify(server, never()).dispatchCommand(any(), anyString());
                verify(logger).log(eq(java.util.logging.Level.WARNING), contains("Failed to give reward"), any(Throwable.class));
                var attempted = org.mockito.ArgumentCaptor.forClass(String.class);
                var failure = org.mockito.ArgumentCaptor.forClass(String.class);
                verify(logger, atLeastOnce()).info(attempted.capture());
                verify(logger).warning(failure.capture());
                String admission = attempted.getAllValues().stream()
                    .filter(value -> value.startsWith("Cooldown reward status=attempted"))
                    .findFirst().orElseThrow();
                var matcher = java.util.regex.Pattern.compile("occurrence=([0-9a-f-]+)").matcher(admission);
                assertTrue(matcher.find());
                assertTrue(failure.getValue().contains("occurrence=" + matcher.group(1)));
                assertTrue(failure.getValue().contains("player=" + UUID_VALUE));
                assertTrue(failure.getValue().contains("site=site:site"));
                assertTrue(failure.getValue().contains("outcome=unknown/partial"));
                assertTrue(failure.getValue().contains("completedInjections="));
                if (partial) {
                    var count = java.util.regex.Pattern.compile("completedInjections=([0-9]+)").matcher(failure.getValue());
                    assertTrue(count.find());
                    assertTrue(Integer.parseInt(count.group(1)) >= 1);
                }
                assertFalse(failure.getValue().contains("%player%"));
                assertEquals(partial ? 1 : 0, completed.get());
            } else {
                verify(server, times(allSites ? 2 : 1)).dispatchCommand(console, literal ? "say ready" : online ? "say OnlinePlayer" : "say OfflinePlayer");
            }
            bukkit.verify(() -> org.bukkit.Bukkit.getOfflinePlayer(""), never());
            if (mixed) {
                bukkit.verify(() -> org.bukkit.Bukkit.getPlayer(""), never());
                verify(unrelated, never()).chat(anyString());
            }
            verify(scheduler, atLeastOnce()).executeOrScheduleSync(eq(plugin), any(Runnable.class));
            verify(scheduler, atLeastOnce()).runTaskAsynchronously(eq(plugin), any(Runnable.class));
            if (online) verify(scheduler, atLeastOnce()).executeOrScheduleSync(eq(plugin), any(Runnable.class), eq(live));
            assertEquals(UUID_VALUE, user.getJavaUUID());
            verify(user).setCoolDownCheckSite(any());
            if (allSites) verify(user).setCoolDownCheck(false);
        } finally {
            check.shutdown();
            rewards.getDelayedTimer().shutdownNow();
            miscPlugin.set(misc, oldMiscPlugin);
            com.bencodez.advancedcore.AdvancedCorePlugin.setInstance(oldInstance);
        }
    }
}
