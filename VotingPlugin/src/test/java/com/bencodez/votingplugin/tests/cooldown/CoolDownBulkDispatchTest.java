package com.bencodez.votingplugin.tests.cooldown;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Server;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.Event;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
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

class CoolDownBulkDispatchTest {
    private static final UUID UUID_VALUE = UUID.fromString("00000000-0000-0000-0000-000000000123");

    @Test void singleSiteScanKeepsIdentityUntilDeferredRewardRuns() throws Exception { exercise(false, "OfflinePlayer", null, "OfflinePlayer"); }
    @Test void allSitesScanKeepsIdentityUntilDeferredRewardRuns() throws Exception { exercise(true, "OfflinePlayer", null, "OfflinePlayer"); }
    @Test void absentNameStillDispatchesTheEvent() throws Exception { exercise(false, null, null, ""); }
    @Test void explicitNameSurvivesCachedNameAndClear() throws Exception { exercise(true, "OldName", "OnlineName", "OnlineName"); }

    @Test void uuidEntryPointsUseExistingUserAndOfflineFetchMode() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        VotingPluginUser user = mock(VotingPluginUser.class);
        VoteSite site = mock(VoteSite.class);
        Config config = mock(Config.class);
        VoteSiteManager sites = mock(VoteSiteManager.class);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(UUID_VALUE)).thenReturn(user);
        when(user.isOnline()).thenReturn(false);
        when(user.getCoolDownCheck()).thenReturn(true);
        when(user.canVoteAll()).thenReturn(true);
        when(user.getJavaUUID()).thenReturn(UUID_VALUE);
        when(user.getCoolDownCheckSiteList()).thenReturn(new HashMap<>());
        when(user.canVoteSite(site)).thenReturn(true);
        when(site.getKey()).thenReturn("site");
        when(plugin.getConfigFile()).thenReturn(config);
        when(config.isPerSiteCoolDownEvents()).thenReturn(true);
        when(plugin.getVoteSiteManager()).thenReturn(sites);
        when(sites.getVoteSitesEnabled()).thenReturn(new ArrayList<>(List.of(site)));
        CoolDownCheck check = new CoolDownCheck(plugin);
        try {
            check.check(UUID_VALUE);
            check.checkPerSite(UUID_VALUE);
            verify(plugin.getVotingPluginUserManager(), times(2)).getVotingPluginUser(UUID_VALUE);
            verify(user, times(2)).userDataFetechMode(UserDataFetchMode.NO_CACHE);
            verify(plugin.getServer().getPluginManager()).callEvent(isA(PlayerVoteCoolDownEndEvent.class));
            verify(plugin.getServer().getPluginManager()).callEvent(isA(PlayerVoteSiteCoolDownEndEvent.class));
            verify(user, never()).setPlayerName(anyString());
        } finally {
            check.shutdown();
        }
    }

    private void exercise(boolean allSites, String cachedName, String explicitName, String expected) throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        Config config = mock(Config.class);
        VoteSiteManager sites = mock(VoteSiteManager.class);
        VoteSite site = mock(VoteSite.class);
        RewardHandler rewards = mock(RewardHandler.class);
        SpecialRewardsConfig special = mock(SpecialRewardsConfig.class);
        Server server = mock(Server.class);
        PluginManager events = mock(PluginManager.class);
        YamlConfiguration data = new YamlConfiguration();
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
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
        when(user.getPlugin()).thenReturn(plugin);
        doCallRealMethod().when(user).getPlayerName();
        doCallRealMethod().when(user).setPlayerName(anyString());
        doCallRealMethod().when(user).userDataFetechMode(any(UserDataFetchMode.class));
        doCallRealMethod().when(user).updateTempCacheWithColumns(any());
        doCallRealMethod().when(user).clearTempCache();
        if (explicitName != null) user.setPlayerName(explicitName);
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

        CoolDownCheck check = new CoolDownCheck(plugin);
        AtomicReference<Runnable> deferred = new AtomicReference<>();
        AtomicReference<String> seen = new AtomicReference<>();
        doAnswer(call -> {
            AdvancedCoreUser recipient = call.getArgument(0);
            deferred.set(() -> seen.set(recipient.getPlayerName()));
            return null;
        }).when(rewards).giveReward(any(AdvancedCoreUser.class), any(), anyString(), any(RewardOptions.class));
        doAnswer(call -> {
            Event event = call.getArgument(0);
            if (event instanceof PlayerVoteCoolDownEndEvent) check.onCoolDownEnd((PlayerVoteCoolDownEndEvent) event);
            if (event instanceof PlayerVoteSiteCoolDownEndEvent) check.onCoolDownEnd((PlayerVoteSiteCoolDownEndEvent) event);
            return null;
        }).when(events).callEvent(any(Event.class));
        try {
            if (allSites) check.checkAllVoteSites(); else check.checkAllVoteSite(site);
            assertNull(userData.getTempCache());
            assertNotNull(deferred.get(), "cooldown listener should dispatch reward");
            deferred.get().run();
            assertEquals(expected, seen.get());
            assertEquals(UUID_VALUE, user.getJavaUUID());
            verify(user).setCoolDownCheckSite(any());
            if (allSites) verify(user).setCoolDownCheck(false);
        } finally {
            check.shutdown();
        }
    }
}
