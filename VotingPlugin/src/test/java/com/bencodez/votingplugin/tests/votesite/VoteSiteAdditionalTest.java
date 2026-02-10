package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.Server;
import org.bukkit.inventory.ItemFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * Extra unit tests for {@link VoteSite}.
 */
public class VoteSiteAdditionalTest {

    private VotingPluginMain plugin;
    private ConfigVoteSites cfg;
    private Config configFile;
    private ServerData serverData;

    @BeforeAll
    public static void setupBukkitMinimal() {
        Server mockServer = mock(Server.class);
        ItemFactory mockItemFactory = mock(ItemFactory.class);
        Logger mockLogger = mock(Logger.class);

        when(mockServer.getItemFactory()).thenReturn(mockItemFactory);
        when(mockServer.getLogger()).thenReturn(mockLogger);

        Bukkit.setServer(mockServer);
    }

    @BeforeEach
    public void setUp() {
        plugin = mock(VotingPluginMain.class);
        cfg = mock(ConfigVoteSites.class);
        configFile = mock(Config.class);
        serverData = mock(ServerData.class);

        when(plugin.getConfigVoteSites()).thenReturn(cfg);
        when(plugin.getConfigFile()).thenReturn(configFile);
        when(plugin.getServerData()).thenReturn(serverData);

        when(cfg.getVoteURL(anyString())).thenReturn("example.com");
        when(cfg.getServiceSite(anyString())).thenReturn("ServiceSite");
        when(cfg.getVoteDelay(anyString())).thenReturn(ParsedDuration.parse("12h"));
        when(cfg.getVoteSiteEnabled(anyString())).thenReturn(true);
        when(cfg.getPriority(anyString())).thenReturn(1);
        when(cfg.getDisplayName(anyString())).thenReturn("DisplayName");
        when(cfg.getItem(anyString())).thenReturn(null);

        when(cfg.getVoteSiteResetVoteDelayDaily(anyString())).thenReturn(false);
        when(cfg.getVoteSiteGiveOffline(anyString())).thenReturn(false);
        when(cfg.getWaitUntilVoteDelay(anyString())).thenReturn(false);
        when(cfg.getVoteDelayDailyHour(anyString())).thenReturn(0);
        when(cfg.getVoteSiteHidden(anyString())).thenReturn(false);
        when(cfg.getVoteSiteIgnoreCanVote(anyString())).thenReturn(false);
        when(cfg.getPermissionToView(anyString())).thenReturn("");

        when(serverData.getServiceSites()).thenReturn(new ArrayList<>(Arrays.asList("ServiceSite", "OtherSite")));
    }

    @Test
    public void testGetVoteURLJsonStripAddsHttpsWwwWhenMissing() {
        when(configFile.isFormatCommandsVoteForceLinks()).thenReturn(false);
        VoteSite voteSite = new VoteSite(plugin, "site.test");

        String stripped = voteSite.getVoteURLJsonStrip();
        assertEquals("https://www.example.com", stripped);
    }

    @Test
    public void testIsVaidServiceSiteTrueWhenInServerList() {
        VoteSite voteSite = new VoteSite(plugin, "site.test");
        assertTrue(voteSite.isVaidServiceSite());
    }

    @Test
    public void testHasRewardsDelegatesToRewardHandler() throws Exception {
        // IMPORTANT: RewardHandler class init touches AdvancedCorePlugin.getInstance().
        // Mock the static call BEFORE referencing RewardHandler.
        AdvancedCorePlugin advancedCore = mock(AdvancedCorePlugin.class);

        File dataFolder = Files.createTempDirectory("advancedcore-test").toFile();
        when(advancedCore.getDataFolder()).thenReturn(dataFolder);
        when(advancedCore.getLogger()).thenReturn(Logger.getLogger("AdvancedCoreTest"));

        try (MockedStatic<AdvancedCorePlugin> mocked = mockStatic(AdvancedCorePlugin.class)) {
            mocked.when(AdvancedCorePlugin::getInstance).thenReturn(advancedCore);

            RewardHandler handler = mock(RewardHandler.class);
            when(plugin.getRewardHandler()).thenReturn(handler);

            when(cfg.getData()).thenReturn(null);
            when(cfg.getRewardsPath(anyString())).thenReturn("VoteSites.site_test.Rewards");
            when(handler.hasRewards(any(), anyString())).thenReturn(true);

            VoteSite voteSite = new VoteSite(plugin, "site.test");
            assertTrue(voteSite.hasRewards());

            verify(handler).hasRewards(any(), eq("VoteSites.site_test.Rewards"));
        }
    }
}
