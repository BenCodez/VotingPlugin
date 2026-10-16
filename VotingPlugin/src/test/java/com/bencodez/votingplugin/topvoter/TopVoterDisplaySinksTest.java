package com.bencodez.votingplugin.topvoter;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.time.YearMonth;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import org.bukkit.Location;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.mockito.ArgumentCaptor;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.simpleapi.dialog.MultiActionDialogBuilder;
import com.bencodez.simpleapi.dialog.UniDialogService;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoter;
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoterLastMonth;
import com.bencodez.votingplugin.commands.gui.player.VoteTopVoterPreviousMonths;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.discord.DiscordHandler;
import com.bencodez.votingplugin.signs.SignHandler;
import com.bencodez.votingplugin.user.VotingPluginUser;
import github.scarsz.discordsrv.dependencies.jda.api.entities.MessageEmbed;

class TopVoterDisplaySinksTest {
    private VotingPluginMain previousPlugin;
    @BeforeEach void savePlugin() { previousPlugin = VotingPluginMain.plugin; }
    @AfterEach void restorePlugin() { VotingPluginMain.plugin = previousPlugin; }
    private static final YearMonth MONTH = YearMonth.of(2026, 1);
    private static final class Fixture {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
        final Config config = mock(Config.class);
        final GUI gui = mock(GUI.class);
        final Player viewer = mock(Player.class);
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final LinkedHashMap<TopVoterPlayer, Integer> raw = new LinkedHashMap<>();
        Fixture() throws Exception {
            doReturn(true).when(plugin).isEnabled();
            doReturn(0L).when(plugin).getTopVoterDisplayGeneration();
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
            when(gui.getChestVoteTopPlayerItemMaterial()).thenReturn("PAPER");
            when(gui.getChestVoteTopSwitchItemTopVoters()).thenReturn(new ArrayList<>());
            when(gui.getChestGUIExtraItems(anyString())).thenReturn(java.util.Set.of());
            doReturn(gui).when(plugin).getGui();
            when(user.getPlayerName()).thenReturn("SyntheticViewer");
            raw.put(new TopVoterPlayer(new UUID(0, 1), "SyntheticNever", 1L), 100);
            raw.put(new TopVoterPlayer(new UUID(0, 2), "SyntheticJoined", 2L), 90);
            raw.put(new TopVoterPlayer(new UUID(0, 3), "SyntheticUnknown", 3L), 80);
            var state = new TopVoterState();
            for (TopVoter period : TopVoter.values()) state.getTopVoters().put(period, raw);
            state.setLastMonthTopVoters(raw); state.getPreviousMonthsTopVoters().put(MONTH, raw);
            set("configFile", config); set("topVoterState", state);
            VotingPluginMain.plugin = plugin;
            set("topVoterDisplayFilter", new TopVoterDisplayFilter(row -> CompletableFuture.completedStage(
                    row.getPlayerName().equals("SyntheticNever") ? PlayerPresence.NEVER_JOINED
                    : row.getPlayerName().equals("SyntheticJoined") ? PlayerPresence.JOINED : PlayerPresence.UNKNOWN)));
        }
        void set(String name, Object value) throws Exception {
            var field = VotingPluginMain.class.getDeclaredField(name); field.setAccessible(true); field.set(plugin, value);
        }
        void assertRaw() {
            assertEquals(3, raw.size()); assertEquals(100, raw.values().iterator().next());
            assertSame(raw, plugin.getTopVoter(TopVoter.Monthly));
            assertSame(raw, plugin.getLastMonthTopVoter());
            assertSame(raw, plugin.getPreviousMonthsTopVoters().get(MONTH));
        }
    }

    @Test void currentAndHistoricalDialogsUseVisibleContiguousRanks() throws Exception {
        var f = new Fixture(); var service = mock(UniDialogService.class);
        doReturn(service).when(f.plugin).getDialogService();
        for (int board = 0; board < 3; board++) {
            var dialog = mock(MultiActionDialogBuilder.class, RETURNS_SELF);
            when(service.multiAction(f.viewer)).thenReturn(dialog);
            if (board == 0) new VoteTopVoter(f.plugin, f.viewer, f.user, TopVoter.Monthly, 0).onDialog(f.viewer);
            if (board == 1) new VoteTopVoterLastMonth(f.plugin, f.viewer, f.user).onDialog(f.viewer);
            if (board == 2) new VoteTopVoterPreviousMonths(f.plugin, f.viewer, f.user, 0).onDialog(f.viewer);
            verify(dialog).placeholder("position", "1"); verify(dialog).placeholder("position", "2");
            verify(dialog).placeholder("player", "SyntheticJoined"); verify(dialog).placeholder("player", "SyntheticUnknown");
            verify(dialog, never()).placeholder("player", "SyntheticNever");
            verify(dialog).placeholder("votes", "90"); verify(dialog).placeholder("votes", "80");
            verify(dialog).open(); f.assertRaw();
        }
    }

    @Test void currentAndHistoricalChestButtonsUseVisibleRowsAndRanks() throws Exception {
        var f = new Fixture();
        for (int board = 0; board < 3; board++) {
            try (var builders = mockConstruction(ItemBuilder.class, withSettings().defaultAnswer(RETURNS_SELF));
                 var inventories = mockConstruction(BInventory.class, (inv, context) -> {
                     when(inv.getPageButtons()).thenReturn(new ArrayList<>());
                 })) {
                if (board == 0) new VoteTopVoter(f.plugin, f.viewer, f.user, TopVoter.Monthly, 0).onChest(f.viewer);
                if (board == 1) new VoteTopVoterLastMonth(f.plugin, f.viewer, f.user).onChest(f.viewer);
                if (board == 2) new VoteTopVoterPreviousMonths(f.plugin, f.viewer, f.user, 0).onChest(f.viewer);
                assertEquals(1, inventories.constructed().size()); var inv = inventories.constructed().getFirst();
                var buttons = ArgumentCaptor.forClass(BInventoryButton.class);
                verify(inv, times(2)).addButton(buttons.capture());
                for (int index = 0; index < 2; index++) {
                    var builder = buttons.getAllValues().get(index).getBuilder();
                    verify(builder).addPlaceholder("position", Integer.toString(index + 1));
                    verify(builder).addPlaceholder("player", index == 0 ? "SyntheticJoined" : "SyntheticUnknown");
                    verify(builder).addPlaceholder("votes", index == 0 ? "90" : "80");
                }
                verify(inv).openInventory(f.viewer); f.assertRaw();
            }
        }
    }

    @Test void signsRenderOneVisibleSnapshotAndNoPlayerAfterLastRank() throws Exception {
        var f = new Fixture(); var scheduler = mock(BukkitScheduler.class);
        doReturn(scheduler).when(f.plugin).getBukkitScheduler();
        Location location = mock(Location.class, RETURNS_DEEP_STUBS);
        when(location.clone()).thenReturn(location);
        Location skull = mock(Location.class, RETURNS_DEEP_STUBS);
        Sign sign = mock(Sign.class); when(location.getBlock().getState()).thenReturn(sign);
        when(skull.getBlock().getState()).thenReturn(mock(BlockState.class));
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(scheduler).runTask(eq(f.plugin), any(Runnable.class), eq(location));
        doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                .when(scheduler).runTaskLater(eq(f.plugin), any(Runnable.class), anyLong(), eq(location));
        when(f.config.getFormatSignTopVoterSignLine1()).thenReturn("%position%:%player%");
        when(f.config.getFormatSignTopVoterSignLine2()).thenReturn("%votes%");
        when(f.config.getFormatSignTopVoterSignLine3()).thenReturn("%SiteName%");
        when(f.config.getFormatSignTopVoterSignLine4()).thenReturn("");
        for (String board : List.of("all", "monthly", "weekly", "daily")) {
            var handler = new SignHandler(f.plugin, "synthetic", location, skull, board, 1);
            handler.updateLines(); handler.updateSign(0);
            verify(sign).setLine(0, "1:SyntheticJoined"); verify(sign).setLine(1, "90");
            clearInvocations(sign);
            handler = new SignHandler(f.plugin, "synthetic", location, skull, board, 3);
            handler.updateLines(); handler.updateSign(0);
            verify(sign).setLine(0, "3:No Player"); verify(sign).setLine(1, "0");
            clearInvocations(sign); f.assertRaw();
        }
    }

    @Test void discordEmbedContainsOnlyVisibleRowsAndRetainsUnknown() throws Exception {
        var f = new Fixture();
        var render = DiscordHandler.class.getDeclaredMethod("buildTopVoterEmbed", String.class, LinkedHashMap.class);
        render.setAccessible(true);
        var builder = (github.scarsz.discordsrv.dependencies.jda.api.EmbedBuilder) render.invoke(null,
                "Synthetic board", f.plugin.getTopVoterDisplay(TopVoter.Monthly));
        MessageEmbed embed = builder.build();
        String text = embed.getDescription();
        assertNotNull(text); assertFalse(text.contains("SyntheticNever"));
        assertTrue(text.contains("SyntheticJoined")); assertTrue(text.contains("SyntheticUnknown"));
        assertTrue(text.indexOf("SyntheticJoined") < text.indexOf("SyntheticUnknown"));
        assertTrue(text.contains("90 votes")); assertTrue(text.contains("80 votes")); f.assertRaw();
        var empty = (github.scarsz.discordsrv.dependencies.jda.api.EmbedBuilder) render.invoke(null,
                "Synthetic board", new LinkedHashMap<>());
        assertEquals("No votes recorded yet.", empty.build().getDescription());
    }
}
