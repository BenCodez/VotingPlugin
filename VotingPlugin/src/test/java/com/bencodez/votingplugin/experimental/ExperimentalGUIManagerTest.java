package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemFactory;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.hologram.HologramMenuScheduler;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.ClickEvent;

class ExperimentalGUIManagerTest {
    @Test void invalidSettingsListReportsValidationToConsoleWithoutListingOrCreatingResources() {
        try (Fixture f = new Fixture()) {
            f.config.set("Experimental.VoteGUIs.MaxActiveSessions", 0);
            var console = mock(org.bukkit.command.ConsoleCommandSender.class);
            when(console.hasPermission("VotingPlugin.Commands.AdminVote.TestGUI")).thenReturn(true);
            assertDoesNotThrow(() -> f.manager.control(console, "list"));
            verify(console, times(1)).sendMessage(contains("Invalid experimental GUI settings: MaxSessions 1..64"));
            assertTrue(f.owner.isEmpty());
            assertTrue(f.storage.isEmpty());
            assertTrue(f.created.isEmpty());
        }
    }

    @Test void invalidSettingsListReportsValidationOnPlayerOwnerWithoutCreatingResources() {
        try (Fixture f = new Fixture()) {
            f.config.set("Experimental.VoteGUIs.SitesPerPage", 6);
            f.manager.control(f.player, "list");
            verify(f.player, never()).sendMessage(anyString());
            assertDoesNotThrow(f::owner);
            verify(f.player, times(1)).sendMessage(contains("Invalid experimental GUI settings:"));
            assertTrue(f.storage.isEmpty());
            assertTrue(f.created.isEmpty());
        }
    }

    @Test void validSettingsListStillReportsEveryStyle() {
        try (Fixture f = new Fixture()) {
            var console = mock(org.bukkit.command.ConsoleCommandSender.class);
            when(console.hasPermission("VotingPlugin.Commands.AdminVote.TestGUI")).thenReturn(true);
            f.manager.control(console, "list");
            verify(console, times(ExperimentalGUIType.values().length)).sendMessage(anyString());
            verify(console, never()).sendMessage(contains("Invalid experimental GUI settings:"));
            assertTrue(f.storage.isEmpty());
        }
    }

    static final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        final Player player = mock(Player.class);
        final UUID id = UUID.randomUUID();
        final VotingPluginUser user = mock(VotingPluginUser.class);
        final YamlConfiguration config = new YamlConfiguration();
        final Queue<Runnable> owner = new ArrayDeque<>();
        final Queue<Runnable> storage = new ArrayDeque<>();
        final List<Runnable> watches = new ArrayList<>();
        final List<Inventory> created = new ArrayList<>();
        final AtomicReference<Inventory> top = new AtomicReference<>(mock(Inventory.class));
        final MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class);
        final ExperimentalGUIManager manager;
        final Player.Spigot chat = mock(Player.Spigot.class);

        Fixture() {
            config.set("Experimental.VoteGUIs.Enabled", true);
            when(plugin.getConfigFile().getData()).thenReturn(config);
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
            when(plugin.getUser(id)).thenReturn(user);
            when(player.getUniqueId()).thenReturn(id);
            when(player.isOnline()).thenReturn(true);
            when(player.hasPermission(anyString())).thenReturn(true);
            World world = mock(World.class);
            when(player.getWorld()).thenReturn(world);
            when(player.getEyeLocation()).thenReturn(new Location(world, 0, 64, 0));
            InventoryView view = mock(InventoryView.class);
            when(view.getTopInventory()).thenAnswer(inv -> top.get());
            when(player.getOpenInventory()).thenReturn(view);
            when(player.spigot()).thenReturn(chat);
            doAnswer(inv -> { top.set(inv.getArgument(0)); return view; }).when(player).openInventory(any(Inventory.class));
            doAnswer(inv -> { top.set(mock(Inventory.class)); return null; }).when(player).closeInventory();
            doAnswer(inv -> { owner.add(inv.getArgument(1)); return null; }).when(scheduler).player(any(), any(), any());
            when(scheduler.watchPlayer(any(), any(), any())).thenAnswer(inv -> {
                watches.add(inv.getArgument(1));
                return (Runnable) () -> {};
            });
            var timer = plugin.getUserManager().getDataManager().getTimer();
            doAnswer(inv -> { storage.add(inv.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>());
            bukkit.when(Bukkit::getItemFactory).thenReturn(mock(ItemFactory.class));
            bukkit.when(() -> Bukkit.createInventory(any(InventoryHolder.class), eq(54), anyString())).thenAnswer(inv -> {
                Inventory inventory = mock(Inventory.class);
                InventoryHolder holder = inv.getArgument(0);
                when(inventory.getHolder()).thenReturn(holder);
                when(inventory.getSize()).thenReturn(54);
                created.add(inventory);
                return inventory;
            });
            manager = new ExperimentalGUIManager(plugin, scheduler);
        }

        void owner() { while (!owner.isEmpty()) owner.remove().run(); }
        void open() { manager.open(player, ExperimentalGUIType.ANIMATED_INVENTORY); owner(); }
        void snapshot() { storage.remove().run(); owner(); }

        VoteSite site(String key, String url) {
            VoteSite site = mock(VoteSite.class, RETURNS_DEEP_STUBS);
            when(site.getKey()).thenReturn(key);
            when(site.getDisplayName()).thenReturn(key);
            when(site.getPermissionToView()).thenReturn("");
            when(site.getVoteURL(false)).thenReturn(url);
            when(user.canVoteSite(site)).thenReturn(true);
            return site;
        }

        void click(int slot) {
            InventoryClickEvent event = mock(InventoryClickEvent.class);
            InventoryView view = player.getOpenInventory();
            when(event.getView()).thenReturn(view);
            when(event.getWhoClicked()).thenReturn(player);
            when(event.getClick()).thenReturn(ClickType.LEFT);
            when(event.getRawSlot()).thenReturn(slot);
            ((ExperimentalInventoryListener) manager.inventoryListener()).click(event);
        }

        @Override public void close() { manager.shutdown(); owner(); bukkit.close(); }
    }

    @Test void foliaTranslationRetiresNativeMenuWithoutTeleportEventAndKeepsProductionInventory() {
        try (Fixture f = new Fixture()) {
            when(f.scheduler.regionized()).thenReturn(true);
            World world = f.player.getWorld();
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 8));
            Inventory production = f.top.get();
            f.manager.open(f.player, ExperimentalGUIType.HOLOGRAM); f.owner();
            assertEquals(1, f.watches.size());
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 7));
            f.watches.getFirst().run(); f.owner();
            verify(f.plugin.getHologramVoteMenu()).close(eq(f.id), any());
            assertSame(production, f.top.get());
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void foliaRotationDoesNotCloseMenuAndOldGuardCannotRetireReplacement() {
        try (Fixture f = new Fixture()) {
            when(f.scheduler.regionized()).thenReturn(true);
            World world = f.player.getWorld();
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 8));
            f.manager.open(f.player, ExperimentalGUIType.HOLOGRAM); f.owner();
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 8, 90, 45));
            f.watches.getFirst().run(); f.owner();
            verify(f.plugin.getHologramVoteMenu(), never()).close(eq(f.id), any());
            f.manager.open(f.player, ExperimentalGUIType.HOLOGRAM); f.owner();
            clearInvocations(f.plugin.getHologramVoteMenu());
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 7));
            f.watches.getFirst().run(); f.owner();
            verify(f.plugin.getHologramVoteMenu(), never()).close(eq(f.id), any());
            f.watches.get(1).run(); f.owner();
            verify(f.plugin.getHologramVoteMenu()).close(eq(f.id), any());
        }
    }

    @Test void foliaInventoryStylesFailBeforeAdmissionAndNeverTouchProductionInventory() {
        try (Fixture f = new Fixture()) {
            when(f.scheduler.regionized()).thenReturn(true);
            Inventory production = f.top.get();
            for (var type : List.of(ExperimentalGUIType.ANIMATED_INVENTORY,
                    ExperimentalGUIType.STREAK_TRACK, ExperimentalGUIType.NPC)) {
                f.manager.open(f.player, type); f.owner();
            }
            assertSame(production, f.top.get());
            assertTrue(f.created.isEmpty());
            assertTrue(f.storage.isEmpty());
            assertTrue(f.watches.isEmpty());
            verify(f.player, never()).openInventory(any(Inventory.class));
            verify(f.player, never()).closeInventory();
            verify(f.plugin, never()).getUser(any(UUID.class));
        }
    }

    @Test void foliaHologramGetsTheSameOwnerScheduledTranslationGuard() {
        try (Fixture f = new Fixture()) {
            when(f.scheduler.regionized()).thenReturn(true);
            World world = f.player.getWorld();
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 8));
            f.manager.open(f.player, ExperimentalGUIType.HOLOGRAM); f.owner();
            assertEquals(1, f.watches.size());
            when(f.player.getLocation()).thenReturn(new Location(world, 8, 64, 7));
            f.watches.getFirst().run(); f.owner();
            verify(f.plugin.getHologramVoteMenu()).close(eq(f.id), any());
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void creationUsesWorkerSnapshotAndOnlyOwnerSchedulerOpensInventory() {
        try (Fixture f = new Fixture()) {
            f.open();
            assertEquals(1, f.storage.size());
            verify(f.plugin, never()).getUser(f.id);
            verify(f.player, never()).openInventory(any(Inventory.class));
            f.storage.remove().run();
            verify(f.plugin).getUser(f.id);
            verify(f.player, never()).openInventory(any(Inventory.class));
            f.owner();
            assertEquals(1, f.created.size());
            assertTrue(f.top.get().getHolder() instanceof ExperimentalInventory);
            verify(f.user).getDayVoteStreak();
            verify(f.user).getWeekVoteStreak();
            verify(f.user).getMonthVoteStreak();
            verify(f.user).getTotal(com.bencodez.votingplugin.topvoter.TopVoter.AllTime);
            verifyNoMoreInteractions(f.user);
        }
    }

    @Test void replacingBeforeSnapshotDoesNotOpenStaleViewOrCloseSuccessor() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.open();
            assertEquals(2, f.storage.size());
            f.snapshot();
            assertTrue(f.created.isEmpty());
            f.snapshot();
            assertEquals(1, f.created.size());
            f.watches.getFirst().run();
            f.owner();
            assertSame(f.created.getFirst(), f.top.get());
        }
    }

    @Test void correctPageAndLatestConfiguredUrlAreSelectedWithoutRewards() {
        try (Fixture f = new Fixture()) {
            List<VoteSite> sites = new ArrayList<>();
            for (int i = 0; i < 6; i++) sites.add(f.site("site" + i, "https://example.test/" + i));
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(sites));
            f.open(); f.snapshot();
            f.click(53); f.owner();
            when(sites.get(5).getVoteURL(false)).thenReturn("https://example.test/changed");
            f.click(20); f.owner();
            var captor = org.mockito.ArgumentCaptor.forClass(BaseComponent.class);
            verify(f.chat).sendMessage(captor.capture());
            assertEquals(ClickEvent.Action.OPEN_URL, captor.getValue().getClickEvent().getAction());
            assertEquals("https://example.test/changed", captor.getValue().getClickEvent().getValue());
            verify(f.user, times(6)).canVoteSite(any());
            verify(f.user, times(6)).voteNextDurationTime(any());
            verify(f.user).getDayVoteStreak();
            verify(f.user).getWeekVoteStreak();
            verify(f.user).getMonthVoteStreak();
            verify(f.user).getTotal(com.bencodez.votingplugin.topvoter.TopVoter.AllTime);
            verifyNoMoreInteractions(f.user);
        }
    }

    @Test void invalidUrlAndPermissionChangesCannotSendLink() {
        try (Fixture f = new Fixture()) {
            VoteSite site = f.site("bad", "javascript:alert(1)");
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(List.of(site)));
            f.open(); f.snapshot();
            f.click(20); f.owner();
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
            verify(f.player).sendMessage(contains("no valid HTTP/HTTPS"));
            when(site.getPermissionToView()).thenReturn("test.visible");
            when(f.player.hasPermission("test.visible")).thenReturn(false);
            when(site.getVoteURL(false)).thenReturn("https://example.test");
            f.click(20); f.owner();
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
        }
    }

    @Test void disabledFeatureAndDeniedPermissionLeaveProductionInventoryUntouched() {
        try (Fixture f = new Fixture()) {
            Inventory production = f.top.get();
            f.config.set("Experimental.VoteGUIs.Enabled", false);
            f.open();
            assertTrue(f.storage.isEmpty());
            assertSame(production, f.top.get());
            f.config.set("Experimental.VoteGUIs.Enabled", true);
            when(f.player.hasPermission(anyString())).thenReturn(false);
            f.open();
            assertTrue(f.storage.isEmpty());
            assertSame(production, f.top.get());
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void allEightDisabledCommandsHaveNoEffectOnExistingProductionInventoryOrUserData() {
        try (Fixture f = new Fixture()) {
            Inventory production = f.top.get();
            f.config.set("Experimental.VoteGUIs.Enabled", false);
            f.config.set("Experimental.HologramVoteGUI.Enabled", false);
            for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
                f.manager.open(f.player, type); f.owner();
                assertSame(production, f.top.get(), type.name());
            }
            verify(f.plugin, never()).getUser(any(UUID.class));
            verify(f.player, never()).openInventory(any(Inventory.class));
            verify(f.player, never()).closeInventory();
            assertTrue(f.storage.isEmpty());
        }
    }

    @Test void invalidHologramConfigurationDoesNotPreventInventoryOpening() {
        try (Fixture f = new Fixture()) {
            f.config.set("Experimental.HologramVoteGUI.Distance", 999);
            f.open(); f.snapshot();
            assertEquals(1, f.created.size());
            assertSame(f.created.getFirst(), f.top.get());
        }
    }

    @Test void reloadRetiresPendingSnapshotAndAnimationCannotReopenIt() {
        try (Fixture f = new Fixture()) {
            f.open();
            f.manager.clear(); f.owner();
            f.snapshot();
            f.watches.getFirst().run(); f.owner();
            assertTrue(f.created.isEmpty());
            f.open(); f.snapshot();
            f.manager.clear(); f.owner();
            verify(f.player).closeInventory();
        }
    }

    @Test void radialUsesStableSiteKeysAndNativeTargetsWithoutOpeningInventory() {
        AtomicReference<List<ExperimentalDisplays.Target>> targets = new AtomicReference<>();
        AtomicReference<java.util.function.Consumer<ExperimentalDisplays.Target>> selection = new AtomicReference<>();
        try (var renderers = mockConstruction(ExperimentalDisplays.class, (renderer, context) -> {
            doAnswer(inv -> { targets.set(inv.getArgument(3)); selection.set(inv.getArgument(4)); return null; })
                    .when(renderer).render(any(), any(), any(), anyList(), any());
        }); Fixture f = new Fixture()) {
            List<VoteSite> sites = new ArrayList<>();
            for (int i = 0; i < 6; i++) sites.add(f.site("site" + i, "https://example.test/" + i));
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(sites));
            Inventory production = f.top.get();
            f.manager.open(f.player, ExperimentalGUIType.RADIAL); f.owner(); f.snapshot();
            assertSame(production, f.top.get());
            assertEquals(9, targets.get().size());
            assertEquals(5, targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.SITE).count());
            var next = targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.NEXT).findFirst().orElseThrow();
            selection.get().accept(next);
            var site = targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.SITE).findFirst().orElseThrow();
            assertEquals("site5", site.id());
            when(sites.get(5).getVoteURL(false)).thenReturn("https://example.test/new");
            selection.get().accept(site);
            var captor = org.mockito.ArgumentCaptor.forClass(BaseComponent.class);
            verify(f.chat).sendMessage(captor.capture());
            assertEquals("https://example.test/new", captor.getValue().getClickEvent().getValue());
            verify(f.player, never()).openInventory(any(Inventory.class));
        }
    }

    @Test void terminalSelectsOnlyAnEnabledStyleAndRemovalNeverClosesProductionInventory() {
        AtomicReference<List<ExperimentalDisplays.Target>> targets = new AtomicReference<>();
        AtomicReference<java.util.function.Consumer<ExperimentalDisplays.Target>> selection = new AtomicReference<>();
        try (var renderers = mockConstruction(ExperimentalDisplays.class, (renderer, context) -> {
            doAnswer(inv -> { targets.set(inv.getArgument(3)); selection.set(inv.getArgument(4)); return null; })
                    .when(renderer).render(any(), any(), any(), anyList(), any());
        }); Fixture f = new Fixture()) {
            Inventory production = f.top.get();
            f.manager.terminalControl(f.player, "create"); f.owner(); f.snapshot();
            assertEquals(9, targets.get().size());
            assertSame(production, f.top.get());
            var staleSelection = selection.get();
            var inventory = targets.get().stream().filter(t -> t.id().equals("ANIMATED_INVENTORY")).findFirst().orElseThrow();
            f.config.set("Experimental.VoteGUIs.AnimatedInventory.Enabled", false);
            selection.get().accept(inventory); f.owner();
            assertTrue(f.storage.isEmpty());
            assertSame(production, f.top.get());
            f.manager.terminalControl(f.player, "inspect"); f.owner();
            verify(f.player).sendMessage(contains("private, nonpersistent"));
            f.manager.terminalControl(f.player, "remove"); f.owner();
            staleSelection.accept(inventory); f.owner();
            assertTrue(f.storage.isEmpty());
            verify(f.player, never()).closeInventory();
            f.manager.terminalControl(f.player, "create"); f.owner(); f.snapshot();
            f.config.set("Experimental.VoteGUIs.AnimatedInventory.Enabled", true);
            selection.get().accept(inventory); f.owner(); f.snapshot();
            assertTrue(f.top.get().getHolder() instanceof ExperimentalInventory);
        }
    }

    @Test void terminalManagementCannotRemoveAnotherExperimentalStyle() {
        try (Fixture f = new Fixture()) {
            f.open(); f.snapshot();
            Inventory active = f.top.get();
            f.manager.terminalControl(f.player, "remove"); f.owner();
            assertSame(active, f.top.get());
            verify(f.player).sendMessage(contains("No active temporary"));
            verify(f.player, never()).closeInventory();
        }
    }

    @Test void showcaseNavigatesActualRewardsAndSitePagesWithoutExecutingRewards() {
        AtomicReference<List<ExperimentalDisplays.Target>> targets = new AtomicReference<>();
        AtomicReference<java.util.function.Consumer<ExperimentalDisplays.Target>> selection = new AtomicReference<>();
        try (var renderers = mockConstruction(ExperimentalDisplays.class, (renderer, context) -> {
            doAnswer(inv -> { targets.set(inv.getArgument(3)); selection.set(inv.getArgument(4)); return null; })
                    .when(renderer).render(any(), any(), any(), anyList(), any());
        }); Fixture f = new Fixture()) {
            var handler = f.plugin.getVoteStreakHandler();
            var first = new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition(
                    "DailyThree", com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType.DAILY, true, 3, 1, 0, 0, false);
            var second = new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition(
                    "DailyFive", com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType.DAILY, true, 5, 1, 0, 0, false);
            when(handler.getDefinitions()).thenReturn(List.of(first, second));
            when(handler.getStoredPreview(f.user, first)).thenReturn(
                    new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler.StoredPreview(2, 4, false));
            when(handler.getStoredPreview(f.user, second)).thenReturn(
                    new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler.StoredPreview(4, 4, false));
            when(f.plugin.getSpecialRewardsConfig().getData()).thenReturn(new YamlConfiguration());
            List<VoteSite> sites = new ArrayList<>();
            for (int i = 0; i < 6; i++) sites.add(f.site("site" + i, "https://example.test/" + i));
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(sites));
            Inventory production = f.top.get();
            f.manager.open(f.player, ExperimentalGUIType.REWARD_SHOWCASE); f.owner(); f.snapshot();
            assertSame(production, f.top.get());
            assertEquals(10, targets.get().size());
            assertTrue(targets.get().getFirst().text().contains("DailyThree"));
            assertTrue(targets.get().getFirst().text().contains("2/3"));
            selection.get().accept(targets.get().getFirst()); // Preview has no reward action.
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
            selection.get().accept(targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.NEXT_REWARD).findFirst().orElseThrow());
            assertTrue(targets.get().getFirst().text().contains("DailyFive"));
            selection.get().accept(targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.SITE_PAGE).findFirst().orElseThrow());
            var site = targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.SITE).findFirst().orElseThrow();
            assertEquals("site5", site.id());
            selection.get().accept(site);
            verify(f.chat).sendMessage(any(BaseComponent.class));
            f.watches.getFirst().run(); f.owner();
            verify(renderers.constructed().getFirst()).rotateShowcase(any(), eq(1L));
            verify(handler).getDefinitions();
            verify(handler).getStoredPreview(f.user, first);
            verify(handler).getStoredPreview(f.user, second);
            verifyNoMoreInteractions(handler);
            verify(f.player, never()).openInventory(any(Inventory.class));
        }
    }

    @Test void emptyShowcaseRemainsAnIndependentCloseablePreview() {
        AtomicReference<List<ExperimentalDisplays.Target>> targets = new AtomicReference<>();
        AtomicReference<java.util.function.Consumer<ExperimentalDisplays.Target>> selection = new AtomicReference<>();
        try (var renderers = mockConstruction(ExperimentalDisplays.class, (renderer, context) -> {
            doAnswer(inv -> { targets.set(inv.getArgument(3)); selection.set(inv.getArgument(4)); return null; })
                    .when(renderer).render(any(), any(), any(), anyList(), any());
        }); Fixture f = new Fixture()) {
            f.manager.open(f.player, ExperimentalGUIType.REWARD_SHOWCASE); f.owner(); f.snapshot();
            assertEquals(5, targets.get().size());
            assertTrue(targets.get().getFirst().text().contains("No enabled streak rewards"));
            selection.get().accept(targets.get().stream().filter(t -> t.action() == ExperimentalDisplays.Action.CLOSE).findFirst().orElseThrow());
            int renders = org.mockito.Mockito.mockingDetails(renderers.constructed().getFirst()).getInvocations().size();
            f.watches.getFirst().run(); f.owner();
            assertEquals(renders, org.mockito.Mockito.mockingDetails(renderers.constructed().getFirst()).getInvocations().size());
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
        }
    }

    @Test void dialogUsesValidatedNativeUrlButtonsAndItsOwnNavigation() {
        AtomicReference<List<ExperimentalDialogs.Button>> buttons = new AtomicReference<>();
        AtomicReference<java.util.function.Consumer<String>> selection = new AtomicReference<>();
        try (var renderers = mockConstruction(ExperimentalDialogs.class, (renderer, context) -> {
            doAnswer(inv -> { buttons.set(inv.getArgument(4)); selection.set(inv.getArgument(5)); return null; })
                    .when(renderer).render(any(), any(), anyString(), anyString(), anyList(), any());
        }); Fixture f = new Fixture()) {
            List<VoteSite> sites = new ArrayList<>();
            for (int i = 0; i < 6; i++) sites.add(f.site("site" + i, i == 5 ? "invalid" : "https://example.test/" + i));
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(sites));
            f.manager.open(f.player, ExperimentalGUIType.NATIVE_DIALOG); f.owner(); f.snapshot();
            assertEquals(8, buttons.get().size());
            assertEquals("https://example.test/0", buttons.get().getFirst().url());
            selection.get().accept("next");
            assertEquals(4, buttons.get().size());
            assertNull(buttons.get().getFirst().url());
            assertEquals("invalid:site5", buttons.get().getFirst().action());
            verify(f.player, never()).openInventory(any(Inventory.class));
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
        }
    }

    @Test void npcSelectionOpensOnlyItsCurrentExperimentalInventoryAfterSnapshot() {
        AtomicReference<Runnable> selection = new AtomicReference<>();
        try (var npcs = mockConstruction(ExperimentalNPC.class, (npc, context) -> {
            doAnswer(inv -> { selection.set(inv.getArgument(4)); return null; })
                    .when(npc).open(any(), any(), any(), anyString(), any());
        }); Fixture f = new Fixture()) {
            Inventory production = f.top.get();
            f.manager.open(f.player, ExperimentalGUIType.NPC); f.owner();
            assertNull(selection.get());
            verify(f.plugin, never()).getUser(f.id);
            f.snapshot();
            assertSame(production, f.top.get());
            assertNotNull(selection.get());
            selection.get().run();
            assertTrue(f.top.get().getHolder() instanceof ExperimentalInventory);
            Runnable oldSelection = selection.get();
            f.open(); f.snapshot();
            Inventory successor = f.top.get();
            oldSelection.run();
            assertSame(successor, f.top.get());
            f.manager.clear(); f.owner();
            verify(npcs.constructed().getFirst()).retryCleanup();
        }
    }

    @Test void streakTrackShowsExistingDefinitionAndOnlySiteShortcutSendsLink() {
        try (Fixture f = new Fixture()) {
            var definition = new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition(
                    "DailyThree", com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType.DAILY,
                    true, 3, 1, 0, 0, false);
            var handler = f.plugin.getVoteStreakHandler();
            when(handler.getDefinitions()).thenReturn(List.of(definition));
            when(handler.getStoredPreview(f.user, definition)).thenReturn(
                    new com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler.StoredPreview(2, 5, false));
            when(f.plugin.getSpecialRewardsConfig().getData()).thenReturn(new YamlConfiguration());
            VoteSite site = f.site("first", "https://example.test/first");
            when(f.plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(new ArrayList<>(List.of(site)));
            f.manager.open(f.player, ExperimentalGUIType.STREAK_TRACK); f.owner(); f.snapshot();
            verify(f.top.get()).setItem(eq(20), any());
            f.click(20); f.owner(); // Preview has no claiming action.
            verify(f.chat, never()).sendMessage(any(BaseComponent.class));
            f.click(29); f.owner();
            verify(f.chat).sendMessage(any(BaseComponent.class));
            verify(handler).getDefinitions();
            verify(handler).getStoredPreview(f.user, definition);
            verifyNoMoreInteractions(handler);
        }
    }
}
