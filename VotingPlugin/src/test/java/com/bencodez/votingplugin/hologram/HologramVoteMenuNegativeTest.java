package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.UUID;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;

class HologramVoteMenuNegativeTest {
    @Test void disabledConfigurationStopsBeforeStorageOrRendering() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        when(plugin.getConfigFile().getData()).thenReturn(config);
        when(plugin.isEnabled()).thenReturn(true);
        Player player = mock(Player.class);
        when(player.isOnline()).thenReturn(true);
        when(player.hasPermission(anyString())).thenReturn(true);
        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).player(any(), any(), any());
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler);
        menu.open(player);
        verify(player).sendMessage(contains("Enable Experimental.HologramVoteGUI.Enabled"));
        verify(plugin.getUserManager().getDataManager().getTimer(), never()).execute(any());
    }

    @Test void unsupportedPlatformStopsWithoutQueueingWork() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        config.set("Experimental.HologramVoteGUI.Enabled", true);
        when(plugin.getConfigFile().getData()).thenReturn(config);
        when(plugin.isEnabled()).thenReturn(true);
        Player player = mock(Player.class);
        when(player.isOnline()).thenReturn(true);
        when(player.hasPermission(anyString())).thenReturn(true);
        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).player(any(), any(), any());
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler);
        try (MockedStatic<HologramVoteMenu> api = mockStatic(HologramVoteMenu.class, CALLS_REAL_METHODS)) {
            api.when(HologramVoteMenu::supported).thenReturn(false);
            menu.open(player);
        }
        verify(player).sendMessage(contains("requires Minecraft 1.19.4"));
        verify(plugin.getUserManager().getDataManager().getTimer(), never()).execute(any());
    }

    @Test void invalidUrlClickReportsErrorWithoutOpeningExternalLink() {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).player(any(), any(), any());
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).region(any(), any());
        when(scheduler.owns(any())).thenReturn(true);
        Player player = mock(Player.class);
        UUID owner = UUID.randomUUID();
        when(player.getUniqueId()).thenReturn(owner);
        when(player.hasPermission(anyString())).thenReturn(true);
        World world = mock(World.class);
        Location anchor = new Location(world, 0, 64, 0);
        when(player.getWorld()).thenReturn(world);
        when(player.getEyeLocation()).thenReturn(anchor);
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler, () -> 1_000_000_000L);
        HologramVoteMenu.Session session = new HologramVoteMenu.Session(player, anchor,
                new HologramVoteSettings(true, 2.5, 60, 5), 1_000_000_000L);
        session.sites = java.util.List.of(new HologramVoteModel.Site("bad", "Bad", "javascript:bad", true, 0));
        session.version = 1;
        Entity entity = mock(Entity.class);
        when(entity.getUniqueId()).thenReturn(UUID.randomUUID());
        FieldAccess.put(menu, "sessions", owner, session);
        FieldAccess.put(menu, "buttons", entity.getUniqueId(), new HologramVoteMenu.Button(session, 1, HologramVoteMenu.Action.SITE, 0));
        PlayerInteractEntityEvent event = mock(PlayerInteractEntityEvent.class);
        when(event.getRightClicked()).thenReturn(entity);
        when(event.getPlayer()).thenReturn(player);
        menu.interact(event);
        verify(player).sendMessage(contains("no valid HTTP/HTTPS"));
        verify(player, never()).spigot();
    }

    private static final class FieldAccess {
        @SuppressWarnings("unchecked") static <K, V> void put(Object target, String name, K key, V value) {
            try {
                var field = target.getClass().getDeclaredField(name);
                field.setAccessible(true);
                ((java.util.concurrent.ConcurrentHashMap<K, V>) field.get(target)).put(key, value);
            } catch (ReflectiveOperationException failure) { throw new AssertionError(failure); }
        }
    }
}
