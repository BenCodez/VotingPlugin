package com.bencodez.votingplugin.hologram;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import net.md_5.bungee.api.chat.TextComponent;
import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votesites.VoteSite;

class HologramVoteMenuClickTest {
    @Test void ownerClicksPageAndIntruderIsIgnoredWhileExpiredSessionCloses() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        YamlConfiguration config = new YamlConfiguration();
        config.set("Experimental.HologramVoteGUI.Enabled", true);
        config.set("Experimental.HologramVoteGUI.SitesPerPage", 5);
        when(plugin.getConfigFile().getData()).thenReturn(config);
        when(plugin.isEnabled()).thenReturn(true);
        Queue<Runnable> reads = new ArrayDeque<>();
        var timer = plugin.getUserManager().getDataManager().getTimer();
        doAnswer(inv -> { reads.add(inv.getArgument(0)); return null; })
                .when(timer).execute(any(Runnable.class));
        java.util.ArrayList<VoteSite> voteSites = new java.util.ArrayList<>();
        for (int i = 0; i < 7; i++) {
            VoteSite site = mock(VoteSite.class);
            when(site.isHidden()).thenReturn(false);
            when(site.getPermissionToView()).thenReturn("");
            when(site.getKey()).thenReturn("site" + i);
            when(site.getDisplayName()).thenReturn("Site " + i);
            when(site.getVoteURL(false)).thenReturn("https://example.test/" + i);
            voteSites.add(site);
        }
        when(plugin.getVoteSiteManager().getVoteSitesEnabled()).thenReturn(voteSites);
        when(plugin.getUser(any())).thenReturn(mock(com.bencodez.votingplugin.user.VotingPluginUser.class));

        HologramMenuScheduler scheduler = mock(HologramMenuScheduler.class);
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).player(any(), any(), any());
        doAnswer(inv -> { inv.getArgument(1, Runnable.class).run(); return null; }).when(scheduler).region(any(), any());
        AtomicReference<Runnable> watch = new AtomicReference<>();
        when(scheduler.watchPlayer(any(), any(), any())).thenAnswer(inv -> { watch.set(inv.getArgument(1)); return (Runnable) () -> { }; });
        when(scheduler.owns(any())).thenReturn(true);
        AtomicLong now = new AtomicLong(1_000_000_000L);
        HologramVoteMenu menu = new HologramVoteMenu(plugin, scheduler, now::get);
        Player owner = mock(Player.class), intruder = mock(Player.class);
        when(owner.getUniqueId()).thenReturn(java.util.UUID.randomUUID());
        when(owner.isOnline()).thenReturn(true);
        when(owner.hasPermission(anyString())).thenReturn(true);
        when(intruder.getUniqueId()).thenReturn(java.util.UUID.randomUUID());
        World world = mock(World.class);
        Location eye = new Location(world, 0, 64, 0);
        when(owner.getEyeLocation()).thenReturn(eye);
        when(owner.getWorld()).thenReturn(world);
        Entity siteEntity = mock(Entity.class), nextEntity = mock(Entity.class);
        when(siteEntity.getUniqueId()).thenReturn(java.util.UUID.randomUUID());
        when(nextEntity.getUniqueId()).thenReturn(java.util.UUID.randomUUID());
        AtomicReference<HologramVoteMenu.Session> rendered = new AtomicReference<>();
        AtomicReference<TextComponent> sent = new AtomicReference<>();
        Player.Spigot spigot = mock(Player.Spigot.class);
        when(owner.spigot()).thenReturn(spigot);
        doAnswer(inv -> { sent.set((TextComponent) inv.getArgument(0)); return null; }).when(spigot).sendMessage(any(TextComponent.class));

        try (MockedStatic<NativeHologramRenderer> renderer = mockStatic(NativeHologramRenderer.class)) {
            renderer.when(() -> NativeHologramRenderer.render(any(), any(), any())).thenAnswer(inv -> {
                HologramVoteMenu.Session session = inv.getArgument(0);
                rendered.set(session);
                NativeHologramRenderer.Spawned callback = inv.getArgument(2);
                callback.accept(siteEntity, HologramVoteMenu.Action.SITE, session.page * 5);
                callback.accept(nextEntity, HologramVoteMenu.Action.NEXT, -1);
                return null;
            });
            menu.open(owner);
            reads.remove().run();
            assertSame(owner.getUniqueId(), rendered.get().owner);
            PlayerInteractEntityEvent intruderClick = mock(PlayerInteractEntityEvent.class);
            when(intruderClick.getRightClicked()).thenReturn(siteEntity);
            when(intruderClick.getPlayer()).thenReturn(intruder);
            menu.interact(intruderClick);
            verify(intruderClick).setCancelled(true);
            verifyNoInteractions(spigot);

            PlayerInteractEntityEvent siteClick = mock(PlayerInteractEntityEvent.class);
            when(siteClick.getRightClicked()).thenReturn(siteEntity);
            when(siteClick.getPlayer()).thenReturn(owner);
            menu.interact(siteClick);
            now.addAndGet(250_000_000L);
            menu.interact(siteClick);
            verify(siteClick, atLeastOnce()).setCancelled(true);
            assertNotNull(sent.get());
            assertEquals("https://example.test/0", sent.get().getClickEvent().getValue());

            PlayerInteractEntityEvent nextClick = mock(PlayerInteractEntityEvent.class);
            when(nextClick.getRightClicked()).thenReturn(nextEntity);
            when(nextClick.getPlayer()).thenReturn(owner);
            now.addAndGet(250_000_000L);
            menu.interact(nextClick);
            assertEquals(1, rendered.get().page);
            now.addAndGet(250_000_000L);
            menu.interact(siteClick);
            assertEquals("https://example.test/5", sent.get().getClickEvent().getValue());
            now.addAndGet(61_000_000_000L);
            watch.get().run();
            assertTrue(rendered.get().closed.get());
        }
    }
}
