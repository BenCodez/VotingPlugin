package com.bencodez.votingplugin.signs;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.util.ArrayDeque;
import java.util.LinkedHashMap;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import org.bukkit.Location;
import org.bukkit.block.BlockState;
import org.bukkit.block.Sign;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.advancedcore.api.user.presence.PlayerPresenceService;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.topvoter.TopVoterDisplayFilter;

class SignLeaderboardCompletionTest {
    static final class Fixture implements AutoCloseable {
        final ArrayDeque<Runnable> worker = new ArrayDeque<>(), platform = new ArrayDeque<>();
        final AtomicLong clock = new AtomicLong(), generation = new AtomicLong();
        final TopVoterPlayer never = new TopVoterPlayer(new UUID(0, 1), "SyntheticNever", 1L);
        final TopVoterPlayer joined = new TopVoterPlayer(new UUID(0, 2), "SyntheticJoined", 1L);
        final LinkedHashMap<TopVoterPlayer, Integer> raw = new LinkedHashMap<>();
        final PlayerPresenceService service = new PlayerPresenceService(worker::add,
                id -> id.equals(never.getUuid()) ? PlayerPresence.NEVER_JOINED : PlayerPresence.JOINED);
        final TopVoterDisplayFilter filter = new TopVoterDisplayFilter(row -> service.checkAsync(row.getUuid()), clock::get);
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final Config config = mock(Config.class);
        final BukkitScheduler scheduler = mock(BukkitScheduler.class);
        final Location location = mock(Location.class, RETURNS_DEEP_STUBS);
        final Location skull = mock(Location.class, RETURNS_DEEP_STUBS);
        final Sign sign = mock(Sign.class);
        final SignHandler handler;
        Fixture() {
            raw.put(never, 100); raw.put(joined, 90);
            when(plugin.isEnabled()).thenReturn(true); when(plugin.getConfigFile()).thenReturn(config);
            when(config.isHideTopVoterIfNotPlayedBefore()).thenReturn(true);
            when(config.getFormatSignTopVoterSignLine1()).thenReturn("%position%:%player%");
            when(config.getFormatSignTopVoterSignLine2()).thenReturn("%votes%");
            when(config.getFormatSignTopVoterSignLine3()).thenReturn("%SiteName%");
            when(config.getFormatSignTopVoterSignLine4()).thenReturn("");
            when(plugin.getTopVoterDisplayGeneration()).thenAnswer(call -> generation.get());
            when(plugin.getTopVoterDisplay(any())).thenAnswer(call -> filter.project(raw, config.isHideTopVoterIfNotPlayedBefore()));
            when(plugin.getTopVoterDisplayAsync(any())).thenAnswer(call -> filter.projectAsync(raw, config.isHideTopVoterIfNotPlayedBefore()));
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            when(location.clone()).thenReturn(location); when(location.getBlock().getState()).thenReturn(sign);
            when(skull.clone()).thenReturn(skull);
            when(skull.getBlock().getState()).thenReturn(mock(BlockState.class));
            doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                    .when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(skull));
            doAnswer(call -> { call.getArgument(1, Runnable.class).run(); return null; })
                    .when(scheduler).runTask(eq(plugin), any(Runnable.class), eq(location));
            doAnswer(call -> { platform.add(call.getArgument(1)); return null; })
                    .when(scheduler).runTaskLater(eq(plugin), any(Runnable.class), anyLong(), eq(location));
            handler = new SignHandler(plugin, "synthetic", location, skull, "monthly", 1);
        }
        void refresh() { handler.updateLines(); handler.updateSign(0); }
        void drain() {
            int bound = 50;
            while (!worker.isEmpty() || !platform.isEmpty()) {
                assertTrue(bound-- > 0, "Completion cannot retry indefinitely");
                if (!worker.isEmpty()) worker.remove().run(); else platform.remove().run();
            }
        }
        public void close() { filter.close(); service.close(); }
    }
    @Test void signOnlyRefreshCompletesAfterLookupEvenWhenRefreshIntervalExceedsEvidenceTtl() {
        try (var f = new Fixture()) {
            for (int round = 0; round < 2; round++) {
                f.clock.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(round * 61L));
                f.refresh(); f.drain();
                verify(f.sign).setLine(0, "1:SyntheticJoined");
                verify(f.sign).setLine(1, "90");
                assertEquals(2, f.raw.size()); clearInvocations(f.sign);
            }
        }
    }
    @Test void repeatedPendingSignRefreshesCoalesceAndOnlyOwningSchedulerWrites() {
        try (var f = new Fixture()) {
            for (int count = 0; count < 1000; count++) f.handler.updateSign(0);
            assertEquals(2, f.worker.size()); assertTrue(f.platform.isEmpty());
            while (!f.worker.isEmpty()) f.worker.remove().run();
            assertEquals(1, f.platform.size()); verify(f.sign, never()).setLine(anyInt(), anyString());
            f.platform.remove().run(); verify(f.sign).setLine(0, "1:SyntheticJoined");
            verify(f.plugin, times(1)).getTopVoterDisplayAsync(TopVoter.Monthly);
        }
    }
    @Test void joinAfterPreparationBeforePlatformPublicationRevokesOmission() {
        try (var f = new Fixture()) {
            f.refresh(); while (!f.worker.isEmpty()) f.worker.remove().run();
            f.filter.joined(f.never.getUuid(), f.never.getPlayerName());
            f.platform.remove().run(); verify(f.sign).setLine(0, "1:SyntheticNever");
            assertEquals(2, f.raw.size());
        }
    }
    @Test void reloadBeforePlatformPublicationDiscardsOldGeneration() {
        try (var f = new Fixture()) {
            f.refresh(); while (!f.worker.isEmpty()) f.worker.remove().run();
            f.generation.incrementAndGet(); f.platform.remove().run();
            verify(f.sign, never()).setLine(anyInt(), anyString());
            f.refresh(); f.drain(); verify(f.sign).setLine(0, "1:SyntheticJoined");
        }
    }
    @Test void disableBeforePublicationDropsOldWorkAndAllowsEnabledRetry() {
        try (var f = new Fixture()) {
            f.refresh(); while (!f.worker.isEmpty()) f.worker.remove().run();
            when(f.plugin.isEnabled()).thenReturn(false); f.platform.remove().run();
            verify(f.sign, never()).setLine(anyInt(), anyString());
            when(f.plugin.isEnabled()).thenReturn(true); f.refresh(); f.drain();
            verify(f.sign).setLine(0, "1:SyntheticJoined");
        }
    }
    @Test void oldPendingCompletionCannotClearNewGenerationRequest() {
        try (var f = new Fixture()) {
            f.handler.updateSign(0); f.generation.incrementAndGet(); f.handler.updateSign(0);
            f.drain(); verify(f.sign).setLine(0, "1:SyntheticJoined");
            verify(f.plugin, times(2)).getTopVoterDisplayAsync(TopVoter.Monthly);
        }
    }
    @Test void rejectedPlatformDispatchReleasesPreparationForRetry() {
        try (var f = new Fixture()) {
            doThrow(new java.util.concurrent.RejectedExecutionException("synthetic"))
                    .when(f.scheduler).runTaskLater(eq(f.plugin), any(Runnable.class), anyLong(), eq(f.location));
            f.refresh(); f.drain(); verify(f.sign, never()).setLine(anyInt(), anyString());
            doAnswer(call -> { f.platform.add(call.getArgument(1)); return null; })
                    .when(f.scheduler).runTaskLater(eq(f.plugin), any(Runnable.class), anyLong(), eq(f.location));
            f.refresh(); f.drain(); verify(f.sign).setLine(0, "1:SyntheticJoined");
        }
    }
    @Test void disabledOptionPreservesLegacyVisibleRowsWithoutAsyncPreparation() {
        try (var f = new Fixture()) {
            when(f.config.isHideTopVoterIfNotPlayedBefore()).thenReturn(false);
            f.refresh(); f.drain(); verify(f.sign).setLine(0, "1:SyntheticNever");
            assertTrue(f.worker.isEmpty()); verify(f.plugin, never()).getTopVoterDisplayAsync(any());
        }
    }
    @Test void changedSignPeriodDiscardsQueuedOldRenderAndNextRefreshUsesNewPeriod() {
        try (var f = new Fixture()) {
            f.refresh(); while (!f.worker.isEmpty()) f.worker.remove().run();
            f.handler.setData("weekly"); f.platform.remove().run();
            verify(f.sign, never()).setLine(anyInt(), anyString());
            f.refresh(); f.drain(); verify(f.sign).setLine(0, "1:SyntheticJoined");
            verify(f.sign).setLine(2, "weekly");
        }
    }

    @Test void removingTheLastVisibleSignRowClearsItsPreviousSkullOnOwnerScheduler() {
        try (var f = new Fixture()) {
            var head = mock(org.bukkit.block.Skull.class);
            when(f.skull.getBlock().getState()).thenReturn(head);
            // Existing public display can have a populated skull before this row is certified absent.
            f.raw.remove(f.joined); f.handler.updateLines(); f.handler.updateSign(0);
            while (!f.worker.isEmpty()) f.worker.remove().run();
            verify(head, never()).setOwnerProfile(any());
            f.platform.remove().run();
            verify(f.sign).setLine(0,"1:No Player");
            verify(head).setOwnerProfile(null);verify(head).update(true,false);
        }
    }

    @Test void defaultFalseEmptySignPreservesLegacySkullBehavior() {
        try(var f=new Fixture()) {
            var head=mock(org.bukkit.block.Skull.class);when(f.skull.getBlock().getState()).thenReturn(head);
            when(f.config.isHideTopVoterIfNotPlayedBefore()).thenReturn(false);f.raw.clear();
            f.refresh();f.drain();verify(f.sign).setLine(0,"1:No Player");verifyNoInteractions(head);
        }
    }
    @Test void visibleSignRowStillPublishesItsCurrentSkullOwner() {
        try(var f=new Fixture()) {
            var head=mock(org.bukkit.block.Skull.class);when(f.skull.getBlock().getState()).thenReturn(head);
            f.refresh();f.drain();verify(head).setOwner("SyntheticJoined");
            verify(head,never()).setOwnerProfile(null);verify(head).update(true,false);
        }
    }

    @Test void explicitSkullIsAccessedOnlyThroughItsOwnLocationScheduler() {
        try(var f=new Fixture()) {
            var owner=new java.util.concurrent.atomic.AtomicBoolean();var skullTasks=new ArrayDeque<Runnable>();
            var head=mock(org.bukkit.block.Skull.class);
            when(f.skull.getBlock().getState()).thenAnswer(call->{assertTrue(owner.get(),"Skull access outside its owner");return head;});
            when(f.skull.clone()).thenReturn(f.skull);
            doAnswer(call->{skullTasks.add(()->{owner.set(true);try{call.getArgument(1,Runnable.class).run();}finally{owner.set(false);}});return null;})
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),eq(f.skull));
            f.raw.remove(f.joined);f.refresh();f.drain();verifyNoInteractions(head);
            assertEquals(1,skullTasks.size());skullTasks.remove().run();verify(head).setOwnerProfile(null);
        }
    }

    @Test void queuedSkullFramesDropRetiredChangedTargetsAndSupersededRenders() {
        for(String change:java.util.List.of("generation","disabled","target","period","successor"))try(var f=new Fixture()) {
            var pending=new ArrayDeque<Runnable>();var head=mock(org.bukkit.block.Skull.class);
            when(f.skull.getBlock().getState()).thenReturn(head);
            doAnswer(call->{pending.add(call.getArgument(1));return null;})
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),eq(f.skull));
            f.raw.remove(f.joined);f.refresh();f.drain();assertEquals(1,pending.size());
            switch(change) {
                case "generation" -> f.generation.incrementAndGet();
                case "disabled" -> when(f.plugin.isEnabled()).thenReturn(false);
                case "target" -> f.handler.setSkullLocation(mock(Location.class));
                case "period" -> f.handler.setData("weekly");
                case "successor" -> {f.handler.checkSkulls();assertEquals(2,pending.size());}
            }
            pending.remove().run();verifyNoInteractions(head);
            if(change.equals("successor")){pending.remove().run();verify(head).setOwnerProfile(null);}
        }
    }
    @Test void joinWhileSkullTaskWaitsUsesCurrentPlayer() {
        try(var f=new Fixture()) {
            var pending=new ArrayDeque<Runnable>();var head=mock(org.bukkit.block.Skull.class);
            when(f.skull.getBlock().getState()).thenReturn(head);
            doAnswer(call->{pending.add(call.getArgument(1));return null;})
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),eq(f.skull));
            f.refresh();f.drain();f.filter.joined(f.never.getUuid(),f.never.getPlayerName());
            pending.remove().run();verify(head).setOwner("SyntheticNever");
        }
    }
    @Test void rejectedSkullDispatchAllowsLaterRefresh() {
        try(var f=new Fixture()) {
            var head=mock(org.bukkit.block.Skull.class);when(f.skull.getBlock().getState()).thenReturn(head);
            doThrow(new java.util.concurrent.RejectedExecutionException("synthetic"))
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),eq(f.skull));
            f.raw.remove(f.joined);f.refresh();f.drain();verifyNoInteractions(head);
            doAnswer(call->{call.getArgument(1,Runnable.class).run();return null;})
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),eq(f.skull));
            f.refresh();f.drain();verify(head).setOwnerProfile(null);
        }
    }

    @Test void preferredAndNearbySkullsUseEachBlockOwnerAcrossRegionEdges() {
        for(boolean preferred:java.util.List.of(true,false))try(var f=new Fixture()) {
            var world=mock(org.bukkit.World.class);var origin=new Location(world,15,64,15);
            var headLocation=preferred?new Location(world,16,65,15):new Location(world,14,63,14);
            var head=mock(org.bukkit.block.Skull.class);var direction=mock(org.bukkit.block.data.Directional.class);
            when(direction.getFacing()).thenReturn(org.bukkit.block.BlockFace.EAST);
            when(f.sign.getBlockData()).thenReturn(direction);
            var owner=new java.util.concurrent.atomic.AtomicReference<Location>();
            var tasks=new ArrayDeque<Runnable>();var visited=new java.util.HashSet<Location>();
            when(world.getBlockAt(any(Location.class))).thenAnswer(call->{
                var target=call.getArgument(0,Location.class);return world.getBlockAt(target.getBlockX(),target.getBlockY(),target.getBlockZ());
            });
            when(world.getBlockAt(anyInt(),anyInt(),anyInt())).thenAnswer(call->{
                var target=new Location(world,call.getArgument(0,Integer.class),call.getArgument(1,Integer.class),call.getArgument(2,Integer.class));
                assertEquals(target,owner.get(),"Block access outside its owner");visited.add(target);
                var block=mock(org.bukkit.block.Block.class);
                when(block.getState()).thenAnswer(ignored->{assertEquals(target,owner.get());return target.equals(origin)?f.sign:target.equals(headLocation)?head:mock(BlockState.class);});
                return block;
            });
            doAnswer(call->{var target=call.getArgument(2,Location.class);var action=call.getArgument(1,Runnable.class);
                tasks.add(()->{owner.set(target);try{action.run();}finally{owner.set(null);}});return null;})
                    .when(f.scheduler).runTask(eq(f.plugin),any(Runnable.class),any(Location.class));
            f.handler.setLocation(origin);f.handler.setSkullLocation(null);f.raw.clear();
            f.handler.updateLines();f.handler.checkSkulls();int bound=40;
            while(!tasks.isEmpty()){assertTrue(bound-->0);tasks.remove().run();}
            verify(head).setOwnerProfile(null);verify(head).update(true,false);
            if(preferred)assertEquals(java.util.Set.of(origin,headLocation),visited);
            else assertTrue(visited.size()>=27);
        }
    }

}
