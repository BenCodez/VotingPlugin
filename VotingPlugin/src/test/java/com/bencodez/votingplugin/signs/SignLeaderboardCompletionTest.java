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
    @Test void nonSkullTargetDoesNotProjectTheWholeLeaderboard() {
        try(var f=new Fixture()) {
            clearInvocations(f.plugin);f.handler.checkSkulls();
            verify(f.plugin,never()).getTopVoterDisplay(any());
        }
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
            when(head.setOwner(anyString())).thenReturn(true);
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

    static final class RegionalSkulls implements AutoCloseable {
        final Fixture f = new Fixture();
        final org.bukkit.World world = mock(org.bukkit.World.class);
        final Location origin = new Location(world, 15, 64, 15);
        final Location target;
        final Location neighbor = new Location(world, 16, 64, 14);
        final org.bukkit.block.Skull head = mock(org.bukkit.block.Skull.class);
        final org.bukkit.block.Skull unrelated = mock(org.bukkit.block.Skull.class);
        final java.util.concurrent.atomic.AtomicBoolean neighborPlaced = new java.util.concurrent.atomic.AtomicBoolean();
        final java.util.concurrent.atomic.AtomicReference<Location> owner = new java.util.concurrent.atomic.AtomicReference<>();
        final ArrayDeque<Runnable> tasks = new ArrayDeque<>();
        final java.util.Set<Location> visited = new java.util.HashSet<>();

        RegionalSkulls(boolean preferred) {
            target = preferred ? new Location(world, 16, 65, 15) : new Location(world, 14, 63, 14);
            var direction = mock(org.bukkit.block.data.Directional.class);
            when(direction.getFacing()).thenReturn(org.bukkit.block.BlockFace.EAST);
            when(f.sign.getBlockData()).thenReturn(direction);
            when(head.setOwner(anyString())).thenReturn(true);
            when(head.update(true, false)).thenReturn(true);
            when(world.getBlockAt(any(Location.class))).thenAnswer(call -> {
                var point = call.getArgument(0, Location.class);
                return world.getBlockAt(point.getBlockX(), point.getBlockY(), point.getBlockZ());
            });
            when(world.getBlockAt(anyInt(), anyInt(), anyInt())).thenAnswer(call -> {
                var point = new Location(world, call.getArgument(0, Integer.class),
                        call.getArgument(1, Integer.class), call.getArgument(2, Integer.class));
                assertEquals(point, owner.get(), "Block access outside its owner");
                visited.add(point);
                var block = mock(org.bukkit.block.Block.class);
                when(block.getState()).thenAnswer(ignored -> {
                    assertEquals(point, owner.get(), "Block state outside its owner");
                    return point.equals(origin) ? f.sign : point.equals(target) ? head
                            : neighborPlaced.get() && point.equals(neighbor) ? unrelated : mock(BlockState.class);
                });
                return block;
            });
            doAnswer(call -> {
                var point = call.getArgument(2, Location.class);
                var action = call.getArgument(1, Runnable.class);
                tasks.add(() -> { owner.set(point); try { action.run(); } finally { owner.set(null); } });
                return null;
            }).when(f.scheduler).runTask(eq(f.plugin), any(Runnable.class), any(Location.class));
            f.handler.setLocation(origin);
            f.handler.setSkullLocation(null);
            f.raw.remove(f.never);
        }
        org.bukkit.profile.PlayerProfile useProfile(boolean rejected) {
            return useProfile(rejected, false);
        }
        org.bukkit.profile.PlayerProfile useProfile(boolean rejected, boolean absent) {
            var cache = mock(com.bencodez.simpleapi.skull.SkullCacheHandler.class);
            var item = mock(org.bukkit.inventory.ItemStack.class);
            var meta = mock(org.bukkit.inventory.meta.SkullMeta.class);
            var profile = absent ? null : mock(org.bukkit.profile.PlayerProfile.class);
            when(meta.getOwnerProfile()).thenReturn(profile);
            when(item.getItemMeta()).thenReturn(meta);
            when(cache.getSkull(any(UUID.class), anyString())).thenReturn(item);
            when(f.plugin.getSkullCacheHandler()).thenReturn(cache);
            if (rejected) doThrow(new IllegalArgumentException("synthetic rejected profile"))
                    .when(head).setOwnerProfile(profile);
            return profile;
        }
        void refresh() { f.handler.updateLines(); f.handler.checkSkulls(); }
        void drain() {
            int bound = 60;
            while (!tasks.isEmpty()) { assertTrue(bound-- > 0, "Bounded regional discovery"); tasks.remove().run(); }
        }
        void paint() {
            refresh(); drain(); verify(head).setOwner("SyntheticJoined");
            clearInvocations(head); visited.clear();
        }
        void empty() { f.raw.clear(); refresh(); drain(); }
        public void close() { f.close(); }
    }

    @Test void unassignedEmptySignDoesNotClearPreferredOrNearbySkullOwners() {
        for (boolean preferred : java.util.List.of(true, false)) try (var r = new RegionalSkulls(preferred)) {
            r.neighborPlaced.set(true); r.empty();
            verifyNoInteractions(r.head, r.unrelated);
            assertEquals(java.util.Set.of(r.origin), r.visited);
        }
    }

    @Test void assignedAutomaticSkullClearsOnItsOwnerWithoutTouchingNewNeighbors() {
        for (boolean preferred : java.util.List.of(true, false)) try (var r = new RegionalSkulls(preferred)) {
            r.paint(); r.neighborPlaced.set(true); r.empty();
            verify(r.head).setOwnerProfile(null); verify(r.head).update(true, false);
            verifyNoInteractions(r.unrelated);
            assertEquals(java.util.Set.of(r.origin, r.target), r.visited);
        }
    }

    @Test void publicRangeRefreshDoesNotDiscoverSkullsForAnUnassignedEmptyRow() {
        try (var r = new RegionalSkulls(false)) {
            r.neighborPlaced.set(true); r.f.raw.clear(); r.f.handler.updateLines();
            r.f.handler.updateSkulls(r.origin.clone().add(-1, -1, -1), r.origin.clone().add(1, 1, 1));
            r.drain(); verifyNoInteractions(r.head, r.unrelated);
        }
    }

    @Test void queuedVisibleDiscoveryBecomingEmptyCannotClearUnassignedSkulls() {
        for (boolean preferred : java.util.List.of(true, false)) try (var r = new RegionalSkulls(preferred)) {
            r.neighborPlaced.set(true); r.refresh(); r.f.raw.clear(); r.drain();
            verifyNoInteractions(r.head, r.unrelated);
        }
    }

    @Test void automaticAssignmentsDoNotCrossDisplayGenerationOrPeriod() {
        for (String change : java.util.List.of("generation", "period")) try (var r = new RegionalSkulls(false)) {
            r.paint();
            if (change.equals("generation")) r.f.generation.incrementAndGet();
            else r.f.handler.setData("weekly");
            r.empty(); verifyNoInteractions(r.head);
        }
    }

    @Test void successfulClearRetiresAutomaticAssociationButFailedClearCanRetry() {
        for (boolean failedFirst : java.util.List.of(true, false)) try (var r = new RegionalSkulls(false)) {
            r.paint(); when(r.head.update(true, false)).thenReturn(!failedFirst);
            r.empty(); verify(r.head).setOwnerProfile(null); clearInvocations(r.head);
            when(r.head.update(true, false)).thenReturn(true);
            r.empty();
            if (failedFirst) verify(r.head).setOwnerProfile(null);
            else verifyNoInteractions(r.head);
        }
    }

    @Test void failedAutomaticDisplayDoesNotClaimTheExistingWorldSkull() {
        try (var r = new RegionalSkulls(false)) {
            when(r.head.update(true, false)).thenReturn(false); r.paint();
            r.empty(); verifyNoInteractions(r.head);
        }
    }

    @Test void failedFallbackAssignmentCannotClaimAnExistingAutomaticSkull() {
        for (boolean preferred : java.util.List.of(true, false)) try (var r = new RegionalSkulls(preferred)) {
            when(r.head.setOwner(anyString())).thenReturn(false);
            r.paint(); r.neighborPlaced.set(true); r.empty();
            verifyNoInteractions(r.head, r.unrelated);
            assertEquals(java.util.Set.of(r.origin), r.visited);
        }
    }

    @Test void fallbackAssociationRequiresBothAssignmentAndPublication() {
        for (boolean preferred : java.util.List.of(true, false))
            for (boolean assigned : java.util.List.of(true, false))
                for (boolean published : java.util.List.of(true, false))
                    try (var r = new RegionalSkulls(preferred)) {
                        when(r.head.setOwner(anyString())).thenReturn(assigned);
                        when(r.head.update(true, false)).thenReturn(published);
                        r.refresh(); r.drain(); verify(r.head).setOwner("SyntheticJoined");
                        if (!assigned) verify(r.head, never()).update(true, false);
                        clearInvocations(r.head); r.empty();
                        if (assigned && published) verify(r.head).setOwnerProfile(null);
                        else verifyNoInteractions(r.head);
                    }
    }

    @Test void profilePublicationUsesTheSameConfirmedAssociationRule() {
        for (boolean published : java.util.List.of(true, false)) try (var r = new RegionalSkulls(false)) {
            var profile = r.useProfile(false);
            when(r.head.update(true, false)).thenReturn(published);
            r.refresh(); r.drain(); verify(r.head).setOwnerProfile(profile);
            verify(r.head, never()).setOwner(anyString());
            clearInvocations(r.head); r.empty();
            if (published) verify(r.head).setOwnerProfile(null);
            else verifyNoInteractions(r.head);
        }
    }

    @Test void rejectedProfileUsesOnlySuccessfulFallbackAssociation() {
        for (boolean assigned : java.util.List.of(true, false))
            for (boolean published : java.util.List.of(true, false))
                try (var r = new RegionalSkulls(false)) {
                    var profile = r.useProfile(true);
                    when(r.head.setOwner(anyString())).thenReturn(assigned);
                    when(r.head.update(true, false)).thenReturn(published);
                    r.refresh(); r.drain(); verify(r.head).setOwnerProfile(profile);
                    verify(r.head).setOwner("SyntheticJoined");
                    clearInvocations(r.head); r.empty();
                    if (assigned && published) verify(r.head).setOwnerProfile(null);
                    else verifyNoInteractions(r.head);
                }
    }

    @Test void failedFallbackCanRetryBeforeCreatingAnAutomaticAssociation() {
        try (var r = new RegionalSkulls(false)) {
            when(r.head.setOwner(anyString())).thenReturn(false);
            r.refresh(); r.drain(); clearInvocations(r.head);
            when(r.head.setOwner(anyString())).thenReturn(true);
            r.paint(); r.empty(); verify(r.head).setOwnerProfile(null);
        }
    }

    @Test void retiredFrameCannotPublishAfterOwnerAssignmentCompletes() {
        for (boolean profilePath : java.util.List.of(true, false))
            for (boolean preferred : java.util.List.of(true, false))
                try (var r = new RegionalSkulls(preferred)) {
                    if (profilePath) {
                        var profile = r.useProfile(false);
                        doAnswer(call -> { r.f.generation.incrementAndGet(); return null; })
                                .when(r.head).setOwnerProfile(profile);
                    } else {
                        when(r.head.setOwner(anyString())).thenAnswer(call -> {
                            r.f.generation.incrementAndGet(); return true;
                        });
                    }
                    r.refresh(); r.drain(); verify(r.head, never()).update(true, false);
                    clearInvocations(r.head); r.empty(); verifyNoInteractions(r.head);
                }
    }

    @Test void absentCachedProfileUsesOnlyConfirmedFallbackPublication() {
        for (boolean preferred : java.util.List.of(true, false))
            for (boolean assigned : java.util.List.of(true, false))
                for (boolean published : java.util.List.of(true, false))
                    try (var r = new RegionalSkulls(preferred)) {
                        r.useProfile(false, true);
                        when(r.head.setOwner(anyString())).thenReturn(assigned);
                        when(r.head.update(true, false)).thenReturn(published);
                        r.refresh(); r.drain();
                        verify(r.head, never()).setOwnerProfile(null);
                        verify(r.head).setOwner("SyntheticJoined");
                        if (!assigned) verify(r.head, never()).update(true, false);
                        clearInvocations(r.head); r.empty();
                        if (assigned && published) verify(r.head).setOwnerProfile(null);
                        else verifyNoInteractions(r.head);
                    }
    }

    @Test void absentCachedProfileFailureCanRetryWithoutClearingTheExistingSkull() {
        try (var r = new RegionalSkulls(false)) {
            r.useProfile(false, true); when(r.head.setOwner(anyString())).thenReturn(false);
            r.refresh(); r.drain(); verify(r.head, never()).setOwnerProfile(null);
            verify(r.head, never()).update(true, false); clearInvocations(r.head);
            r.empty(); verifyNoInteractions(r.head);
            r.f.raw.put(r.f.joined, 20);
            when(r.head.setOwner(anyString())).thenReturn(true);
            r.paint(); r.empty(); verify(r.head).setOwnerProfile(null);
        }
    }

    @Test void retiredFrameCannotPublishAnEmptyRowClear() {
        for (boolean preferred : java.util.List.of(true, false))
            for (String scope : java.util.List.of("generation", "period", "target"))
                try (var r = new RegionalSkulls(preferred)) {
                    r.paint();
                    doAnswer(call -> {
                        if (scope.equals("generation")) r.f.generation.incrementAndGet();
                        else if (scope.equals("period")) r.f.handler.setData("weekly");
                        else r.f.handler.setSkullLocation(r.neighbor);
                        return null;
                    }).when(r.head).setOwnerProfile(null);
                    r.empty(); verify(r.head).setOwnerProfile(null);
                    verify(r.head, never()).update(true, false);
                    verifyNoInteractions(r.unrelated);
                }
    }

}
