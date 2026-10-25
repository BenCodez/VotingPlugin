package com.bencodez.votingplugin.tests.topvoter;

import com.bencodez.votingplugin.topvoter.*;

import static org.junit.jupiter.api.Assertions.*;
import java.util.LinkedHashMap;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.advancedcore.api.user.presence.PlayerPresenceService;

class TopVoterDisplayFilterTest {
    private TopVoterPlayer row(int id) { return new TopVoterPlayer(new UUID(0, id), "synthetic-" + id, 1L); }
    private LinkedHashMap<TopVoterPlayer, Integer> board(TopVoterPlayer... rows) {
        var board = new LinkedHashMap<TopVoterPlayer, Integer>();
        for (int index = 0; index < rows.length; index++) board.put(rows[index], 100 - index);
        return board;
    }

    @Test void disabledDefaultNeverRequestsEvidenceOrChangesRanksAndTotals() {
        var first = row(1); var second = row(2); var raw = board(first, second);
        try (var filter = new TopVoterDisplayFilter(player -> { fail("Disabled filtering must not inspect history"); return null; })) {
            assertEquals(raw, filter.project(raw, false));
            assertEquals(java.util.List.of(first, second), java.util.List.copyOf(raw.keySet()));
        }
    }

    @Test void onlyConfirmedNegativeIsExcludedAndRawAwardRankingIsUntouched() {
        var first = row(1); var second = row(2); var third = row(3); var raw = board(first, second, third);
        try (var filter = new TopVoterDisplayFilter(player -> CompletableFuture.completedStage(
                player.getUuid().equals(first.getUuid()) ? PlayerPresence.NEVER_JOINED
                        : player.getUuid().equals(second.getUuid()) ? PlayerPresence.JOINED : PlayerPresence.UNKNOWN))) {
            var visible = filter.project(raw, true);
            assertEquals(java.util.List.of(second, third), java.util.List.copyOf(visible.keySet()));
            assertEquals(99, visible.get(second)); assertEquals(98, visible.get(third));
            assertEquals(100, raw.get(first)); assertEquals(3, raw.size());
        }
    }

    @Test void pendingAndFailedReadsStayVisibleAndDuplicateRequestsCoalesce() {
        var future = new CompletableFuture<PlayerPresence>(); var calls = new AtomicInteger(); var raw = board(row(1));
        try (var filter = new TopVoterDisplayFilter(player -> { calls.incrementAndGet(); return future; })) {
            assertEquals(raw, filter.project(raw, true)); assertEquals(raw, filter.project(raw, true));
            assertEquals(1, calls.get()); future.completeExceptionally(new IllegalStateException("synthetic unavailable"));
            assertEquals(raw, filter.project(raw, true));
        }
    }

    @Test void joinAndRetirementRejectLateNegativeCompletion() {
        var future = new CompletableFuture<PlayerPresence>(); var player = row(1); var raw = board(player);
        var fresh = new CompletableFuture<PlayerPresence>();
        var requests = new java.util.ArrayDeque<CompletableFuture<PlayerPresence>>(); requests.add(future); requests.add(fresh);
        var filter = new TopVoterDisplayFilter(ignored -> requests.remove());
        assertEquals(raw, filter.project(raw, true)); filter.joined(player.getUuid(), player.getPlayerName());
        future.complete(PlayerPresence.NEVER_JOINED);
        assertEquals(raw, filter.project(raw, true));
        filter.close(); fresh.complete(PlayerPresence.NEVER_JOINED); assertEquals(raw, filter.project(raw, true));
        try (var replacement = new TopVoterDisplayFilter(ignored -> CompletableFuture.completedStage(PlayerPresence.JOINED))) {
            assertEquals(raw, replacement.project(raw, true));
        }
    }

    @Test void staleNegativeIsVisibleAgainUntilRefreshed() {
        var time = new AtomicLong(); var calls = new AtomicInteger(); var raw = board(row(1));
        try (var filter = new TopVoterDisplayFilter(player -> calls.incrementAndGet() == 1
                ? CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED) : new CompletableFuture<>(), time::get)) {
            assertTrue(filter.project(raw, true).isEmpty()); time.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(30));
            assertEquals(raw, filter.project(raw, true)); assertEquals(2, calls.get());
        }
    }

    @Test void anUnobservedRemoteJoinCannotRemainHiddenWhileExpiredEvidenceRefreshes() {
        var time=new AtomicLong();var calls=new AtomicInteger();var raw=board(row(1));
        var refresh=new CompletableFuture<PlayerPresence>();
        try(var filter=new TopVoterDisplayFilter(player->calls.incrementAndGet()==1
                ?CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED):refresh,time::get)) {
            assertTrue(filter.project(raw,true).isEmpty());
            time.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(30));
            // No local joined() event: the shared history may have changed on another server.
            assertEquals(raw,filter.project(raw,true));
            assertEquals(raw,filter.project(raw,true));assertEquals(2,calls.get());
            refresh.complete(PlayerPresence.JOINED);
            assertEquals(raw,filter.project(raw,true));assertEquals(2,calls.get());
        }
    }

    @Test void largeBoardHasBoundedInFlightReadsAndSaturationKeepsRowsVisible() {
        var calls = new AtomicInteger(); var raw = new LinkedHashMap<TopVoterPlayer, Integer>();
        for (int id = 0; id < 10000; id++) raw.put(row(id), id);
        try (var filter = new TopVoterDisplayFilter(player -> { calls.incrementAndGet(); return new CompletableFuture<>(); })) {
            assertEquals(raw, filter.project(raw, true)); assertEquals(128, calls.get());
            assertEquals(raw, filter.project(raw, true)); assertEquals(128, calls.get());
        }
    }

    @Test void unlimitedBoardReusesEveryFreshEvidenceRowWithoutFlickerOrRepeatedReads() {
        var calls=new AtomicInteger();var pending=new java.util.concurrent.atomic.AtomicBoolean();
        var raw=new LinkedHashMap<TopVoterPlayer,Integer>();
        for(int id=0;id<10000;id++)raw.put(row(id),id);
        try(var filter=new TopVoterDisplayFilter(player->{calls.incrementAndGet();return pending.get()
                ?new CompletableFuture<>():CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED);},()->0L)) {
            assertTrue(filter.project(raw,true).isEmpty());assertEquals(10000,calls.get());pending.set(true);
            assertTrue(filter.project(raw,true).isEmpty(),"Fresh negatives must not flicker during an unchanged projection");
            assertEquals(10000,calls.get());assertEquals(10000,raw.size());
        }
    }

    @Test void loadedCurrentAndHistoricalBoardsShareAStableBudgetAndStillExpireOrInvalidate() {
        var calls=new AtomicInteger();var time=new AtomicLong();var budget=new AtomicLong(20000);
        var current=new LinkedHashMap<TopVoterPlayer,Integer>();var historical=new LinkedHashMap<TopVoterPlayer,Integer>();
        for(int id=0;id<10000;id++){current.put(row(id),id);historical.put(row(id+10000),id);}
        try(var filter=new TopVoterDisplayFilter(player->java.util.List.of(player.getUuid()),
                (player,aliases)->{calls.incrementAndGet();return CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED);},
                time::get,budget::get)) {
            assertTrue(filter.project(current,true).isEmpty());assertTrue(filter.project(historical,true).isEmpty());
            assertEquals(20000,calls.get());
            for(int i=0;i<3;i++){assertTrue(filter.project(current,true).isEmpty());assertTrue(filter.project(historical,true).isEmpty());}
            assertEquals(20000,calls.get());
            var joined=current.keySet().iterator().next();filter.joined(joined.getUuid(),joined.getPlayerName());
            assertTrue(filter.project(current,true).isEmpty());assertEquals(20001,calls.get());
            time.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(31));
            assertTrue(filter.project(current,true).isEmpty());assertEquals(30001,calls.get());
            budget.set(0L);
            assertTrue(filter.project(board(row(30000)),true).isEmpty());
            var cache=TopVoterDisplayFilter.class.getDeclaredField("cache");cache.setAccessible(true);
            assertTrue(((java.util.Map<?,?>)cache.get(filter)).size()<=8192,"Removing loaded boards restores the baseline retention bound");
        }catch(ReflectiveOperationException failure){throw new AssertionError(failure);}
    }

    @Test void exactCandidateAsyncPresenceServiceSuppliesCheckedDisplayEvidence() {
        var tasks = new java.util.ArrayDeque<Runnable>(); var first = row(1); var second = row(2); var raw = board(first, second);
        try (var service = new PlayerPresenceService(tasks::add,
                identity -> identity.equals(first.getUuid()) ? PlayerPresence.NEVER_JOINED : PlayerPresence.JOINED);
                var filter = new TopVoterDisplayFilter(player -> service.checkAsync(player.getUuid()))) {
            assertEquals(raw, filter.project(raw, true)); tasks.remove().run(); tasks.remove().run();
            assertEquals(java.util.List.of(second), java.util.List.copyOf(filter.project(raw, true).keySet()));
            assertEquals(2, raw.size());
        }
    }

    @Test void renamedCheckedAliasRevokesCompletedNegativeForLegacyRow() {
        var legacy = row(1); UUID current = new UUID(0, 2); var raw = board(legacy);
        var calls = new AtomicInteger();
        try (var filter = new TopVoterDisplayFilter(player -> java.util.List.of(player.getUuid(), current),
                (player, aliases) -> calls.incrementAndGet() == 1
                        ? CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED) : new CompletableFuture<>())) {
            assertTrue(filter.project(raw, true).isEmpty());
            filter.joined(current, "RenamedSynthetic");
            assertEquals(raw, filter.project(raw, true));
            assertEquals(2, calls.get());
        }
    }

    @Test void renamedCheckedAliasRejectsPendingNegativeCompletionForLegacyRow() {
        var legacy = row(1); UUID current = new UUID(0, 2); var raw = board(legacy);
        var late = new CompletableFuture<PlayerPresence>(); var calls = new AtomicInteger();
        try (var filter = new TopVoterDisplayFilter(player -> java.util.List.of(player.getUuid(), current),
                (player, aliases) -> calls.incrementAndGet() == 1 ? late : new CompletableFuture<>())) {
            assertEquals(raw, filter.project(raw, true));
            filter.joined(current, "RenamedSynthetic");
            late.complete(PlayerPresence.NEVER_JOINED);
            assertEquals(raw, filter.project(raw, true));
            assertEquals(2, calls.get());
        }
    }

    @Test void aliasScopeIsResolvedOnceAndCopiedBeforeProviderAdmission() {
        var raw = board(row(1)); var calls = new AtomicInteger();
        var scope = new java.util.ArrayList<UUID>(); scope.add(new UUID(0, 2)); scope.add(null);
        try (var filter = new TopVoterDisplayFilter(player -> { calls.incrementAndGet(); return scope; },
                (player, aliases) -> {
                    scope.clear();
                    assertEquals(2, aliases.size()); assertNull(aliases.get(1));
                    assertThrows(UnsupportedOperationException.class, () -> aliases.clear());
                    return CompletableFuture.completedStage(PlayerPresence.UNKNOWN);
                })) {
            assertEquals(raw, filter.project(raw, true)); assertEquals(raw, filter.project(raw, true));
            assertEquals(1, calls.get());
        }
    }

    @Test void joinWhileResolvingAliasesCannotPublishStaleNegativeOrConsumeAdmission() {
        var raw = board(row(1)); var reference = new java.util.concurrent.atomic.AtomicReference<TopVoterDisplayFilter>();
        var resolutions = new AtomicInteger(); var checks = new AtomicInteger(); UUID alias = new UUID(0, 2);
        try (var filter = new TopVoterDisplayFilter(player -> {
            if (resolutions.incrementAndGet() == 1) reference.get().joined(alias, "RenamedSynthetic");
            return java.util.List.of(player.getUuid(), alias);
        }, (player, aliases) -> { checks.incrementAndGet(); return CompletableFuture.completedStage(PlayerPresence.JOINED); })) {
            reference.set(filter);
            assertEquals(raw, filter.project(raw, true)); assertEquals(0, checks.get());
            assertEquals(raw, filter.project(raw, true)); assertEquals(1, checks.get());
        }
    }

    @Test void unrelatedJoinDoesNotRevokeCompletedAliasEvidence() {
        var raw = board(row(1)); var checks = new AtomicInteger();
        try (var filter = new TopVoterDisplayFilter(player -> java.util.List.of(player.getUuid()),
                (player, aliases) -> { checks.incrementAndGet(); return CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED); })) {
            assertTrue(filter.project(raw, true).isEmpty()); filter.joined(new UUID(0, 99), "UnrelatedSynthetic");
            assertTrue(filter.project(raw, true).isEmpty()); assertEquals(1, checks.get());
            filter.joined(null, "UnrelatedSynthetic"); assertTrue(filter.project(raw, true).isEmpty());
        }
    }

    @Test void asyncProjectionAwaitsWorkerWithoutResubmittingExpiredEvidence() {
        var tasks = new java.util.ArrayDeque<Runnable>(); var first = row(1); var second = row(2);
        var raw = board(first, second); var time = new AtomicLong();
        try (var service = new PlayerPresenceService(tasks::add, id -> id.equals(first.getUuid())
                ? PlayerPresence.NEVER_JOINED : PlayerPresence.JOINED);
                var filter = new TopVoterDisplayFilter(player -> service.checkAsync(player.getUuid()), time::get)) {
            var result = filter.projectAsync(raw, true).toCompletableFuture();
            assertFalse(result.isDone()); assertEquals(2, tasks.size());
            tasks.remove().run(); tasks.remove().run();
            assertEquals(java.util.List.of(second), java.util.List.copyOf(result.join().keySet()));
            time.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(31));
            var refresh = filter.projectAsync(raw, true).toCompletableFuture();
            assertFalse(refresh.isDone()); tasks.remove().run(); tasks.remove().run();
            assertEquals(java.util.List.of(second), java.util.List.copyOf(refresh.join().keySet()));
            assertTrue(tasks.isEmpty()); assertEquals(2, raw.size());
        }
    }

    @Test void joinAndCloseSettleAwaitingSnapshotsWithoutWaitingForStaleWorker() {
        var raw = board(row(1)); var pending = new CompletableFuture<PlayerPresence>();
        try (var filter = new TopVoterDisplayFilter(player -> pending)) {
            var joined = filter.projectAsync(raw, true).toCompletableFuture();
            filter.joined(row(1).getUuid(), "RenamedSynthetic");
            assertEquals(raw, joined.join());
            var retiring = filter.projectAsync(raw, true).toCompletableFuture();
            filter.close(); assertEquals(raw, retiring.join());
            pending.complete(PlayerPresence.NEVER_JOINED); assertEquals(raw, retiring.join());
        }
    }

    @Test void boundedAsyncDeadlineKeepsUnfinishedRowsVisibleWithoutFinishingProvider() throws Exception {
        var pending = new CompletableFuture<PlayerPresence>(); var raw = board(row(1));
        try (var filter = new TopVoterDisplayFilter(player -> pending)) {
            var result = filter.projectAsync(raw, true).toCompletableFuture();
            assertFalse(result.isDone());
            assertEquals(raw, result.get(7, java.util.concurrent.TimeUnit.SECONDS));
            assertFalse(pending.isDone());
            pending.complete(PlayerPresence.NEVER_JOINED);
            assertEquals(raw, result.join());
        }
    }
    @Test void pendingNegativeRejectsMappingChangeBeforeCompletion() {
        var player = row(1); var raw = board(player);
        var scope = new java.util.concurrent.atomic.AtomicReference<java.util.List<UUID>>(java.util.List.of(player.getUuid()));
        var request = new CompletableFuture<PlayerPresence>(); var calls = new AtomicInteger();
        try (var filter = new TopVoterDisplayFilter(ignored -> scope.get(), (ignored, aliases) ->
                calls.incrementAndGet() == 1 ? request : new CompletableFuture<>())) {
            var result = filter.projectAsync(raw, true).toCompletableFuture();
            scope.set(java.util.Arrays.asList(player.getUuid(), new UUID(0, 2), null));
            request.complete(PlayerPresence.NEVER_JOINED);
            assertEquals(raw, result.join());
            assertEquals(raw, filter.project(raw, true));
        }
    }

    @Test void completedNegativeRevalidatesExactScopeIncludingAmbiguityMarker() {
        var player = row(1); var raw = board(player);
        var scope = new java.util.concurrent.atomic.AtomicReference<java.util.List<UUID>>(java.util.List.of(player.getUuid()));
        var calls = new AtomicInteger();
        try (var filter = new TopVoterDisplayFilter(ignored -> scope.get(), (ignored, aliases) ->
                calls.incrementAndGet() == 1 ? CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED)
                        : new CompletableFuture<>())) {
            assertTrue(filter.project(raw, true).isEmpty());
            scope.set(java.util.Arrays.asList(player.getUuid(), null));
            assertEquals(raw, filter.project(raw, true));
            assertEquals(raw, filter.projectAsync(raw, true).toCompletableFuture()
                    .completeOnTimeout(raw, 1, java.util.concurrent.TimeUnit.SECONDS).join());
        }
    }

    @Test void asyncProjectionRevalidatesCompletedNegativeWhileOtherRowRemainsPending() {
        var first = row(1); var second = row(2); var raw = board(first, second);
        var scope = new java.util.concurrent.atomic.AtomicReference<java.util.List<UUID>>(java.util.List.of(first.getUuid()));
        var pending = new CompletableFuture<PlayerPresence>();
        try (var filter = new TopVoterDisplayFilter(player -> player == null ? null
                : player.getUuid().equals(first.getUuid()) ? scope.get() : java.util.List.of(second.getUuid()),
                (player, aliases) -> player.getUuid().equals(first.getUuid())
                        ? CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED) : pending)) {
            var result = filter.projectAsync(raw, true).toCompletableFuture();
            assertFalse(result.isDone());
            scope.set(java.util.List.of(first.getUuid(), new UUID(0, 3)));
            pending.complete(PlayerPresence.JOINED);
            assertEquals(raw, result.join());
        }
    }

    @Test void negativeScopeRevalidationFailureKeepsRowVisible() {
        var player = row(1); var raw = board(player); var failResolution = new java.util.concurrent.atomic.AtomicBoolean();
        try (var filter = new TopVoterDisplayFilter(ignored -> {
            if (failResolution.get()) throw new IllegalStateException("synthetic unavailable");
            return java.util.List.of(player.getUuid());
        }, (ignored, aliases) -> CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED))) {
            assertTrue(filter.project(raw, true).isEmpty());
            failResolution.set(true); assertEquals(raw, filter.project(raw, true));
        }
    }

    @Test void negativeCompletionAndProjectionResolveScopeWithoutHoldingEvidenceMonitor() throws Exception {
        var player = row(1); var raw = board(player); var calls = new AtomicInteger();
        var reference = new java.util.concurrent.atomic.AtomicReference<TopVoterDisplayFilter>();
        try (var executor = java.util.concurrent.Executors.newSingleThreadExecutor();
                var filter = new TopVoterDisplayFilter(ignored -> {
                    if (calls.incrementAndGet() > 1) {
                        try {
                            executor.submit(() -> reference.get().joined(new UUID(0, 99), "UnrelatedSynthetic"))
                                    .get(1, java.util.concurrent.TimeUnit.SECONDS);
                        } catch (Exception failure) { throw new AssertionError("Resolver must run outside evidence monitor", failure); }
                    }
                    return java.util.List.of(player.getUuid());
                }, (ignored, aliases) -> CompletableFuture.completedStage(PlayerPresence.NEVER_JOINED))) {
            reference.set(filter);
            assertTrue(filter.projectAsync(raw, true).toCompletableFuture().get(1, java.util.concurrent.TimeUnit.SECONDS).isEmpty());
            assertTrue(filter.project(raw, true).isEmpty());
        }
    }

}
