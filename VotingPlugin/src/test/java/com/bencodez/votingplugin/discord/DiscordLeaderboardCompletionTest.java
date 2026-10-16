package com.bencodez.votingplugin.discord;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.util.ArrayDeque;
import java.util.LinkedHashMap;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.user.presence.PlayerPresence;
import com.bencodez.advancedcore.api.user.presence.PlayerPresenceService;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.topvoter.TopVoterDisplayFilter;

class DiscordLeaderboardCompletionTest {
    static final class RecordingHandler extends DiscordHandler {
        int posts;
        LinkedHashMap<TopVoterPlayer, Integer> published;
        RecordingHandler(VotingPluginMain plugin) { super(plugin); }
        @Override void publishTopVoterMessage(TopVoter top, LinkedHashMap<TopVoterPlayer, Integer> snapshot) {
            posts++; published = snapshot;
        }
    }
    static final class Fixture implements AutoCloseable {
        final ArrayDeque<Runnable> worker = new ArrayDeque<>();
        final AtomicLong clock = new AtomicLong(), generation = new AtomicLong();
        final TopVoterPlayer never = new TopVoterPlayer(new UUID(0, 1), "SyntheticNever", 1L);
        final TopVoterPlayer joined = new TopVoterPlayer(new UUID(0, 2), "SyntheticJoined", 1L);
        final LinkedHashMap<TopVoterPlayer, Integer> raw = new LinkedHashMap<>();
        final PlayerPresenceService service = new PlayerPresenceService(worker::add,
                uuid -> uuid.equals(never.getUuid()) ? PlayerPresence.NEVER_JOINED : PlayerPresence.JOINED);
        final TopVoterDisplayFilter filter = new TopVoterDisplayFilter(row -> service.checkAsync(row.getUuid()), clock::get);
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final RecordingHandler handler;
        Fixture() {
            raw.put(never, 100); raw.put(joined, 90);
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
            when(plugin.getServerData()).thenReturn(mock(ServerData.class));
            when(plugin.getTopVoterDisplayGeneration()).thenAnswer(call -> generation.get());
            when(plugin.getTopVoterDisplayAsync(any())).thenAnswer(call -> filter.projectAsync(raw, true));
            when(plugin.getTopVoterDisplay(any())).thenAnswer(call -> filter.project(raw, true));
            var timer = mock(ScheduledExecutorService.class);
            doAnswer(call -> { worker.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            when(plugin.getTimer()).thenReturn(timer);
            handler = new RecordingHandler(plugin);
        }
        void drain() {
            int bound = 20;
            while (!worker.isEmpty()) { assertTrue(bound-- > 0, "Completion must not retry indefinitely"); worker.remove().run(); }
        }
        @Override public void close() { filter.close(); service.close(); }
    }

    @Test void asynchronousProjectionFailureHasARedactedDiagnosticAndReleasesPreparation() {
        try(var f=new Fixture()) {
            doThrow(new IllegalStateException("synthetic-private-detail")).when(f.plugin).getTopVoterDisplay(any());
            f.handler.updateTopVoterMessageId(TopVoter.Monthly);assertDoesNotThrow(f::drain);
            verify(f.plugin.getLogger()).warning(argThat((String text)->text.contains("Discord")&&!text.contains("synthetic-private-detail")));
            assertEquals(0,f.handler.posts);
            doAnswer(call->f.filter.project(f.raw,true)).when(f.plugin).getTopVoterDisplay(any());
            f.handler.updateTopVoterMessageId(TopVoter.Monthly);f.drain();assertEquals(1,f.handler.posts);
        }
    }

    @Test void freshAndExpiredUpdatesOnPresenceWorkerPublishOnlyCompletedProjection() {
        try (var fixture = new Fixture()) {
            fixture.worker.add(() -> fixture.handler.updateTopVoterMessageId(TopVoter.Monthly));
            fixture.worker.remove().run(); assertEquals(0, fixture.handler.posts);
            fixture.drain(); assertEquals(1, fixture.handler.posts);
            assertEquals(java.util.List.of(fixture.joined), java.util.List.copyOf(fixture.handler.published.keySet()));
            fixture.clock.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(31));
            fixture.worker.add(() -> fixture.handler.updateTopVoterMessageId(TopVoter.Monthly));
            fixture.worker.remove().run(); assertEquals(1, fixture.handler.posts);
            fixture.drain(); assertEquals(2, fixture.handler.posts);
            assertEquals(1, fixture.handler.published.size()); assertEquals(2, fixture.raw.size());
        }
    }

    @Test void repeatedUpdatesDuringPreparationCoalesceToOnePublish() {
        try (var fixture = new Fixture()) {
            for (int count = 0; count < 1000; count++) fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            assertEquals(2, fixture.worker.size()); assertEquals(0, fixture.handler.posts);
            fixture.drain(); assertEquals(1, fixture.handler.posts);
            verify(fixture.plugin, times(1)).getTopVoterDisplayAsync(TopVoter.Monthly);
        }
    }

    @Test void reloadAndDisableDiscardOlderPreparedSnapshots() {
        try (var fixture = new Fixture()) {
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.generation.incrementAndGet(); fixture.drain(); assertEquals(0, fixture.handler.posts);
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            when(fixture.plugin.isEnabled()).thenReturn(false); fixture.drain(); assertEquals(0, fixture.handler.posts);
            when(fixture.plugin.isEnabled()).thenReturn(true);
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly); fixture.drain(); assertEquals(1, fixture.handler.posts);
        }
    }

    @Test void newerGenerationUpdateWhileOlderPreparationIsPendingIsNotLost() {
        try (var fixture = new Fixture()) {
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.generation.incrementAndGet();
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.drain(); assertEquals(1, fixture.handler.posts);
            assertEquals(1, fixture.handler.published.size());
        }
    }

    @Test void rejectedCompletionDispatchReleasesPreparationForNextUpdate() {
        try (var fixture = new Fixture()) {
            ScheduledExecutorService timer = fixture.plugin.getTimer();
            doThrow(new java.util.concurrent.RejectedExecutionException("synthetic")).when(timer).execute(any(Runnable.class));
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly); fixture.drain();
            assertEquals(0, fixture.handler.posts);
            doAnswer(call -> { fixture.worker.add(call.getArgument(0)); return null; }).when(timer).execute(any(Runnable.class));
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly); fixture.drain();
            assertEquals(1, fixture.handler.posts);
        }
    }
    @Test void overlappingUpdatesPublishFirstRealDeadlineFallbackWithoutRestartingWait() throws Exception {
        try (var fixture = new Fixture()) {
            var dispatched = new java.util.concurrent.CompletableFuture<Runnable>();
            ScheduledExecutorService timer = fixture.plugin.getTimer();
            doAnswer(call -> { dispatched.complete(call.getArgument(0)); return null; })
                    .when(timer).execute(any(Runnable.class));
            when(fixture.plugin.getTopVoterDisplay(any())).thenAnswer(call -> fixture.filter.project(fixture.raw, true));
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            for (int count = 0; count < 1000; count++) fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            // Leave the real presence worker queued: the production five-second preparation timeout fires.
            dispatched.get(7, java.util.concurrent.TimeUnit.SECONDS).run();
            assertEquals(1, fixture.handler.posts);
            assertEquals(fixture.raw, fixture.handler.published);
            verify(fixture.plugin, times(1)).getTopVoterDisplayAsync(TopVoter.Monthly);
            assertEquals(2, fixture.worker.size());
        }
    }

    @Test void coalescedPublicationUsesCurrentTotalsAndDiscardsSubsequentUnrequestedReload() {
        try (var fixture = new Fixture()) {
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.raw.put(fixture.joined, 123);
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.drain(); assertEquals(1, fixture.handler.posts);
            assertEquals(123, fixture.handler.published.get(fixture.joined));
            fixture.clock.set(java.util.concurrent.TimeUnit.SECONDS.toNanos(31));
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.generation.incrementAndGet(); fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.generation.incrementAndGet(); fixture.drain();
            assertEquals(1, fixture.handler.posts, "An unrequested later generation must discard coalesced stale work");
        }
    }

    @Test void joinAfterPreparationBeforeQueuedPublicationRevokesOldOmission() {
        try (var fixture = new Fixture()) {
            fixture.handler.updateTopVoterMessageId(TopVoter.Monthly);
            fixture.worker.remove().run(); fixture.worker.remove().run();
            assertEquals(0, fixture.handler.posts);
            fixture.filter.joined(fixture.never.getUuid(), fixture.never.getPlayerName());
            fixture.worker.remove().run();
            assertEquals(1, fixture.handler.posts);
            assertTrue(fixture.handler.published.containsKey(fixture.never),
                    "Publication must revalidate the omitted row after a join");
            verify(fixture.plugin, times(1)).getTopVoterDisplayAsync(TopVoter.Monthly);
        }
    }

}
