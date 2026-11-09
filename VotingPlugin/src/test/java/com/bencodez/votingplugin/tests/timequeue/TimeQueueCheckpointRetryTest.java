package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;

class TimeQueueCheckpointRetryTest {
    @Test void repeatedStorageFailuresBackOffAndRepairResumesTheSameOccurrence() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        ServerData data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
        var timer = plugin.getVoteTimer();
        var bukkit = plugin.getBukkitScheduler();
        List<Runnable> attempts = new ArrayList<>(), retries = new ArrayList<>();
        List<Long> seconds = new ArrayList<>();
        doAnswer(call -> { attempts.add(call.getArgument(0)); return null; })
                .when(timer).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
        doAnswer(call -> { retries.add(call.getArgument(1)); seconds.add(call.getArgument(2)); return null; })
                .when(bukkit).runTaskLaterAsynchronously(eq(plugin), any(Runnable.class), anyLong());
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        handler.addVote("First", "site");
        doThrow(new IllegalStateException("synthetic write failure"))
                .doThrow(new IllegalStateException("synthetic write failure"))
                .doNothing().when(data).prepareTimedVoteDispatch(any(), any(), any());
        assertThrows(IllegalStateException.class, attempts.getFirst()::run);
        var id = handler.getTimeChangeQueue().peek().getVoteId();
        retries.getFirst().run();
        assertThrows(IllegalStateException.class, attempts.get(1)::run);
        assertEquals(List.of(1L, 2L), seconds);
        assertEquals(id, handler.getTimeChangeQueue().peek().getVoteId());
        retries.get(1).run();
        attempts.get(2).run();
        assertTrue(handler.getTimeChangeQueue().isEmpty());
        verify(plugin.getServer().getPluginManager(), times(1)).callEvent(any());
    }

    @Test void scheduledPreparationAndCompletionFailuresEachRetryWithoutRepeatingDeliveredEvents() {
        for (String phase : List.of("prepare", "complete")) {
            VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
            ServerData data = mock(ServerData.class);
            when(plugin.getServerData()).thenReturn(data);
            when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
            List<Runnable> scheduled = new ArrayList<>();
            var timer = plugin.getVoteTimer();
            doAnswer(call -> { scheduled.add(call.getArgument(0)); return null; })
                    .when(timer).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
            TimeQueueHandler handler = new TimeQueueHandler(plugin);
            handler.addVote("First", "site");
            handler.addVote("Later", "site");
            if (phase.equals("prepare")) doThrow(new IllegalStateException("synthetic write failure"))
                    .doNothing().when(data).prepareTimedVoteDispatch(any(), any(), any());
            else doThrow(new IllegalStateException("synthetic write failure"))
                    .doNothing().when(data).completeTimedVoteDispatch(any());
            assertThrows(IllegalStateException.class, scheduled.getFirst()::run);
            var retry = org.mockito.ArgumentCaptor.forClass(Runnable.class);
            verify(plugin.getBukkitScheduler(), times(1)).runTaskLaterAsynchronously(eq(plugin), retry.capture(), eq(1L));
            retry.getValue().run();
            assertEquals(2, scheduled.size());
            scheduled.get(1).run();
            assertTrue(handler.getTimeChangeQueue().isEmpty());
            verify(plugin.getServer().getPluginManager(), times(2)).callEvent(any());
            if (phase.equals("complete")) verify(data, times(1)).addUncertainTimedVote(any(),
                    argThat(vote -> vote.getName().equals("First")));
        }
    }
}
