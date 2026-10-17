package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginManager;
import org.bukkit.plugin.RegisteredListener;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.user.validation.UserValidationResult;
import com.bencodez.advancedcore.api.user.validation.ValidationSource;
import com.bencodez.advancedcore.api.user.validation.ValidationStatus;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

class TimeQueueHandlerCancellationTest {
    @Test
    void cancelledFirstVoteDoesNotStrandAcceptedVoteInScheduledDrain() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
        PluginManager manager = plugin.getServer().getPluginManager();
        VoteSite site = mock(VoteSite.class);
        VotingPluginUser cancelledUser = mock(VotingPluginUser.class);
        VotingPluginUser acceptedUser = mock(VotingPluginUser.class);
        UUID acceptedId = UUID.fromString("00000000-0000-0000-0000-000000000002");
        when(plugin.getVoteTimer()).thenReturn(voteTimer);
        when(plugin.getServerData().getTimedVoteCacheKeys()).thenReturn(Set.of());
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getOptions().isProcessRewards()).thenReturn(true);
        when(plugin.getConfigFile().isAddTotals()).thenReturn(true);
        when(plugin.getVoteSiteManager().getVoteSiteName(true, "example.org")).thenReturn("example");
        when(plugin.getVoteSiteManager().getVoteSite("example", true)).thenReturn(site);
        when(plugin.getUserManager().getProperName("AcceptedVoter")).thenReturn("AcceptedVoter");
        when(plugin.getUserManager().getValidationService().validate("AcceptedVoter", false))
                .thenReturn(new UserValidationResult(ValidationStatus.VALID, "AcceptedVoter",
                        ValidationSource.STORAGE, "stored test user", false));
        when(site.isEnabled()).thenReturn(true);
        when(site.getKey()).thenReturn("example");
        when(site.getServiceSite()).thenReturn("example.org");
        when(acceptedUser.getPlayerName()).thenReturn("AcceptedVoter");
        when(acceptedUser.getUUID()).thenReturn(acceptedId.toString());
        when(acceptedUser.getJavaUUID()).thenReturn(acceptedId);
        when(acceptedUser.isOnline()).thenReturn(true);

        RegisteredListener cancelling = new RegisteredListener(new Listener() { },
                (target, event) -> {
                    if (((PlayerVoteEvent) event).getPlayer().equals("CancelledVoter"))
                        ((PlayerVoteEvent) event).setCancelled(true);
                }, EventPriority.HIGHEST, mock(Plugin.class), false);
        PlayerVoteListener consumer = new PlayerVoteListener(plugin);
        RegisteredListener consuming = new RegisteredListener(consumer,
                (target, event) -> ((PlayerVoteListener) target).onplayerVote((PlayerVoteEvent) event),
                EventPriority.MONITOR, mock(Plugin.class), true);
        List<PlayerVoteEvent> dispatched = new ArrayList<>();
        List<PlayerPostVoteEvent> completed = new ArrayList<>();
        doAnswer(call -> {
            Object event = call.getArgument(0);
            if (event instanceof PlayerVoteEvent vote) {
                dispatched.add(vote);
                vote.setBroadcast(false);
                vote.setVotingPluginUser(vote.getPlayer().equals("CancelledVoter") ? cancelledUser : acceptedUser);
                cancelling.callEvent(vote);
                consuming.callEvent(vote);
            } else if (event instanceof PlayerPostVoteEvent post) completed.add(post);
            return null;
        }).when(manager).callEvent(any());

        TimeQueueHandler queue = new TimeQueueHandler(plugin);
        queue.getTimeChangeQueue().add(new VoteTimeQueue("CancelledVoter", "example.org", 123L));
        queue.getTimeChangeQueue().add(new VoteTimeQueue("AcceptedVoter", "example.org", 456L));
        ArgumentCaptor<Runnable> drain = ArgumentCaptor.forClass(Runnable.class);
        verify(voteTimer).schedule(drain.capture(), eq(120L), eq(TimeUnit.SECONDS));

        drain.getValue().run();

        assertEquals(List.of("CancelledVoter", "AcceptedVoter"), dispatched.stream().map(PlayerVoteEvent::getPlayer).toList());
        assertTrue(dispatched.get(0).isCancelled());
        assertFalse(dispatched.get(1).isCancelled());
        assertEquals(456L, dispatched.get(1).getTime());
        assertTrue(queue.getTimeChangeQueue().isEmpty(), "cancellation must not strand later votes");
        assertEquals(1, completed.size(), "only the accepted vote reaches post-vote completion");
        verifyNoInteractions(cancelledUser);
        verify(acceptedUser).playerVote(site, true, false);
        verify(acceptedUser).addPoints();
        verify(acceptedUser).addTotal();
        verify(acceptedUser).addTotalDaily();
        verify(acceptedUser).addTotalWeekly();
        verify(acceptedUser).setTime(site, 456L);
        verify(plugin.getServerData()).clearTimedVoteCache();

        queue.processQueue();
        assertEquals(2, dispatched.size(), "a subsequent drain cannot dispatch either retired entry again");
        assertEquals(1, completed.size());
        verify(acceptedUser, times(1)).playerVote(site, true, false);
        verify(acceptedUser, times(1)).addPoints();
        verify(voteTimer, times(1)).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
    }
}
