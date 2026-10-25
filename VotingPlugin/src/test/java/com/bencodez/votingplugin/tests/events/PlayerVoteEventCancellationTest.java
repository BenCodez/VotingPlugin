package com.bencodez.votingplugin.tests.events;

import com.bencodez.votingplugin.events.PlayerVoteEvent;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import org.bukkit.event.Cancellable;
import org.bukkit.event.EventPriority;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.RegisteredListener;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;
import com.bencodez.votingplugin.listeners.PlayerVoteListener;

class PlayerVoteEventCancellationTest {
    @Test
    void eventUsesBukkitCancellationContract() {
        PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "Example", true);

        assertTrue(event instanceof Cancellable);
        assertFalse(event.isCancelled());
        event.setCancelled(true);
        assertTrue(event.isCancelled());
    }

    @Test
    void bukkitDispatchSkipsCancelledVotesButProcessesUncancelledVotes() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        PlayerVoteListener listener = new PlayerVoteListener(plugin);
        RegisteredListener registration = new RegisteredListener(listener,
                (target, event) -> ((PlayerVoteListener) target).onplayerVote((PlayerVoteEvent) event),
                EventPriority.MONITOR, mock(Plugin.class), true);
        PlayerVoteEvent event = new PlayerVoteEvent(null, "Steve", "Example", true);
        try (var processor = org.mockito.Mockito.mockStatic(SharedVoteProcessor.class)) {
            event.setCancelled(true);
            registration.callEvent(event);
            listener.onplayerVote(event); // direct callers retain the same guard
            processor.verifyNoInteractions();
            event.setCancelled(false);
            registration.callEvent(event);
            processor.verify(() -> SharedVoteProcessor.process(org.mockito.ArgumentMatchers.any()),
                    org.mockito.Mockito.times(1));
        }
    }
}
