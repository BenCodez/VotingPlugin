package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;
import com.bencodez.votingplugin.user.VotingPluginUser;

class ExperimentalStreakPreviewTest {
    @Test void presentationUsesActualStoredProgressAndAwardIdentityWithoutMutations() {
        var plugin = mock(VotingPluginMain.class);
        var user = mock(VotingPluginUser.class);
        var handler = new VoteStreakHandler(plugin);
        var first = new VoteStreakDefinition("ten", VoteStreakType.DAILY, true, 10, 1, 0, 0, false, "shared");
        var next = new VoteStreakDefinition("twenty", VoteStreakType.DAILY, true, 20, 1, 0, 0, false, "shared");
        when(user.getVoteStreakState("VoteStreakGroup_DAILY_shared"))
                .thenReturn("2026-10-09|12|1|true||0|ten|15");
        var preview = handler.getStoredPreview(user, first);
        assertEquals(12, preview.amount());
        assertEquals(15, preview.bestAmount());
        assertTrue(preview.awardRecorded());
        assertFalse(handler.getStoredPreview(user, next).awardRecorded());
        verify(user, times(2)).getVoteStreakState("VoteStreakGroup_DAILY_shared");
        verifyNoMoreInteractions(user);
        verifyNoInteractions(plugin);
    }

    @Test void legacyBestFallbackIsReadOnlyAndDoesNotRewriteOldSerializedData() {
        var user = mock(VotingPluginUser.class);
        var handler = new VoteStreakHandler(mock(VotingPluginMain.class));
        var definition = new VoteStreakDefinition("old", VoteStreakType.DAILY, true, 3, 1, 0, 0, false);
        when(user.getVoteStreakState("VoteStreak_old")).thenReturn("2026-10-09|2|true||0");
        when(user.getBestDayVoteStreak()).thenReturn(9);
        var preview = handler.getStoredPreview(user, definition);
        assertEquals(2, preview.amount());
        assertEquals(9, preview.bestAmount());
        assertFalse(preview.awardRecorded());
        verify(user).getVoteStreakState("VoteStreak_old");
        verify(user).getBestDayVoteStreak();
        verifyNoMoreInteractions(user);
    }
}
