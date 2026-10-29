package com.bencodez.votingplugin.tests.placeholders;

import com.bencodez.votingplugin.placeholders.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import java.util.UUID;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

class DateVotePlaceholderEntryPointTest {
    @Test void publicOnlineAndOfflineRoutesPreserveEventCaseAndBypassLegacyIdentityAndCacheWork() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        var manager = mock(DateVoteMilestones.class);
        var presence = new PlaceholderPlayerPresence();
        var player = mock(Player.class); var offline = mock(OfflinePlayer.class);
        UUID storage = UUID.randomUUID(), offlineId = UUID.randomUUID();
        presence.playerOnline(storage, player);
        when(plugin.getPlaceholderPlayerPresence()).thenReturn(presence);
        when(plugin.getDateVoteMilestones()).thenReturn(manager);
        when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.AUTO);
        when(offline.getUniqueId()).thenReturn(offlineId);
        when(manager.placeholder(storage, "votes_Event_With_Case")).thenReturn("3");
        when(manager.placeholder(eq(offlineId), any(java.util.function.Supplier.class), eq("award_10_Event_With_Case"))).thenReturn("submitted");
        var placeholders = new PlaceHolders(plugin);
        assertEquals("3", placeholders.getPlaceHolder(player, "DateVoteMilestones_votes_Event_With_Case"));
        assertEquals("3", placeholders.getPlaceHolder((OfflinePlayer) player, "datevotemilestones_votes_Event_With_Case", true));
        assertEquals("submitted", placeholders.getPlaceHolder(offline, "DateVoteMilestones_award_10_Event_With_Case"));
        assertEquals("3", placeholders.getPlaceholderValue(player, "DateVoteMilestones_votes_Event_With_Case", false, false, true));
        verifyNoInteractions(plugin.getVotingPluginUserManager(), plugin.getUserManager(), plugin.getServerData());
        verify(player, never()).getUniqueId(); verify(player, never()).getName();
        verify(offline, never()).getName();
    }
    @Test void unknownOnlineIdentityAndMissingManagerAreUnavailableWithoutStorage() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getConfigFile().getPlaceholderCacheLevel()).thenReturn(PlaceholderCacheLevel.NONE);
        when(plugin.getPlaceholderPlayerPresence()).thenReturn(new PlaceholderPlayerPresence());
        var placeholders = new PlaceHolders(plugin);
        assertEquals("unavailable", placeholders.getPlaceHolder(mock(Player.class), "DateVoteMilestones_votes_case"));
        assertEquals("no player", placeholders.getPlaceHolder((OfflinePlayer) null, "DateVoteMilestones_votes_case"));
        verifyNoInteractions(plugin.getVotingPluginUserManager(), plugin.getUserManager());
    }
}
