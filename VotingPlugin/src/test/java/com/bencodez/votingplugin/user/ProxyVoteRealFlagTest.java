package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

class ProxyVoteRealFlagTest {
    @Test void accountingFailureFlagSurvivesBukkitDispatchAndRejectsCompletion() throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        var user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        var field = VotingPluginUser.class.getDeclaredField("plugin"); field.setAccessible(true); field.set(user, plugin);
        doReturn("Player").when(user).getPlayerName();
        var dispatcher = plugin.getServer().getPluginManager();
        doAnswer(call -> { ((PlayerVoteEvent) call.getArgument(0)).setDateMilestoneAccountingFailed(true); return null; })
                .when(dispatcher).callEvent(any());
        assertThrows(IllegalStateException.class, () -> user.bungeeVotePluginMessaging("service", 123, null,
                false, false, false, 1, false, true, true, UUID.randomUUID(), false, true));
        verify(plugin.getServer().getPluginManager(), times(1)).callEvent(any());
    }
    @Test void nativeBackendAdapterPreservesCanonicalTestFlagAndLegacyDefaults() throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        var user = mock(VotingPluginUser.class, CALLS_REAL_METHODS);
        var field = VotingPluginUser.class.getDeclaredField("plugin"); field.setAccessible(true); field.set(user, plugin);
        doReturn("Player").when(user).getPlayerName();
        UUID id = UUID.randomUUID();
        user.bungeeVotePluginMessaging("service", 123, null, false, false, false, 1,
                false, true, true, id, false, false);
        user.bungeeVotePluginMessaging("service", 123, null, false, false, false, 1,
                false, true, true, id, false);
        var event = ArgumentCaptor.forClass(org.bukkit.event.Event.class);
        verify(plugin.getServer().getPluginManager(), times(2)).callEvent(event.capture());
        var fake = (PlayerVoteEvent) event.getAllValues().get(0);
        var legacy = (PlayerVoteEvent) event.getAllValues().get(1);
        assertFalse(fake.isRealVote()); assertTrue(legacy.isRealVote());
        assertEquals(id, fake.getProxyVoteId()); assertEquals(123, fake.getTime());
        assertTrue(fake.isBungee()); assertFalse(fake.isTargetedProxyVote());
    }
}
