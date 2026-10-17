package com.bencodez.votingplugin.tests.session;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.List;
import java.util.UUID;
import java.util.function.BiConsumer;
import org.bukkit.Server;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;

/** Contract coverage for the real bundled Spigot UniDialog adapter. */
class GuidedSessionDialogsAdapterTest {
    @Test
    void showsNativeDialogRegistersItsEventBridgeAndReleasesOwnedActions() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        Server server = mock(Server.class, RETURNS_DEEP_STUBS);
        PluginManager plugins = mock(PluginManager.class);
        Player player = mock(Player.class);
        when(plugin.getServer()).thenReturn(server);
        when(server.getPluginManager()).thenReturn(plugins);

        SessionDialogViewAccess adapter = SessionDialogViewAccess.nativeAdapter(plugin);
        verify(plugins).registerEvent(any(), any(), any(), any(), eq(plugin), eq(false));

        BiConsumer<UUID, String> callback = mock(BiConsumer.class);
        Runnable cleanup = adapter.show(player, "Voting", "Choose a site", List.of(
                new SessionDialogViewAccess.Button("Vote", "Open", null, "https://example.test/vote"),
                new SessionDialogViewAccess.Button("Check", "Refresh", "check", null)), callback);
        assertNotNull(cleanup);
        verify(player).showDialog(any(net.md_5.bungee.api.dialog.Dialog.class));

        cleanup.run();
        cleanup.run();
        verifyNoInteractions(callback);
    }
}
