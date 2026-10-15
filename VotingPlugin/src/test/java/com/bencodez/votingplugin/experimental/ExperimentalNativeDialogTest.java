package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import java.util.List;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;

class ExperimentalNativeDialogTest {
    @Test void nativeSpigotBuilderOpensUrlAndPrivateCallbackButtonsWithoutProductionService() {
        assertTrue(NativeExperimentalDialog.supported());
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        Player player = mock(Player.class);
        var backend = new NativeExperimentalDialog(plugin);
        Runnable release = backend.show(player, "Voting", "Real streak preview", List.of(
                new ExperimentalDialogs.Button("Site", "Available", "https://example.test/vote", null),
                new ExperimentalDialogs.Button("Next", "Next sites", null, "next")), (owner, action) -> fail());
        var captured = org.mockito.ArgumentCaptor.forClass(net.md_5.bungee.api.dialog.Dialog.class);
        verify(player).showDialog(captured.capture());
        var dialog = assertInstanceOf(net.md_5.bungee.api.dialog.MultiActionDialog.class, captured.getValue());
        assertEquals(2, dialog.actions().size());
        assertEquals(net.md_5.bungee.api.dialog.DialogBase.AfterAction.CLOSE, dialog.getBase().afterAction());
        assertEquals(Boolean.TRUE, dialog.getBase().canCloseWithEscape());
        assertEquals(Boolean.FALSE, dialog.getBase().pause());
        var url = assertInstanceOf(net.md_5.bungee.api.dialog.action.StaticAction.class, dialog.actions().getFirst().action());
        assertEquals(net.md_5.bungee.api.chat.ClickEvent.Action.OPEN_URL, url.clickEvent().getAction());
        assertEquals("https://example.test/vote", url.clickEvent().getValue());
        verify(plugin, never()).getDialogService();
        release.run(); release.run();
        backend.close(player); verify(player).clearDialog();
    }
}
