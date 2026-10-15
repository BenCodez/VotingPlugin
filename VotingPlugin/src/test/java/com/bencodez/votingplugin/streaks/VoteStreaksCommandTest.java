package com.bencodez.votingplugin.streaks;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;
import java.util.ArrayList;
import java.util.List;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VoteStreaksCommandTest {
    @Test void actualCommandLoaderInitializesAndRetainsOneStreaksHandlerOnEveryLoad() throws Exception {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        var list = new java.util.concurrent.atomic.AtomicReference<ArrayList<CommandHandler>>();
        when(plugin.getVoteCommand()).thenAnswer(call -> list.get());
        doAnswer(call -> { list.set(call.getArgument(0)); return null; }).when(plugin).setVoteCommand(any());
        var loader = new com.bencodez.votingplugin.commands.CommandLoader(plugin);
        var load = loader.getClass().getDeclaredMethod("loadVoteCommand"); load.setAccessible(true);
        var basic = mock(com.bencodez.advancedcore.command.CommandLoader.class);
        var inherited = mock(CommandHandler.class); when(inherited.getArgs()).thenReturn(new String[]{"Version"});
        when(inherited.getPerm()).thenReturn("VotingPlugin.Version");
        when(basic.getBasicCommands("VotingPlugin")).thenAnswer(call -> new ArrayList<>(List.of(inherited)));
        try (var singleton = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            singleton.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(basic);
            for (int i = 0; i < 2; i++) {
                load.invoke(loader);
                assertEquals(1, list.get().stream().filter(c -> java.util.Arrays.equals(c.getArgs(), new String[]{"Streaks"})).count());
                assertTrue(list.get().stream().anyMatch(c -> java.util.Arrays.equals(c.getArgs(), new String[]{"Help&?"})));
                assertTrue(list.get().stream().anyMatch(c -> java.util.Arrays.equals(c.getArgs(), new String[]{"Shop"})));
                assertTrue(list.get().contains(inherited), "normal shared commands must remain registered");
            }
        }
    }
    @Test void realDispatchPreservesExistingHandlersAndUsesDedicatedPlayerPermission() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); var old = mock(CommandHandler.class);
        var commands = new ArrayList<>(List.of(old)); when(plugin.getVoteCommand()).thenReturn(commands);
        var scheduler = mock(BukkitScheduler.class); when(plugin.getBukkitScheduler()).thenReturn(scheduler);
        var tasks = new java.util.ArrayDeque<Runnable>();
        doAnswer(call -> { tasks.add(call.getArgument(1)); return null; }).when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
        var menu = mock(VoteStreaksMenu.class); when(plugin.getVoteStreaksMenu()).thenReturn(menu);
        VoteStreaksCommand.register(plugin); assertEquals(2, commands.size()); assertSame(old, commands.getFirst());
        CommandHandler command = commands.getLast(); assertArrayEquals(new String[] {"Streaks"}, command.getArgs());
        assertEquals("VotingPlugin.Commands.Vote.Streaks|VotingPlugin.Player", command.getPerm());
        Player player = mock(Player.class); when(player.hasPermission(anyString())).thenReturn(true);
        assertTrue(command.runCommand(player, new String[] {"streaks"})); verifyNoInteractions(menu);
        tasks.remove().run(); verify(menu).open(player); verifyNoInteractions(old);
        command.execute(mock(CommandSender.class), new String[] {"Streaks"}); verify(menu, times(1)).open(any());
    }
    @Test void deniedPermissionCannotReadUserDataOrOpenAMenu() {
        var plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS); var commands = new ArrayList<CommandHandler>();
        when(plugin.getOptions().getFormatNoPerms()).thenReturn("Denied");
        when(plugin.getVoteCommand()).thenReturn(commands); VoteStreaksCommand.register(plugin);
        var scheduler = mock(BukkitScheduler.class); when(plugin.getBukkitScheduler()).thenReturn(scheduler);
        Player denied = mock(Player.class);
        assertTrue(commands.getFirst().runCommand(denied, new String[] {"Streaks"}));
        verifyNoInteractions(plugin.getVotingPluginUserManager());
        verify(scheduler, never()).runTaskAsynchronously(any(), any(Runnable.class));
    }
    @Test void leafPermissionDeclaresOnlyTheRootGateAsAChild() throws Exception {
        try (var input = getClass().getResourceAsStream("/plugin.yml")) {
            assertNotNull(input); var description = new org.bukkit.plugin.PluginDescriptionFile(input);
            var permission = description.getPermissions().stream().filter(p -> p.getName().equals(VoteStreaksMenu.PERMISSION)).findFirst().orElseThrow();
            assertEquals(java.util.Map.of("VotingPlugin.Commands.Vote", true), permission.getChildren());
        }
    }
}
