package com.bencodez.votingplugin.npc;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.LinkedBlockingQueue;

import org.bukkit.Location;
import org.bukkit.World;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VotingNPCCommandsTest {
    private static class Fixture {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final BukkitScheduler scheduler = mock(BukkitScheduler.class);
        final VotingNPCManager manager = mock(VotingNPCManager.class);
        final ArrayList<CommandHandler> handlers = new ArrayList<>();
        final LinkedBlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
        final LinkedBlockingQueue<Runnable> asynchronous = new LinkedBlockingQueue<>();
        Fixture() {
            when(plugin.getAdminVoteCommand()).thenReturn(handlers);
            when(plugin.getVotingNPCManager()).thenReturn(manager);
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            when(manager.available()).thenReturn(true);
            doAnswer(inv -> { tasks.add(inv.getArgument(1)); return null; })
                    .when(scheduler).runTask(eq(plugin), any(Runnable.class), any(Entity.class));
            doAnswer(inv -> { tasks.add(inv.getArgument(1)); return null; })
                    .when(scheduler).runTask(eq(plugin), any(Runnable.class));
            doAnswer(inv -> { asynchronous.add(inv.getArgument(1)); return null; })
                    .when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
            VotingNPCCommands.register(plugin);
        }
        Player player() {
            Player player = mock(Player.class);
            when(player.isOnline()).thenReturn(true);
            when(player.hasPermission("VotingPlugin.Commands.AdminVote.NPC")).thenReturn(true);
            World world = mock(World.class); when(world.getUID()).thenReturn(new UUID(1, 0));
            when(player.getLocation()).thenReturn(new Location(world, 4, 64, 4));
            return player;
        }
        CommandHandler command(String action, int arguments) {
            return handlers.stream().filter(value -> value.getArgs()[1].equals(action)
                    && value.getArgs().length == arguments).findFirst().orElseThrow();
        }
    }

    @Test void usesOnlyDedicatedNpcPatternsAndExplicitAdminPermission() {
        var f = new Fixture();
        assertEquals(List.of(List.of("NPC", "Create", "(string)"), List.of("NPC", "Create", "(string)", "(list)"),
                List.of("NPC", "List"), List.of("NPC", "Inspect", "(npcid)"), List.of("NPC", "Remove", "(npcid)")),
                f.handlers.stream().map(value -> List.of(value.getArgs())).toList());
        for (var handler : f.handlers) assertEquals("VotingPlugin.Commands.AdminVote.NPC|VotingPlugin.Admin", handler.getPerm());
    }

    @Test void capturesPlayerStateOnlyOnOwnerAndSuccessOnlyAfterDurableCompletion() {
        var f = new Fixture(); Player player = f.player();
        var completion = new CompletableFuture<VotingNPCDefinition>();
        when(f.manager.create(any())).thenReturn(completion);
        f.command("Create", 4).execute(player, new String[] { "NPC", "Create", "voting", "Vote", "here" });
        verify(player, never()).getLocation(); verify(f.manager, never()).create(any());
        f.tasks.remove().run();
        var definition = ArgumentCaptor.forClass(VotingNPCDefinition.class); verify(f.manager).create(definition.capture());
        assertEquals("Vote here", definition.getValue().name()); assertEquals("voting", definition.getValue().id());
        verify(player, never()).sendMessage(anyString());
        completion.complete(definition.getValue());
        verify(player, never()).sendMessage(anyString());
        f.tasks.remove().run();
        verify(player).sendMessage(contains("Saved voting NPC 'voting'"));
    }

    @Test void delayedPermissionRevocationAndDisabledFeaturePreventMutation() {
        var f = new Fixture(); Player player = f.player();
        var handler = f.command("Create", 3);
        handler.execute(player, new String[] { "NPC", "Create", "voting" });
        when(player.hasPermission("VotingPlugin.Commands.AdminVote.NPC")).thenReturn(false);
        f.tasks.remove().run(); verify(f.manager, never()).create(any());
        when(player.hasPermission("VotingPlugin.Commands.AdminVote.NPC")).thenReturn(true);
        when(f.manager.available()).thenReturn(false);
        handler.execute(player, new String[] { "NPC", "Create", "voting" }); f.tasks.remove().run();
        verify(f.manager, never()).create(any()); verify(player).sendMessage(contains("VotingNPCs.Enabled"));
    }

    @Test void rejectedStorageDoesNotSendSuccessAndDoesNotLeakRawFailure() {
        var f = new Fixture(); Player player = f.player();
        when(f.manager.remove("voting")).thenReturn(CompletableFuture.failedFuture(new IllegalStateException("Synthetic private diagnostic")));
        f.command("Remove", 3).execute(player, new String[] { "NPC", "Remove", "voting" });
        f.tasks.remove().run(); f.tasks.remove().run();
        verify(player).sendMessage(contains("was not confirmed"));
        verify(player, never()).sendMessage(contains("Synthetic private"));
        verify(player, never()).sendMessage(startsWith("Removed saved"));
    }

    @Test void invalidDefinitionAndConsoleCreationDoNotMutate() {
        var f = new Fixture(); Player player = f.player();
        f.command("Create", 3).execute(player, new String[] { "NPC", "Create", "bad id" }); f.tasks.remove().run();
        verify(f.manager, never()).create(any()); verify(player).sendMessage(contains("lowercase ID"));
        CommandSender console = mock(CommandSender.class); when(console.hasPermission("VotingPlugin.Admin")).thenReturn(true);
        f.command("Create", 3).execute(console, new String[] { "NPC", "Create", "voting" }); f.tasks.remove().run();
        verify(console).sendMessage("Create a voting NPC from in game."); verify(f.manager, never()).create(any());
    }

    @Test void bothCreationFormsMatchRealCommandDispatchWithStandardFrameworkPlaceholders() {
        var tabs = new com.bencodez.simpleapi.command.TabCompleteHandler();
        // These are the standard placeholders AdvancedCore registers before commands.
        tabs.addTabCompleteOption("(String)", new ArrayList<>());
        tabs.addTabCompleteOption("(List)", new ArrayList<>());
        try (var tabSingleton = mockStatic(com.bencodez.simpleapi.command.TabCompleteHandler.class)) {
            tabSingleton.when(com.bencodez.simpleapi.command.TabCompleteHandler::getInstance).thenReturn(tabs);
            var f = new Fixture(); Player player = f.player();
            when(f.manager.create(any())).thenReturn(new CompletableFuture<>());
            assertTrue(f.command("Create", 3).runCommand(player, new String[] { "npc", "create", "new-voting" }));
            f.asynchronous.remove().run(); f.tasks.remove().run();
            assertTrue(f.command("Create", 4).runCommand(player, new String[] { "npc", "create", "another", "Vote", "here" }));
            f.asynchronous.remove().run(); f.tasks.remove().run();
            var values = ArgumentCaptor.forClass(VotingNPCDefinition.class); verify(f.manager, times(2)).create(values.capture());
            assertEquals(List.of("new-voting", "another"), values.getAllValues().stream().map(VotingNPCDefinition::id).toList());
        }
    }
}
