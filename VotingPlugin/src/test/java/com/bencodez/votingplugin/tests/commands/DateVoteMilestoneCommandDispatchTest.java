package com.bencodez.votingplugin.tests.commands;

import com.bencodez.votingplugin.commands.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.executers.CommandAdminVote;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones;
import com.bencodez.votingplugin.specialrewards.datemilestones.DateVoteMilestones.AdminAction;
import java.lang.reflect.Field;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import java.util.logging.Logger;
import org.bukkit.Bukkit;
import org.bukkit.command.Command;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DateVoteMilestoneCommandDispatchTest {
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final DateVoteMilestones manager = mock(DateVoteMilestones.class);
    final CommandSender sender = mock(CommandSender.class);
    final BukkitScheduler scheduler = mock(BukkitScheduler.class);
    final ArrayDeque<Runnable> framework = new ArrayDeque<>();
    Field instance;
    Object previous;

    @BeforeEach void setup() throws Exception {
        instance = TabCompleteHandler.class.getDeclaredField("instance");
        instance.setAccessible(true);
        previous = instance.get(null);
        var completions = new TabCompleteHandler();
        instance.set(null, completions);
        for (String token : List.of("(player)", "(number)", "(milestone)"))
            completions.addTabCompleteOption(token, new ArrayList<>());
        when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<CommandHandler>());
        when(plugin.getDateVoteMilestones()).thenReturn(manager);
        when(plugin.getBukkitScheduler()).thenReturn(scheduler);
        when(plugin.getConfigFile().getFormatInvalidCommandAdminVote()).thenReturn("invalid command");
        when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        when(sender.hasPermission(anyString())).thenReturn(true);
        doAnswer(call -> { framework.add(call.getArgument(1)); return null; })
                .when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
        DateVoteMilestoneCommandsAccess.register(plugin, "VotingPlugin.Admin");
        for (int index = 0; index < plugin.getAdminVoteCommand().size(); index++) {
            var handler = spy(plugin.getAdminVoteCommand().get(index));
            doReturn("denied").when(handler).formatNoPerms();
            plugin.getAdminVoteCommand().set(index, handler);
        }
    }

    @AfterEach void restore() throws Exception { instance.set(null, previous); }

    @Test void everyActionAndAliasDispatchesThroughTheActualSdkMatcher() {
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            for (AdminAction action : AdminAction.values()) {
                for (String root : List.of("DateVoteMilestone", "dVm")) {
                    clearInvocations(manager, sender);
                    var args = new ArrayList<>(List.of(root, action.name().toLowerCase(Locale.ROOT),
                            "Example", "festival_With_Underscores"));
                    String value = switch (action) {
                        case Status, Preview -> null;
                        case SetVotes -> "3";
                        case Force, ForceNoLimits -> "starter_With_Underscores";
                    };
                    if (value != null) args.add(value);
                    new CommandAdminVote(plugin).onCommand(sender, mock(Command.class), "av", args.toArray(String[]::new));
                    assertEquals(1, framework.size(), root + " " + action + " must reach SDK scheduling");
                    framework.remove().run();
                    verify(manager).administer(sender, action, "Example", "festival_With_Underscores", value);
                    verify(sender, never()).sendMessage("invalid command");
                }
            }
        }
    }

    @Test void exactTargetsReachTheWorkerWithoutSdkFuzzyPlayerLookup() {
        try (var bukkit = mockStatic(Bukkit.class)) {
            var nearby = mock(Player.class);
            when(nearby.getName()).thenReturn("Example");
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of(nearby));
            bukkit.when(() -> Bukkit.getPlayer("Exam")).thenReturn(nearby);
            for (String target : List.of("Exam", "EXample", UUID.randomUUID().toString())) {
                clearInvocations(manager);
                new CommandAdminVote(plugin).onCommand(sender, mock(Command.class), "av",
                        new String[] { "DVM", "Status", target, "festival_With_Underscores" });
                assertEquals(1, framework.size());
                framework.remove().run();
                verify(manager).administer(sender, AdminAction.Status, target, "festival_With_Underscores", null);
            }
            bukkit.verifyNoInteractions();
        }
    }

    @Test void deniedPlayerCannotDispatchAnyAction() {
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            var player = mock(Player.class);
            when(player.getName()).thenReturn("Example");
            for (AdminAction action : AdminAction.values()) {
                var args = new ArrayList<>(List.of("DVM", action.name(), "Example", "festival_With_Underscores"));
                if (action == AdminAction.SetVotes) args.add("3");
                if (action == AdminAction.Force || action == AdminAction.ForceNoLimits) args.add("starter");
                new CommandAdminVote(plugin).onCommand(player, mock(Command.class), "av", args.toArray(String[]::new));
            }
            assertTrue(framework.isEmpty());
            verifyNoInteractions(manager);
            verify(player, times(5)).sendMessage("denied");
        }
    }

    @Test void eachActionAcceptsEitherItsOwnPermissionOrTheDocumentedAdminFallback() {
        try (var bukkit = mockStatic(Bukkit.class)) {
            for (AdminAction action : AdminAction.values()) {
                for (String permission : List.of("VotingPlugin.Admin",
                        "VotingPlugin.Commands.AdminVote.DateVoteMilestone." + action.name())) {
                    clearInvocations(manager);
                    var player = mock(Player.class);
                    when(player.hasPermission(permission)).thenReturn(true);
                    var args = new ArrayList<>(List.of("DVM", action.name(), "Example", "festival_With_Underscores"));
                    String value = action == AdminAction.SetVotes ? "3"
                            : action == AdminAction.Force || action == AdminAction.ForceNoLimits ? "starter" : null;
                    if (value != null) args.add(value);
                    new CommandAdminVote(plugin).onCommand(player, mock(Command.class), "av", args.toArray(String[]::new));
                    assertEquals(1, framework.size(), action + " must accept " + permission);
                    framework.remove().run();
                    verify(manager).administer(player, action, "Example", "festival_With_Underscores", value);
                }
            }
            bukkit.verifyNoInteractions();
        }
    }
}
