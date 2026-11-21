package com.bencodez.votingplugin.tests.commands;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.*;

import com.bencodez.advancedcore.AdvancedCorePlugin;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.user.VotingPluginUser;

class ManualTotalMilestoneCommandTest {
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final CommandSender sender = mock(CommandSender.class);
    final ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
    final ArrayDeque<Runnable> pending = new ArrayDeque<>();
    final ArrayDeque<Runnable> framework = new ArrayDeque<>();
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final AtomicInteger total = new AtomicInteger(9);
    Field singleton;
    Object previous;
    AdvancedCorePlugin oldCore;

    @BeforeEach void setup() throws Exception {
        singleton = TabCompleteHandler.class.getDeclaredField("instance");
        singleton.setAccessible(true); previous = singleton.get(null);
        var completions = new TabCompleteHandler();
        completions.addTabCompleteOption("(player)", new ArrayList<>());
        singleton.set(null, completions);
        oldCore = AdvancedCorePlugin.getInstance();
        AdvancedCorePlugin.setInstance(plugin);
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        when(plugin.getOptions().isMultiplePermissionChecks()).thenReturn(true);
        when(plugin.getOptions().getFormatNoPerms()).thenReturn("denied");
        when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>());
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(worker);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("Example")).thenReturn(user);
        when(sender.hasPermission(anyString())).thenReturn(true);
        doAnswer(c -> { pending.add(c.getArgument(0)); return null; }).when(worker).execute(any(Runnable.class));
        var scheduler = plugin.getBukkitScheduler();
        doAnswer(c -> { framework.add(c.getArgument(1)); return null; }).when(scheduler)
                .runTaskAsynchronously(eq(plugin), any(Runnable.class));
        var sharedCommands = mock(com.bencodez.advancedcore.command.CommandLoader.class);
        when(sharedCommands.getBasicAdminCommands("VotingPlugin")).thenReturn(new ArrayList<>());
        try (var shared = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            shared.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(sharedCommands);
            Method method = CommandLoader.class.getDeclaredMethod("loadAdminVoteCommand");
            method.setAccessible(true); method.invoke(new CommandLoader(plugin));
        }
    }
    @AfterEach void restore() throws Exception {
        if (singleton != null) singleton.set(null, previous);
        AdvancedCorePlugin.setInstance(oldCore);
    }
    CommandHandler command() {
        return plugin.getAdminVoteCommand().stream()
                .filter(h -> java.util.Arrays.equals(h.getArgs(), new String[] {"VoteMilestone&VM", "Check", "(player)"}))
                .findFirst().orElseThrow();
    }
    void dispatch(String... args) {
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            assertTrue(command().runCommand(sender, args));
        }
        framework.remove().run();
    }
    @Test void inlineRegistrationUsesMilestoneConventionsAndRemovesOldSyntax() {
        assertEquals("VotingPlugin.Commands.AdminVote.VoteMilestone.Check|VotingPlugin.Admin", command().getPerm());
        assertFalse(command().runCommand(sender, new String[] {"User", "Example", "CheckMilestones"}));
        assertFalse(command().runCommand(sender, new String[] {"VoteMilestone", "Check"}));
        assertFalse(command().runCommand(sender, new String[] {"VoteMilestone", "Check", "Example", "extra"}));
        assertTrue(pending.isEmpty()); assertTrue(framework.isEmpty());
    }
    @Test void checkFollowsWorkAlreadyAdmittedToTheStorageWorker() {
        pending.add(() -> total.set(10));
        dispatch("VoteMilestone", "Check", "Example");
        assertEquals(2, pending.size());
        verifyNoInteractions(plugin.getVoteMilestonesManager());
        assertEquals(9, total.get());
        pending.remove().run();
        var manager = plugin.getVoteMilestonesManager();
        doAnswer(c -> { assertEquals(10, total.get()); return null; }).when(manager).checkCurrentVoteTotals(user);
        pending.remove().run();
        verify(manager).checkCurrentVoteTotals(user);
    }
    @Test void deniedSenderNeverReachesFrameworkOrStorage() {
        var denied = mock(Player.class);
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            assertTrue(command().runCommand(denied, new String[] {"VoteMilestone", "Check", "Example"}));
        }
        assertTrue(pending.isEmpty()); assertTrue(framework.isEmpty());
    }
    @Test void bothDedicatedAndAdminPermissionsAuthorizeTheHandler() {
        for (String permission : List.of("VotingPlugin.Commands.AdminVote.VoteMilestone.Check", "VotingPlugin.Admin")) {
            var allowed = mock(Player.class);
            when(allowed.hasPermission(permission)).thenReturn(true);
            try (var bukkit = mockStatic(Bukkit.class)) {
                bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
                assertTrue(command().runCommand(allowed, new String[] {"VM", "Check", "Example"}));
            }
            assertEquals(1, framework.size()); framework.remove().run();
            assertEquals(1, pending.size()); pending.clear();
        }
    }
    @Test void offlinePlayerNameAndCaseInsensitiveAliasUseNormalResolutionOnWorker() {
        dispatch("vM", "cHeCk", "Example");
        verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser("Example");
        pending.remove().run();
        verify(plugin.getVoteMilestonesManager()).checkCurrentVoteTotals(user);
    }
    @Test void playerArgumentUsesTheSameOnlineNameCompletionAsAdjacentCommands() {
        var online = mock(Player.class); when(online.getName()).thenReturn("Example");
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(() -> Bukkit.getPlayer("Exam")).thenReturn(online);
            assertTrue(command().runCommand(sender, new String[] {"VoteMilestone", "Check", "Exam"}));
        }
        framework.remove().run(); pending.remove().run();
        verify(plugin.getVotingPluginUserManager()).getVotingPluginUser("Example");
    }
    @Test void proxyModeRefusesLocalEligibility() {
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        dispatch("VoteMilestone", "Check", "Example"); pending.remove().run();
        verifyNoInteractions(plugin.getVoteMilestonesManager());
    }
    @Test void rejectedWorkerNeverChecks() {
        doThrow(new RejectedExecutionException()).when(worker).execute(any(Runnable.class));
        dispatch("VoteMilestone", "Check", "Example");
        verifyNoInteractions(plugin.getVoteMilestonesManager()); assertTrue(pending.isEmpty());
    }
    @Test void replySchedulerRejectionDoesNotRepeatTheEvaluation() {
        var scheduler = plugin.getBukkitScheduler();
        doThrow(new RejectedExecutionException()).when(scheduler).runTask(eq(plugin), any(Runnable.class));
        dispatch("VoteMilestone", "Check", "Example"); pending.remove().run();
        verify(plugin.getVoteMilestonesManager(), times(1)).checkCurrentVoteTotals(user);
        assertTrue(pending.isEmpty());
        verify(sender, never()).sendMessage(contains("eligibility checked"));
    }
    @Test void workerReadFailureNeverEvaluatesRewards() {
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("Example")).thenThrow(new IllegalStateException());
        dispatch("VoteMilestone", "Check", "Example"); pending.remove().run();
        verifyNoInteractions(plugin.getVoteMilestonesManager());
    }
}
