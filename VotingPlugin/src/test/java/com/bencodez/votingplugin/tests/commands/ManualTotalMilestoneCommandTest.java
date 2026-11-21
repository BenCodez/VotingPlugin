package com.bencodez.votingplugin.tests.commands;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.mockito.ArgumentMatchers.*;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.junit.jupiter.api.*;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.command.TabCompleteHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

class ManualTotalMilestoneCommandTest {
    final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
    final CommandSender sender = mock(CommandSender.class);
    final ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
    final ArrayDeque<Runnable> pending = new ArrayDeque<>();
    final VotingPluginUser user = mock(VotingPluginUser.class);
    final AtomicInteger total = new AtomicInteger(9);
    Field singleton;
    Object previous;

    @BeforeEach void setup() throws Exception {
        singleton = TabCompleteHandler.class.getDeclaredField("instance");
        singleton.setAccessible(true); previous = singleton.get(null);
        singleton.set(null, new TabCompleteHandler());
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>());
        when(plugin.getUserManager().getDataManager().getTimer()).thenReturn(worker);
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("Example")).thenReturn(user);
        when(sender.hasPermission(anyString())).thenReturn(true);
        doAnswer(c -> { pending.add(c.getArgument(0)); return null; }).when(worker).execute(any(Runnable.class));
        when(user.getTotal(TopVoter.AllTime)).thenAnswer(c -> total.get());
        doAnswer(c -> { total.set(c.getArgument(1)); return null; }).when(user).setTotal(eq(TopVoter.AllTime), anyInt());
        register("register");
    }
    @AfterEach void restore() throws Exception { singleton.set(null, previous); }

    void register(String name) throws Exception {
        Class<?> type = Class.forName("com.bencodez.votingplugin.commands.VoteMilestoneCheckCommands");
        Method method = type.getDeclaredMethod(name, VotingPluginMain.class, String.class);
        method.setAccessible(true); method.invoke(null, plugin, "VotingPlugin.Admin");
    }
    CommandHandler command(String action) {
        return plugin.getAdminVoteCommand().stream().filter(h -> h.getArgs()[2].equals(action))
                .filter(h -> h.getArgs().length == 3 || h.getArgs()[3].equals("AllTime")).findFirst().orElseThrow();
    }
    void dispatch(String... args) {
        try (var bukkit = mockStatic(Bukkit.class)) {
            bukkit.when(Bukkit::getOnlinePlayers).thenReturn(List.of());
            assertTrue(command(args[2]).runCommand(sender, args));
        }
    }
    @Test void checkFollowsWorkAlreadyAdmittedToTheStorageWorker() {
        pending.add(() -> total.set(10));
        dispatch("User", "Example", "CheckMilestones");
        assertEquals(2, pending.size());
        verifyNoInteractions(plugin.getVoteMilestonesManager());
        assertEquals(9, total.get());
        pending.remove().run();
        var manager = plugin.getVoteMilestonesManager();
        doAnswer(c -> { assertEquals(10, total.get()); return null; }).when(manager).checkCurrentVoteTotals(user);
        pending.remove().run();
        verify(plugin.getVoteMilestonesManager()).checkCurrentVoteTotals(user);
        verify(user, never()).setTotal(any(), anyInt());
    }
    @Test void deniedAndMalformedInputsNeverReachStorage() {
        var denied = mock(org.bukkit.entity.Player.class);
        assertTrue(command("CheckMilestones").runCommand(denied, new String[] { "User", "Example", "CheckMilestones" }));
        assertTrue(pending.isEmpty());
        dispatch("User", "bad name", "CheckMilestones");
        assertFalse(command("CheckMilestones").runCommand(sender, new String[] { "User", "Example", "CheckMilestones", "extra" }));
        assertTrue(pending.isEmpty());
    }
    @Test void exactOfflineUuidIsResolvedOnlyOnWorker() {
        UUID id = UUID.randomUUID();
        when(plugin.getVotingPluginUserManager().getVotingPluginUser(id, false)).thenReturn(user);
        dispatch("User", id.toString(), "CheckMilestones");
        verify(plugin.getVotingPluginUserManager(), never()).getVotingPluginUser(id, false);
        pending.remove().run();
        verify(plugin.getVoteMilestonesManager()).checkCurrentVoteTotals(user);
    }
    @Test void proxyModeRefusesLocalEligibility() {
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(true);
        dispatch("User", "Example", "CheckMilestones"); pending.remove().run();
        verifyNoInteractions(plugin.getVoteMilestonesManager());
    }
    @Test void rejectedWorkerNeverChecksAndStorageFailureDoesNotReportCompletion() {
        doThrow(new RejectedExecutionException()).when(worker).execute(any(Runnable.class));
        dispatch("User", "Example", "CheckMilestones");
        verifyNoInteractions(plugin.getVoteMilestonesManager());
        assertTrue(pending.isEmpty());
    }
    @Test void replySchedulerRejectionDoesNotRepeatTheEvaluation() {
        var scheduler = plugin.getBukkitScheduler();
        doThrow(new RejectedExecutionException()).when(scheduler).runTask(eq(plugin), any(Runnable.class));
        dispatch("User", "Example", "CheckMilestones"); pending.remove().run();
        verify(plugin.getVoteMilestonesManager(), times(1)).checkCurrentVoteTotals(user);
        assertTrue(pending.isEmpty());
        verify(sender, never()).sendMessage(contains("eligibility checked"));
    }
    @Test void workerReadFailureNeverEvaluatesRewards() {
        when(plugin.getVotingPluginUserManager().getVotingPluginUser("Example")).thenThrow(new IllegalStateException());
        dispatch("User", "Example", "CheckMilestones"); pending.remove().run();
        verifyNoInteractions(plugin.getVoteMilestonesManager());
    }
}
