package com.bencodez.votingplugin.commands;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;

class GuidedSessionCommandRegistrationTest {
    @Test void disabledGuidePreservesTheExistingCustomSessionHandler() throws Exception {
        Fixture f = new Fixture();
        f.register();
        assertEquals(1, f.sessionHandlers().size());
        assertEquals("existing owner command", f.sessionHandlers().get(0).getHelpMessage());
        assertFalse(f.plugin.getVoteCommand().stream().anyMatch(h -> h.getArgs().length == 2
                && h.getArgs()[0].equalsIgnoreCase("Session")));
    }

    @Test void enabledGuideReservesSessionAndAllNavigationHandlers() throws Exception {
        Fixture f = new Fixture();
        f.data.set("GuidedVotingSession.Enabled", true);
        f.register();
        assertEquals(8, f.sessionHandlers().size()); // seven built-ins plus the owner custom command
        assertEquals("Guided voting session", f.sessionHandlers().get(0).getHelpMessage());
        for (String action : List.of("next", "previous", "skip", "check", "finish", "restart")) {
            assertTrue(f.sessionHandlers().stream().anyMatch(h -> h.getArgs().length == 2
                    && h.getArgs()[1].equals(action)));
        }
    }

    @Test void disablingAndReloadingRestoresCustomRoutingWithoutDuplicateHandlers() throws Exception {
        Fixture f = new Fixture();
        f.data.set("GuidedVotingSession.Enabled", true);
        f.register();
        f.data.set("GuidedVotingSession.Enabled", false);
        f.loader.refreshGuidedSessionCommands();
        assertEquals(1, f.sessionHandlers().size());
        assertEquals("existing owner command", f.sessionHandlers().get(0).getHelpMessage());
    }

    @Test void enablingOnReloadAddsOnlyGuideHandlersAndPreservesProductionIdentity() throws Exception {
        Fixture f = new Fixture();
        f.register();
        var production = new ArrayList<>(f.plugin.getVoteCommand());
        f.data.set("GuidedVotingSession.Enabled", true);
        f.loader.refreshGuidedSessionCommands();
        f.loader.refreshGuidedSessionCommands();
        assertEquals(production.size() + 7, f.plugin.getVoteCommand().size());
        for (CommandHandler original : production) assertTrue(f.plugin.getVoteCommand().contains(original));
        assertEquals("Guided voting session", f.sessionHandlers().get(0).getHelpMessage());
    }

    @Test void reloadHonorsDisabledGuidePermissionWithoutRemovingCustomHandler() throws Exception {
        Fixture f = new Fixture(); f.register();
        f.data.set("GuidedVotingSession.Enabled", true);
        when(f.config.getDisabledCommands()).thenReturn(new ArrayList<>(List.of("VotingPlugin.Commands.Vote.Session")));
        f.loader.refreshGuidedSessionCommands();
        assertEquals(1, f.sessionHandlers().size());
        assertEquals("existing owner command", f.sessionHandlers().get(0).getHelpMessage());
    }

    @Test void permissionLookupTracksEnableDisableAndRetainsUnrelatedCommands() throws Exception {
        Fixture f = new Fixture(); f.register();
        org.bukkit.entity.Player owner = mock(org.bukkit.entity.Player.class);
        when(owner.hasPermission("owner.session")).thenReturn(true);
        org.bukkit.entity.Player voter = mock(org.bukkit.entity.Player.class);
        when(voter.hasPermission("VotingPlugin.Commands.Vote.Session")).thenReturn(true);
        var index = new java.util.HashMap<String, CommandHandler>();
        CommandHandler custom = f.sessionHandlers().get(0);
        CommandHandler unrelated = f.plugin.getVoteCommand().get(0);
        index.put("voteSession", custom);
        index.put("voteHelp", unrelated);
        f.loader.setCommands(index);
        assertTrue(f.loader.hasPermission(owner, "voteSession"));
        assertFalse(f.loader.hasPermission(voter, "voteSession"));
        f.data.set("GuidedVotingSession.Enabled", true);
        f.loader.refreshGuidedSessionCommands();
        assertFalse(f.loader.hasPermission(owner, "voteSession"));
        assertTrue(f.loader.hasPermission(voter, "votingplugin:voteSession"));
        assertSame(unrelated, index.get("voteHelp"));
        f.data.set("GuidedVotingSession.Enabled", false);
        f.loader.refreshGuidedSessionCommands();
        assertSame(custom, index.get("voteSession"));
        assertTrue(f.loader.hasPermission(owner, "voteSession"));
        assertFalse(f.loader.hasPermission(voter, "voteSession"));
    }

    @Test void initialAliasIndexUsesGuideRatherThanCustomOrNavigationHandler() throws Exception {
        Fixture f = new Fixture();
        f.data.set("GuidedVotingSession.Enabled", true);
        f.register();
        when(f.plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>());
        when(f.plugin.getDescription().getCommands()).thenReturn(java.util.Map.of());
        try (var bukkit = mockStatic(org.bukkit.Bukkit.class)) {
            f.loader.loadAliases();
        }
        CommandHandler indexed = f.loader.getCommands().get("voteSession");
        assertEquals("Guided voting session", indexed.getHelpMessage());
        assertArrayEquals(new String[] {"Session"}, indexed.getArgs());
        org.bukkit.entity.Player voter = mock(org.bukkit.entity.Player.class);
        when(voter.hasPermission("VotingPlugin.Commands.Vote.Session")).thenReturn(true);
        assertTrue(f.loader.hasPermission(voter, "voteSession"));
    }

    @Test void enablingOnReloadHonorsDisabledDefaultPermission() throws Exception {
        Fixture f = new Fixture(); f.register();
        when(f.plugin.getOptions().isMultiplePermissionChecks()).thenReturn(true);
        when(f.config.getDisabledDefaultPermissions()).thenReturn(new ArrayList<>(List.of("VotingPlugin.Commands.Vote.Session")));
        f.loader.setCommands(new java.util.HashMap<>());
        f.data.set("GuidedVotingSession.Enabled", true);
        f.loader.refreshGuidedSessionCommands();
        org.bukkit.entity.Player player = mock(org.bukkit.entity.Player.class);
        when(player.hasPermission("VotingPlugin.Player")).thenReturn(true);
        assertFalse(f.loader.hasPermission(player, "voteSession"));
        assertTrue(f.sessionHandlers().stream().filter(h -> h.getHelpMessage().equals("Guided voting session"))
                .allMatch(h -> h.getPerm().equals("VotingPlugin.Commands.Vote.Session")));
        when(player.hasPermission("VotingPlugin.Commands.Vote.Session")).thenReturn(true);
        assertTrue(f.loader.hasPermission(player, "voteSession"));
    }

    @Test void reloadReconcilesOnlyGuidePermissionInheritanceAndInvalidatesStaleTasks() throws Exception {
        Fixture f = new Fixture(); f.register();
        java.util.List<Runnable> tasks = new ArrayList<>();
        var scheduler = f.plugin.getBukkitScheduler();
        doAnswer(call -> { tasks.add(call.getArgument(1)); return null; })
                .when(scheduler).runTask(eq(f.plugin), any(Runnable.class));
        var manager = mock(org.bukkit.plugin.PluginManager.class);
        try (var bukkit = mockStatic(org.bukkit.Bukkit.class)) {
            bukkit.when(org.bukkit.Bukkit::getPluginManager).thenReturn(manager);
            var server = mock(org.bukkit.Server.class);
            when(server.getPluginManager()).thenReturn(manager);
            bukkit.when(org.bukkit.Bukkit::getServer).thenReturn(server);
            var parent = new org.bukkit.permissions.Permission("VotingPlugin.Player");
            parent.getChildren().put("owner.unrelated", true);
            when(manager.getPermission("VotingPlugin.Player")).thenReturn(parent);
            f.data.set("GuidedVotingSession.Enabled", true);
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            assertEquals(true, parent.getChildren().get("VotingPlugin.Commands.Vote.Session"));
            when(f.config.getDisabledDefaultPermissions()).thenReturn(new ArrayList<>(List.of("VotingPlugin.Commands.Vote.Session")));
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            assertFalse(parent.getChildren().containsKey("VotingPlugin.Commands.Vote.Session"));
            when(f.config.getDisabledDefaultPermissions()).thenReturn(new ArrayList<>());
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            assertEquals(true, parent.getChildren().get("VotingPlugin.Commands.Vote.Session"));
            f.loader.refreshGuidedSessionCommands();
            Runnable staleEnable = tasks.remove(0);
            f.data.set("GuidedVotingSession.Enabled", false);
            f.loader.refreshGuidedSessionCommands();
            Runnable staleDisable = tasks.remove(0);
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            staleEnable.run();
            staleDisable.run();
            assertFalse(parent.getChildren().containsKey("VotingPlugin.Commands.Vote.Session"));
            assertEquals(true, parent.getChildren().get("owner.unrelated"));
        }
    }

    @Test void disablingGuideRetainsCustomSessionInheritanceUnlessOwnerDisablesDefaultPermission() throws Exception {
        Fixture f = new Fixture();
        f.data.set("CustomCommands.owner-session.Permission", "VotingPlugin.Commands.Vote.Session|VotingPlugin.Player");
        f.register();
        java.util.List<Runnable> tasks = new ArrayList<>();
        var scheduler = f.plugin.getBukkitScheduler();
        doAnswer(call -> { tasks.add(call.getArgument(1)); return null; })
                .when(scheduler).runTask(eq(f.plugin), any(Runnable.class));
        var manager = mock(org.bukkit.plugin.PluginManager.class);
        try (var bukkit = mockStatic(org.bukkit.Bukkit.class)) {
            var server = mock(org.bukkit.Server.class);
            when(server.getPluginManager()).thenReturn(manager);
            bukkit.when(org.bukkit.Bukkit::getServer).thenReturn(server);
            bukkit.when(org.bukkit.Bukkit::getPluginManager).thenReturn(manager);
            var parent = new org.bukkit.permissions.Permission("VotingPlugin.Player");
            when(manager.getPermission("VotingPlugin.Player")).thenReturn(parent);
            parent.getChildren().put("VotingPlugin.Commands.Vote.Session", true);
            f.data.set("GuidedVotingSession.Enabled", true);
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            f.data.set("GuidedVotingSession.Enabled", false);
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            assertEquals(true, parent.getChildren().get("VotingPlugin.Commands.Vote.Session"));
            assertEquals("existing owner command", f.sessionHandlers().get(0).getHelpMessage());
            when(f.config.getDisabledDefaultPermissions()).thenReturn(new ArrayList<>(List.of("VotingPlugin.Commands.Vote.Session")));
            f.loader.refreshGuidedSessionCommands();
            tasks.remove(0).run();
            assertFalse(parent.getChildren().containsKey("VotingPlugin.Commands.Vote.Session"));
        }
    }

    private static final class Fixture {
        final VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        final Config config = mock(Config.class);
        final CommandLoader loader = new CommandLoader(plugin);
        final YamlConfiguration data = new YamlConfiguration();
        final AtomicReference<ArrayList<CommandHandler>> handlers = new AtomicReference<>();
        Fixture() {
            when(plugin.getConfigFile()).thenReturn(config);
            when(config.getData()).thenReturn(data);
            when(config.getDisabledCommands()).thenReturn(new ArrayList<>());
            when(config.isAddCustomCommands()).thenReturn(true);
            when(config.getCustomCommands()).thenReturn(Set.of("owner-session"));
            var custom = data.createSection("CustomCommands.owner-session");
            custom.set("Args", new ArrayList<>(List.of("Session")));
            custom.set("HelpMessage", "existing owner command");
            custom.set("Permission", "owner.session");
            when(config.getCustomCommands("owner-session")).thenReturn(custom);
            doAnswer(call -> { handlers.set(call.getArgument(0)); return null; })
                    .when(plugin).setVoteCommand(any(ArrayList.class));
            when(plugin.getVoteCommand()).thenAnswer(call -> handlers.get());
        }
        void register() throws Exception {
            Method load = CommandLoader.class.getDeclaredMethod("loadVoteCommand");
            load.setAccessible(true);
            try (var singleton = mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
                var core = mock(com.bencodez.advancedcore.command.CommandLoader.class);
                singleton.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(core);
                when(core.getBasicCommands("VotingPlugin")).thenReturn(new ArrayList<>());
                load.invoke(loader);
            }
        }
        List<CommandHandler> sessionHandlers() {
            return plugin.getVoteCommand().stream().filter(h -> h.getArgs().length > 0
                    && h.getArgs()[0].equalsIgnoreCase("Session")).toList();
        }
    }
}
