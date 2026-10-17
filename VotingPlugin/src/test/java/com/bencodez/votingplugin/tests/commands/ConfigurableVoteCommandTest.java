package com.bencodez.votingplugin.tests.commands;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

import com.bencodez.votingplugin.commands.CommandLoader;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.logging.Logger;

import org.bukkit.Bukkit;
import org.bukkit.Server;
import org.bukkit.command.Command;
import org.bukkit.command.CommandMap;
import org.bukkit.command.CommandSender;
import org.bukkit.command.PluginCommand;
import org.bukkit.command.SimpleCommandMap;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginDescriptionFile;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.advancedcore.api.command.CommandHandler;
import io.github.projectunified.unidialog.core.payload.DialogPayload;
import com.bencodez.simpleapi.dialog.UniDialogService;
import com.bencodez.simpleapi.dialog.MultiActionDialogBuilder;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.gui.player.VoteHelp;
import com.bencodez.votingplugin.commands.gui.player.VoteURL;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

class ConfigurableVoteCommandTest {
    @Test
    void absentConfigurationKeysLoadTheExistingDefaults() throws Exception {
        Config config = mock(Config.class);
        new com.bencodez.simpleapi.file.annotation.AnnotationHandler()
                .load(new org.bukkit.configuration.file.YamlConfiguration(), config);
        var enabled = Config.class.getDeclaredField("voteCommandEnabled");
        var name = Config.class.getDeclaredField("voteCommandName");
        enabled.setAccessible(true);
        name.setAccessible(true);
        assertTrue(enabled.getBoolean(config));
        assertEquals("vote", name.get(config));
    }

    @Test
    void defaultRootKeepsDispatchPermissionsStandaloneAliasesAndTabCompletion() throws Exception {
        try (Fixture f = new Fixture(true, "vote", true)) {
            PluginCommand original = f.owned.get("vote");
            f.loader.loadAliases();
            assertSame(original, f.map.getCommand("vote"));
            assertSame(original, f.map.getCommand("votingplugin:vote"));
            assertSame(f.owned.get("votegui"), f.map.getCommand("vgui"));
            assertEquals("VotingPlugin.Commands.Vote.GUI", f.owned.get("votegui").getPermission());
            assertEquals(List.of("GUI", "Help"), original.tabComplete(f.player, "vote", new String[] { "" }));
            assertTrue(f.map.dispatch(f.player, "vote GUI"));
            assertEquals(1, f.executed.get());
            assertEquals("/vote", f.plugin.getVoteCommandPrefix());
        }
    }

    @Test
    void disableRemovesAllOwnedPlayerLabelsAndNamespacesButKeepsAdminAndForeignCommands() throws Exception {
        try (Fixture f = new Fixture(false, "vote", true)) {
            Command foreign = f.foreign("votechallenge", "other");
            f.loader.loadAliases();
            for (String name : List.of("vote", "votehelp", "vhelp", "votegui", "vgui")) {
                assertNull(f.map.getCommand(name), name);
                assertNull(f.map.getCommand("votingplugin:" + name), name);
            }
            assertSame(f.owned.get("adminvote"), f.map.getCommand("adminvote"));
            assertSame(f.owned.get("av"), f.map.getCommand("av"));
            assertSame(f.owned.get("adminvotehelp"), f.map.getCommand("avhelp"));
            assertNotNull(f.owned.get("adminvotehelp").getExecutor());
            assertSame(foreign, f.map.getCommand("votechallenge"));
            assertFalse(f.plugin.isVoteCommandEnabled());
            assertFalse(f.map.dispatch(f.player, "votingplugin:vote GUI"));
            f.loader.processSlotClick(f.player, mock(VotingPluginUser.class), "help");
            verify(f.player, never()).performCommand(anyString());
            assertTrue(new VoteHelp(f.plugin, f.player, 1).helpText(f.player).isEmpty());
        }
    }

    @Test
    void renameRemovesOldRootAndRetainsCanonicalStandaloneAliases() throws Exception {
        try (Fixture f = new Fixture(true, " Ballot ", true)) {
            f.loader.loadAliases();
            assertNull(f.map.getCommand("vote"));
            assertNull(f.map.getCommand("votingplugin:vote"));
            assertSame(f.owned.get("vote"), f.map.getCommand("ballot"));
            assertSame(f.owned.get("vote"), f.map.getCommand("votingplugin:ballot"));
            assertSame(f.owned.get("votegui"), f.map.getCommand("vgui"));
            assertNull(f.map.getCommand("ballotgui"));
            assertEquals("/ballot", f.plugin.getVoteCommandPrefix());
            assertTrue(f.map.dispatch(f.player, "ballot GUI"));
            assertTrue(f.map.dispatch(f.player, "vgui"));
            assertEquals(2, f.executed.get());
            assertEquals(List.of("GUI", "Help"), f.owned.get("vote").tabComplete(f.player, "ballot", new String[] { "" }));
            assertEquals("VotingPlugin.Commands.Vote.GUI", f.owned.get("votegui").getPermission());
            assertTrue(f.loader.hasPermission(f.player, "ballot GUI"));
        }
    }

    @Test
    void syntheticPlayerAndAdminAliasesCannotReplaceRenamedRoot() throws Exception {
        for (boolean aliases : List.of(true, false)) {
            for (String name : List.of("votetogglereminders", "adminvotesynthetic")) {
                try (Fixture f = new Fixture(true, name, aliases)) {
                    f.plugin.getVoteCommand().add(f.handler("ToggleReminders", "VotingPlugin.Commands.Vote.ToggleReminders"));
                    f.plugin.getAdminVoteCommand().add(f.handler("Synthetic", "VotingPlugin.Commands.AdminVote.Synthetic"));
                    f.loader.loadAliases();
                    PluginCommand root = f.owned.get("vote");
                    assertSame(root, f.map.getCommand(name));
                    assertInstanceOf(com.bencodez.votingplugin.commands.executers.CommandVote.class, root.getExecutor());
                    assertFalse(f.loader.getAliasCommandNames().contains(name));
                    assertTrue(f.map.dispatch(f.player, name + " GUI"));
                    assertTrue(f.map.dispatch(f.player, name + " Help"));
                    assertEquals(2, f.executed.get());
                    assertTrue(root.tabComplete(f.player, name, new String[] { "" }).contains("GUI"));
                    assertNotEquals("VotingPlugin.Commands.AdminVote.Synthetic", root.getPermission());
                    assertNotEquals("VotingPlugin.Commands.Vote.ToggleReminders", root.getPermission());
                }
            }
        }
    }

    @Test
    void disabledSyntheticAliasCannotUnregisterRenamedRoot() throws Exception {
        try (Fixture f = new Fixture(true, "votetogglereminders", true)) {
            var field = CommandLoader.class.getDeclaredField("disabledAliasCommandNames");
            field.setAccessible(true);
            @SuppressWarnings("unchecked") var disabled = (java.util.Set<String>) field.get(f.loader);
            disabled.add("votetogglereminders");
            disabled.add("votegui");
            f.loader.loadAliases();
            assertSame(f.owned.get("vote"), f.map.getCommand("votetogglereminders"));
            assertSame(f.owned.get("vote"), f.map.getCommand("votingplugin:votetogglereminders"));
            assertTrue(f.loader.isVoteCommandEnabled());
            assertTrue(f.map.dispatch(f.player, "votetogglereminders Help"));
            assertEquals(1, f.executed.get());
            assertNull(f.map.getCommand("votegui"));
            assertNull(f.map.getCommand("vgui"));
        }
    }

    @Test
    void foreignBareLabelKeepsItsOwnerAndGeneratedActionsUseTheVotingPluginNamespace() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            Command foreign = f.foreign("ballot", "other");
            f.loader.loadAliases();
            assertSame(foreign, f.map.getCommand("ballot"));
            assertSame(f.owned.get("vote"), f.map.getCommand("votingplugin:ballot"));
            assertEquals("/votingplugin:ballot", f.plugin.getVoteCommandPrefix());
            f.loader.processSlotClick(f.player, mock(VotingPluginUser.class), "help");
            verify(f.player).performCommand("votingplugin:ballot help");
            assertFalse(f.loader.isVotingPluginCommand(f.player, "ballot"));
            assertTrue(f.loader.isVotingPluginCommand(f.player, "votingplugin:ballot"));
            assertTrue(f.map.dispatch(f.player, "votingplugin:ballot GUI"));
            assertEquals(1, f.executed.get());
        }
    }

    @Test
    void renamedFallbackPublishesThroughForwardingMapWithoutReplacingForeignCommand() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            f.map.useForwardingStorage();
            Command foreign = f.foreign("ballot", "other");
            f.loader.loadAliases();
            assertSame(foreign, f.map.getCommand("ballot"));
            assertSame(f.owned.get("vote"), f.map.getCommand("votingplugin:ballot"));
            assertTrue(f.plugin.isVoteCommandEnabled());
            assertEquals("/votingplugin:ballot", f.plugin.getVoteCommandPrefix());
            assertTrue(f.map.dispatch(f.player, "votingplugin:ballot GUI"));
            assertEquals(1, f.executed.get());
        }
    }

    @Test
    void occupiedNamespaceCannotBeOverwrittenEvenWhenBareLabelIsFree() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            Command foreign = f.foreign("ballot", "other");
            f.map.known().remove("ballot");
            f.map.known().put("votingplugin:ballot", foreign);
            f.loader.loadAliases();
            assertSame(foreign, f.map.getCommand("votingplugin:ballot"));
            assertNull(f.map.getCommand("ballot"));
            assertSame(f.owned.get("vote"), f.map.getCommand("vote"));
            assertEquals("/vote", f.plugin.getVoteCommandPrefix());
        }
    }

    @Test
    void foreignBareAliasCannotBeReplacedByBukkitsPrimaryCommandRegistration() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            Command foreign = f.foreign("poll", "other");
            f.map.known().put("ballot", foreign);
            f.loader.loadAliases();
            assertSame(foreign, f.map.getCommand("ballot"));
            assertSame(foreign, f.map.getCommand("poll"));
            assertSame(f.owned.get("vote"), f.map.getCommand("votingplugin:ballot"));
            assertEquals("/votingplugin:ballot", f.plugin.getVoteCommandPrefix());
            assertTrue(f.map.dispatch(f.player, "votingplugin:ballot GUI"));
            assertEquals(1, f.executed.get());
        }
    }

    @Test
    void invalidNamesAndOwnedAdminOrPlayerAliasNamesFallBackWithoutChangingThoseCommands() throws Exception {
        for (String name : List.of("", "/ballot", "ballot GUI", "other:ballot", "bad\nname", "av", "adminvote", "avhelp", "fakevote", "votegui", "vgui")) {
            try (Fixture f = new Fixture(true, name, true)) {
                f.loader.loadAliases();
                assertEquals("/vote", f.plugin.getVoteCommandPrefix(), name);
                assertSame(f.owned.get("adminvote"), f.map.getCommand("adminvote"));
                assertSame(f.owned.get("av"), f.map.getCommand("av"));
                assertSame(f.owned.get("adminvotehelp"), f.map.getCommand("avhelp"));
                assertSame(f.owned.get("votegui"), f.map.getCommand("vgui"));
            }
        }
    }

    @Test
    void disablingRestoresOtherPluginsFallbackAndRemovesDisplacedOwnedAliasObjects() throws Exception {
        try (Fixture f = new Fixture(false, "vote", true)) {
            Command foreignVote = f.foreign("vote", "other");
            Command foreignGui = f.foreign("votegui", "other");
            // Simulate both canonical labels being replaced, leaving only vgui.
            f.map.known().put("votegui", foreignGui);
            f.map.known().put("votingplugin:votegui", foreignGui);
            f.loader.loadAliases();
            assertSame(foreignVote, f.map.getCommand("vote"));
            assertSame(foreignGui, f.map.getCommand("votegui"));
            assertSame(foreignGui, f.map.getCommand("votingplugin:votegui"));
            assertNull(f.map.getCommand("vgui"));
            assertNull(f.map.getCommand("votingplugin:vgui"));
            assertNull(f.map.getCommand("votingplugin:vote"));
        }
    }

    @Test
    void aliasToggleRemovesPlayerAndAdminStandaloneAliasesButPreservesRenamedRootAndAdminRoots() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", false)) {
            Command foreign = f.foreign("votegui", "other");
            f.loader.loadAliases();
            assertSame(foreign, f.map.getCommand("votegui"));
            assertNull(f.map.getCommand("votingplugin:votegui"));
            assertNull(f.map.getCommand("vgui"));
            assertNull(f.map.getCommand("adminvotehelp"));
            assertNull(f.map.getCommand("avhelp"));
            assertSame(f.owned.get("vote"), f.map.getCommand("ballot"));
            assertSame(f.owned.get("adminvote"), f.map.getCommand("adminvote"));
            assertSame(f.owned.get("av"), f.map.getCommand("av"));
        }
    }

    @Test
    void reloadCannotChangeCapturedRootToggleAliasesHelpOrNavigation() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            f.loader.loadAliases();
            when(f.config.isVoteCommandEnabled()).thenReturn(false);
            when(f.config.getVoteCommandName()).thenReturn("newroot");
            when(f.config.isLoadCommandAliases()).thenReturn(false);
            f.loader.loadAliases();
            assertTrue(f.plugin.isVoteCommandEnabled());
            assertEquals("/ballot", f.plugin.getVoteCommandPrefix());
            assertSame(f.owned.get("vote"), f.map.getCommand("ballot"));
            assertNull(f.map.getCommand("newroot"));
            assertSame(f.owned.get("votegui"), f.map.getCommand("vgui"));
            f.loader.processSlotClick(f.player, mock(VotingPluginUser.class), "help");
            verify(f.player).performCommand("ballot help");
            VoteHelp help = new VoteHelp(f.plugin, f.player, 1);
            assertTrue(help.helpText(f.player).stream().allMatch(line -> line.getClickEvent().getValue().startsWith("/ballot ")));
            assertEquals(List.of("GUI", "Help"), f.owned.get("vote").tabComplete(f.player, "ballot", new String[] { "" }));
        }
    }

    @Test
    void disabledStartupCannotBecomeRegisteredThroughReload() throws Exception {
        try (Fixture f = new Fixture(false, "vote", true)) {
            f.loader.loadAliases();
            when(f.config.isVoteCommandEnabled()).thenReturn(true);
            when(f.config.getVoteCommandName()).thenReturn("ballot");
            f.loader.loadAliases();
            assertFalse(f.plugin.isVoteCommandEnabled());
            assertNull(f.map.getCommand("vote"));
            assertNull(f.map.getCommand("ballot"));
            assertNull(f.map.getCommand("vgui"));
        }
    }

    @Test
    void clickableAndLegacyHelpAndExistingInvalidHintUseActiveRoot() throws Exception {
        try (Fixture f = new Fixture(true, "ballot", true)) {
            f.loader.loadAliases();
            VoteHelp help = new VoteHelp(f.plugin, f.player, 1);
            assertEquals(2, help.helpText(f.player).size());
            assertEquals(2, help.helpTextLegacy(f.player).size());
            assertTrue(help.helpText(f.player).stream().allMatch(line -> line.getClickEvent().getValue().startsWith("/ballot ")));
            assertTrue(help.helpTextLegacy(f.player).stream().allMatch(line -> line.getClickEvent().getValue().startsWith("/ballot ")));
            assertEquals("see /ballot help!", f.plugin.formatVoteCommandMessage("see /vote help!"));
            assertEquals("see /ballot help!", f.plugin.formatVoteCommandMessage("see %votecommand% help!"));
            assertEquals("/votechallenge /adminvote", f.plugin.formatVoteCommandMessage("/votechallenge /adminvote"));
            f.map.dispatch(f.player, "ballot unknown");
            verify(f.player).sendMessage("see /ballot help!");
            f.map.dispatch(f.player, "vgui unknown");
            verify(f.player, times(2)).sendMessage("see /ballot help!");
        }
    }

    @Test
    void urlDialogBackButtonUsesCapturedRootAndDoesNothingWhenPlayerCommandsAreDisabled() throws Exception {
        for (boolean enabled : List.of(true, false)) {
            try (Fixture f = new Fixture(enabled, "ballot", true)) {
                f.loader.loadAliases();
                UniDialogService service = mock(UniDialogService.class);
                MultiActionDialogBuilder dialog = mock(MultiActionDialogBuilder.class, RETURNS_SELF);
                GUI gui = mock(GUI.class);
                VoteSiteManager sites = mock(VoteSiteManager.class);
                when(f.plugin.getDialogService()).thenReturn(service);
                when(service.multiAction(f.player)).thenReturn(dialog);
                when(f.plugin.getGui()).thenReturn(gui);
                when(f.plugin.getVoteSiteManager()).thenReturn(sites);
                when(sites.getVoteSitesEnabled()).thenReturn(new ArrayList<>());
                when(gui.isChestVoteURLBackButton()).thenReturn(true);
                new VoteURL(f.plugin, f.player, mock(VotingPluginUser.class), true).onDialog(f.player);
                @SuppressWarnings("unchecked")
                ArgumentCaptor<Consumer<DialogPayload>> callback = ArgumentCaptor.forClass(Consumer.class);
                verify(dialog).button(eq("&eBack"), eq("&7Return to previous menu"), callback.capture());
                callback.getValue().accept(null);
                if (enabled) verify(f.player).performCommand("ballot gui");
                else verify(f.player, never()).performCommand(anyString());
            }
        }
    }

    /** Exposes the same public method provided by CraftServer, without CraftBukkit packages. */
    public interface CommandMapAccess { CommandMap getCommandMap(); }

    private static final class TestCommandMap extends SimpleCommandMap {
        TestCommandMap(Server server) { super(server); }
        Map<String, Command> known() { return knownCommands; }
        void useForwardingStorage() throws Exception {
            var forwarding = new ForwardingStorage();
            forwarding.putAll(knownCommands);
            var field = SimpleCommandMap.class.getDeclaredField("knownCommands");
            field.setAccessible(true);
            field.set(this, forwarding);
        }
    }

    // Paper's bridge overrides the basic map operations but inherits HashMap's
    // putIfAbsent, which writes backing storage instead of the command dispatcher.
    private static final class ForwardingStorage extends HashMap<String, Command> {
        private final Map<String, Command> dispatcher = new java.util.LinkedHashMap<>();
        @Override public Command get(Object key) { return dispatcher.get(key); }
        @Override public Command put(String key, Command value) { return dispatcher.put(key, value); }
        @Override public void putAll(Map<? extends String, ? extends Command> values) { values.forEach(this::put); }
        @Override public Command remove(Object key) { return dispatcher.remove(key); }
        @Override public boolean remove(Object key, Object value) { return dispatcher.remove(key, value); }
        @Override public boolean containsKey(Object key) { return dispatcher.containsKey(key); }
        @Override public boolean containsValue(Object value) { return dispatcher.containsValue(value); }
        @Override public int size() { return dispatcher.size(); }
        @Override public boolean isEmpty() { return dispatcher.isEmpty(); }
        @Override public void clear() { dispatcher.clear(); }
        @Override public java.util.Set<Map.Entry<String, Command>> entrySet() { return dispatcher.entrySet(); }
        @Override public java.util.Set<String> keySet() { return dispatcher.keySet(); }
        @Override public Collection<Command> values() { return dispatcher.values(); }
    }

    private static final class Fixture implements AutoCloseable {
        final VotingPluginMain previousPlugin = VotingPluginMain.plugin;
        final VotingPluginMain plugin = mock(VotingPluginMain.class);
        final Config config = mock(Config.class);
        final Player player = mock(Player.class);
        final MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class);
        final TestCommandMap map;
        final Map<String, PluginCommand> owned = new HashMap<>();
        final CommandLoader loader;
        final AtomicInteger executed = new AtomicInteger();

        Fixture(boolean enabled, String name, boolean aliases) throws Exception {
            VotingPluginMain.plugin = plugin;
            try {
            Server server = mock(Server.class, withSettings().extraInterfaces(CommandMapAccess.class));
            PluginManager manager = mock(PluginManager.class);
            Logger logger = Logger.getLogger("ConfigurableVoteCommandTest");
            bukkit.when(Bukkit::getServer).thenReturn(server);
            bukkit.when(Bukkit::getPluginManager).thenReturn(manager);
            bukkit.when(Bukkit::getLogger).thenReturn(logger);
            bukkit.when(Bukkit::getVersion).thenReturn("Mock (MC: 1.21.9)");
            bukkit.when(Bukkit::getBukkitVersion).thenReturn("1.21.9-R0.1-SNAPSHOT");
            map = new TestCommandMap(server);
            when(((CommandMapAccess) server).getCommandMap()).thenReturn(map);
            when(plugin.getServer()).thenReturn(server);
            when(plugin.getConfigFile()).thenReturn(config);
            when(plugin.getLogger()).thenReturn(logger);
            when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getName()).thenReturn("VotingPlugin");
            PluginDescriptionFile description = new PluginDescriptionFile(getClass().getResourceAsStream("/plugin.yml"));
            when(plugin.getDescription()).thenReturn(description);
            AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
            when(plugin.getOptions()).thenReturn(options);
            when(options.isMultiplePermissionChecks()).thenReturn(true);
            when(config.isVoteCommandEnabled()).thenReturn(enabled);
            when(config.getVoteCommandName()).thenReturn(name);
            when(config.isLoadCommandAliases()).thenReturn(aliases);
            when(config.getFormatCommandsVoteHelpHoverColor()).thenReturn("aqua");
            when(config.getFormatCommandsVoteHelpLine()).thenReturn("%Command% : %HelpMessage%");
            when(config.getFormatInvalidCommandVote()).thenReturn("see /vote help!");
            BukkitScheduler scheduler = mock(BukkitScheduler.class);
            when(plugin.getBukkitScheduler()).thenReturn(scheduler);
            doAnswer(call -> { ((Runnable) call.getArgument(1)).run(); return null; })
                    .when(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
            ArrayList<CommandHandler> playerHandlers = new ArrayList<>();
            playerHandlers.add(handler("GUI", "VotingPlugin.Commands.Vote.GUI"));
            playerHandlers.add(handler("Help", "VotingPlugin.Commands.Vote.Help"));
            when(plugin.getVoteCommand()).thenReturn(playerHandlers);
            var adminHandlers = new ArrayList<>(List.of(handler("Help", "VotingPlugin.Commands.AdminVote.Help")));
            when(plugin.getAdminVoteCommand()).thenReturn(adminHandlers);
            when(player.hasPermission(anyString())).thenReturn(true);
            for (var entry : description.getCommands().entrySet()) {
                PluginCommand command = command(entry.getKey(), plugin);
                Object declaredAliases = entry.getValue().get("aliases");
                if (declaredAliases instanceof String alias) command.setAliases(List.of(alias));
                else if (declaredAliases instanceof Collection<?> values)
                    command.setAliases(values.stream().map(String::valueOf).toList());
                owned.put(entry.getKey(), command);
                map.register("votingplugin", command);
            }
            when(plugin.getCommand(anyString())).thenAnswer(call -> {
                String key = ((String) call.getArgument(0)).toLowerCase(java.util.Locale.ROOT);
                Command command = map.getCommand(key);
                if (!(command instanceof PluginCommand result) || result.getPlugin() != plugin)
                    command = map.getCommand("votingplugin:" + key);
                return command instanceof PluginCommand result && result.getPlugin() == plugin ? result : null;
            });
            loader = new CommandLoader(plugin);
            var field = VotingPluginMain.class.getDeclaredField("commandLoader");
            field.setAccessible(true);
            field.set(plugin, loader);
            when(plugin.getCommandLoader()).thenReturn(loader);
            when(plugin.getVoteCommandPrefix()).thenCallRealMethod();
            when(plugin.isVoteCommandEnabled()).thenCallRealMethod();
            when(plugin.formatVoteCommandMessage(anyString())).thenCallRealMethod();
            } catch (Exception | Error failure) {
                bukkit.close();
                VotingPluginMain.plugin = previousPlugin;
                throw failure;
            }
        }

        CommandHandler handler(String argument, String permission) {
            return new CommandHandler(plugin, new String[] { argument }, permission, "Open menu") {
                @Override public void execute(CommandSender sender, String[] args) { executed.incrementAndGet(); }
            };
        }

        Command foreign(String label, String namespace) throws Exception {
            Plugin owner = mock(Plugin.class);
            when(owner.getName()).thenReturn("OtherPlugin");
            PluginCommand command = command(label, owner);
            map.register(namespace, command);
            return command;
        }

        static PluginCommand command(String name, Plugin owner) throws Exception {
            Constructor<PluginCommand> constructor = PluginCommand.class.getDeclaredConstructor(String.class, Plugin.class);
            constructor.setAccessible(true);
            return ReflectiveAccess.newInstance(constructor, new Object[]{name, owner});
        }

        @Override public void close() {
            bukkit.close();
            VotingPluginMain.plugin = previousPlugin;
        }
    }
}
