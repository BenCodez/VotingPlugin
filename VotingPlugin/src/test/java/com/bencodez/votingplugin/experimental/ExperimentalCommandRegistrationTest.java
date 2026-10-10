package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.command.AdvancedCoreTabCompleteHandler;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;

/**
 * Exercises the experimental command registrations through the same command
 * handler objects and tab-completion implementation used by /av.
 */
class ExperimentalCommandRegistrationTest {

	private static final String ADMIN = "VotingPlugin.Admin";

	@Test
	void registersEveryExperimentalStyleAndManagementActionWithAdminPermission() throws Exception {
		VotingPluginMain plugin = commandLoaderFixture();
		invokeAdminRegistration(plugin);

		List<CommandHandler> handlers = plugin.getAdminVoteCommand();
		for (ExperimentalGUIType type : ExperimentalGUIType.values()) {
			CommandHandler handler = find(handlers, type.command());
			assertNotNull(handler, type.command());
			assertEquals(type.command(), handler.getArgs()[0].toLowerCase(),
					"the dedicated command identity must stay stable");
			assertTrue(handler.getPerm().contains(ADMIN), type.command());
		}
		for (String action : Arrays.asList("list", "close", "status", "cleanup")) {
			CommandHandler handler = find(handlers, "TestGUI", action);
			assertNotNull(handler, "testgui " + action);
			assertTrue(handler.getPerm().contains(ADMIN));
		}
		for (String action : Arrays.asList("create", "remove", "list", "inspect")) {
			CommandHandler handler = find(handlers, "testterminalgui", action);
			assertNotNull(handler, "testterminalgui " + action);
			assertTrue(handler.getPerm().contains(ADMIN));
		}
	}

	@Test
	void frameworkPermissionChecksAllowAdminAndRejectOrdinaryPlayers() throws Exception {
		VotingPluginMain plugin = commandLoaderFixture();
		invokeAdminRegistration(plugin);
		CommandHandler handler = find(plugin.getAdminVoteCommand(), "testdialoggui");
		assertNotNull(handler);

		Player admin = mock(Player.class);
		when(admin.hasPermission(ADMIN)).thenReturn(true);
		Player ordinary = mock(Player.class);
		when(ordinary.hasPermission(ADMIN)).thenReturn(false);
		assertTrue(handler.hasPerm(admin));
		assertFalse(handler.hasPerm(ordinary));
	}

	@Test
	void registeredHandlersRouteToTheExperimentalManager() throws Exception {
		VotingPluginMain plugin = commandLoaderFixture();
		ExperimentalGUIManager manager = mock(ExperimentalGUIManager.class);
		when(plugin.getExperimentalGUIManager()).thenReturn(manager);
		invokeAdminRegistration(plugin);
		Player player = mock(Player.class);
		CommandSender console = mock(CommandSender.class);

		find(plugin.getAdminVoteCommand(), "testdialoggui").execute(player, new String[] { "testdialoggui" });
		verify(manager).open(player, ExperimentalGUIType.NATIVE_DIALOG);
		find(plugin.getAdminVoteCommand(), "TestGUI", "status").execute(console,
				new String[] { "TestGUI", "status" });
		verify(manager).control(console, "status");
		find(plugin.getAdminVoteCommand(), "testterminalgui", "inspect").execute(console,
				new String[] { "testterminalgui", "inspect" });
		verify(manager).terminalControl(console, "inspect");
	}

	@Test
	void tabCompletionComesFromRegisteredHandlersAndPreservesExistingAdminCommands() throws Exception {
		VotingPluginMain plugin = commandLoaderFixture();
		invokeAdminRegistration(plugin);
		CommandSender sender = mock(CommandSender.class);
		when(sender.hasPermission(ADMIN)).thenReturn(true);
		ArrayList<CommandHandler> handlers = new ArrayList<>(plugin.getAdminVoteCommand());

		List<String> roots = AdvancedCoreTabCompleteHandler.getInstance()
				.getTabCompleteOptions(handlers, sender, new String[] { "test" }, 0);
		assertTrue(roots.stream().anyMatch(value -> value.equalsIgnoreCase("testhologram")));
		assertTrue(roots.stream().anyMatch(value -> value.equalsIgnoreCase("testdialoggui")));
		assertTrue(roots.stream().anyMatch(value -> value.equalsIgnoreCase("testgui")));
		assertTrue(roots.stream().anyMatch(value -> value.equalsIgnoreCase("testterminalgui")));
		assertTrue(roots.stream().anyMatch(value -> value.equalsIgnoreCase("vote")),
                    "the existing /av Vote command must remain available");
            assertNotNull(find(handlers, "GUI"), "existing admin GUI command");
            assertNotNull(find(handlers, "Vote"), "existing admin vote command");

		List<String> controls = AdvancedCoreTabCompleteHandler.getInstance()
				.getTabCompleteOptions(handlers, sender, new String[] { "testgui", "" }, 1);
		assertEquals(new HashSet<>(Arrays.asList("list", "close", "status", "cleanup")), new HashSet<>(controls));
	}

    @Test
    void existingAdminSiteAndEditorHandlersKeepTheirProductionTargetsAndPermissions() throws Exception {
        VotingPluginMain plugin = commandLoaderFixture();
        invokeAdminRegistration(plugin);
        Player player = mock(Player.class);
        var site = mock(com.bencodez.votingplugin.votesites.VoteSite.class);
        when(plugin.getVoteSiteManager().getVoteSite("Example", false)).thenReturn(site);
        var list = find(plugin.getAdminVoteCommand(), "Sites");
        var editor = find(plugin.getAdminVoteCommand(), "Sites", "(sitename)");
        assertEquals("VotingPlugin.Commands.AdminVote.Sites|" + ADMIN, list.getPerm());
        assertEquals("VotingPlugin.Commands.AdminVote.Sites.Site|" + ADMIN, editor.getPerm());
        try (var guis = org.mockito.Mockito.mockConstruction(com.bencodez.votingplugin.commands.gui.AdminGUI.class)) {
            list.execute(player, new String[]{"Sites"});
            editor.execute(player, new String[]{"Sites", "Example"});
            assertEquals(2, guis.constructed().size());
            verify(guis.constructed().get(0)).openAdminGUIVoteSites(player);
            verify(guis.constructed().get(1)).openAdminGUIVoteSiteSite(player, site);
            org.mockito.Mockito.verify(plugin, org.mockito.Mockito.never()).getExperimentalGUIManager();
        }
    }

    @Test
    void existingAdminGuiStillUsesAdvancedCoreMenuAndItsPermission() throws Exception {
        VotingPluginMain plugin = commandLoaderFixture();
        invokeAdminRegistration(plugin);
        var handler = find(plugin.getAdminVoteCommand(), "GUI");
        assertEquals("VotingPlugin.Commands.AdminVote.GUI|" + ADMIN, handler.getPerm());
        Player player = mock(Player.class);
        try (var statics = org.mockito.Mockito.mockStatic(com.bencodez.advancedcore.command.gui.AdminGUI.class)) {
            var gui = mock(com.bencodez.advancedcore.command.gui.AdminGUI.class);
            statics.when(com.bencodez.advancedcore.command.gui.AdminGUI::getInstance).thenReturn(gui);
            handler.execute(player, new String[]{"GUI"});
            verify(gui).openGUI(player);
            org.mockito.Mockito.verify(plugin, org.mockito.Mockito.never()).getExperimentalGUIManager();
        }
    }

	private static VotingPluginMain commandLoaderFixture() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		when(plugin.getOptions().isMultiplePermissionChecks()).thenReturn(true);
		AtomicReference<ArrayList<CommandHandler>> handlers = new AtomicReference<>();
		doAnswer(invocation -> {
			handlers.set(invocation.getArgument(0));
			return null;
		}).when(plugin).setAdminVoteCommand(org.mockito.ArgumentMatchers.any(ArrayList.class));
		when(plugin.getAdminVoteCommand()).thenAnswer(invocation -> handlers.get());
		return plugin;
	}

	private static void invokeAdminRegistration(VotingPluginMain plugin) throws Exception {
		Method method = CommandLoader.class.getDeclaredMethod("loadAdminVoteCommand");
		method.setAccessible(true);
        // AdvancedCore's shared singleton is initialized by plugin startup. This
        // fixture isolates generic AC registrations while exercising real VP handlers.
        try (var coreStatic = org.mockito.Mockito.mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
            var core = mock(com.bencodez.advancedcore.command.CommandLoader.class);
            coreStatic.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(core);
            when(core.getBasicAdminCommands("VotingPlugin")).thenReturn(new ArrayList<>());
            method.invoke(new CommandLoader(plugin));
        }
	}

	private static CommandHandler find(List<CommandHandler> handlers, String... expected) {
		return handlers.stream().filter(handler -> sameArgs(handler.getArgs(), expected)
				|| (expected.length == 1 && handler.isCommand(expected[0]))).findFirst().orElse(null);
	}

	private static boolean sameArgs(String[] actual, String[] expected) {
		if (actual.length != expected.length) return false;
		for (int index = 0; index < actual.length; index++) {
			if (!actual[index].equalsIgnoreCase(expected[index])) return false;
		}
		return true;
	}
}
