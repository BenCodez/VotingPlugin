package com.bencodez.votingplugin.experimental;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.debug.DebugLevel;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.CommandLoader;
import com.bencodez.votingplugin.commands.gui.player.VoteGUI;
import com.bencodez.votingplugin.commands.gui.player.VoteURL;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.user.UserManager;

/** Regression coverage for the production /vote entry handlers. */
class ExperimentalProductionVoteCommandTest {

	@Test
	void emptyVoteEntryUsesTheConfiguredProductionGuiAndNeverExperimentalManager() throws Exception {
		VotingPluginMain plugin = voteLoaderFixture(true);
		invokeVoteRegistration(plugin);
		Player player = mock(Player.class);
		CommandHandler entry = find(plugin.getVoteCommand());

		try (MockedConstruction<VoteGUI> constructed = mockConstruction(VoteGUI.class);
				MockedConstruction<VoteURL> urls = mockConstruction(VoteURL.class)) {
			entry.execute(player, new String[0]);
			assertNotNull(constructed.constructed());
			assertEquals(1, constructed.constructed().size());
			verify(constructed.constructed().get(0)).open();
			assertEquals(0, urls.constructed().size());
			verify(plugin, never()).getExperimentalGUIManager();
		}
	}

	@Test
	void emptyVoteEntryUsesProductionUrlWhenMainGuiIsDisabled() throws Exception {
		VotingPluginMain plugin = voteLoaderFixture(false);
		invokeVoteRegistration(plugin);
		CommandSender console = mock(CommandSender.class);
		CommandHandler entry = find(plugin.getVoteCommand());

		try (MockedConstruction<VoteGUI> constructed = mockConstruction(VoteGUI.class);
				MockedConstruction<VoteURL> urls = mockConstruction(VoteURL.class)) {
			entry.execute(console, new String[0]);
			assertEquals(0, constructed.constructed().size());
			assertEquals(1, urls.constructed().size());
			verify(urls.constructed().get(0)).open();
			verify(plugin, never()).getExperimentalGUIManager();
		}
	}

	@Test
	void explicitVoteGuiHandlerStillOpensProductionGui() throws Exception {
		VotingPluginMain plugin = voteLoaderFixture(true);
		invokeVoteRegistration(plugin);
		Player player = mock(Player.class);
		CommandHandler gui = find(plugin.getVoteCommand(), "GUI");

		try (MockedConstruction<VoteGUI> constructed = mockConstruction(VoteGUI.class)) {
			gui.execute(player, new String[] { "GUI" });
			verify(plugin, never()).getExperimentalGUIManager();
			assertEquals(1, constructed.constructed().size());
			verify(constructed.constructed().get(0)).open();
		}
	}

	private static VotingPluginMain voteLoaderFixture(boolean mainGui) {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		when(config.isUseVoteGUIMainCommand()).thenReturn(mainGui);
		when(config.getDisabledCommands()).thenReturn(new ArrayList<>());
		when(plugin.getConfigFile()).thenReturn(config);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		when(options.getDebug()).thenReturn(DebugLevel.NONE);
		when(plugin.getOptions()).thenReturn(options);
		when(plugin.getGui()).thenReturn(mock(com.bencodez.votingplugin.config.GUI.class));
		when(plugin.getVotingPluginUserManager()).thenReturn(mock(UserManager.class));
		AtomicReference<ArrayList<CommandHandler>> handlers = new AtomicReference<>();
		org.mockito.Mockito.doAnswer(invocation -> {
			handlers.set(invocation.getArgument(0));
			return null;
		}).when(plugin).setVoteCommand(org.mockito.ArgumentMatchers.any(ArrayList.class));
		when(plugin.getVoteCommand()).thenAnswer(invocation -> handlers.get());
		return plugin;
	}

	private static void invokeVoteRegistration(VotingPluginMain plugin) throws Exception {
		Method method = CommandLoader.class.getDeclaredMethod("loadVoteCommand");
		method.setAccessible(true);
		// Generic AdvancedCore registrations require its startup singleton. Keep
		// that boundary isolated while executing VotingPlugin's real handlers.
		try (var coreStatic = org.mockito.Mockito.mockStatic(com.bencodez.advancedcore.command.CommandLoader.class)) {
			var core = mock(com.bencodez.advancedcore.command.CommandLoader.class);
			coreStatic.when(com.bencodez.advancedcore.command.CommandLoader::getInstance).thenReturn(core);
			when(core.getBasicCommands("VotingPlugin")).thenReturn(new ArrayList<>());
			method.invoke(new CommandLoader(plugin));
		}
	}

	private static CommandHandler find(List<CommandHandler> handlers, String... args) {
		return handlers.stream().filter(handler -> sameArgs(handler.getArgs(), args)).findFirst().orElse(null);
	}

	private static boolean sameArgs(String[] actual, String[] expected) {
		if (actual.length != expected.length) return false;
		for (int index = 0; index < actual.length; index++) {
			if (!actual[index].equalsIgnoreCase(expected[index])) return false;
		}
		return true;
	}
}
