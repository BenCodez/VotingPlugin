package com.bencodez.votingplugin.commands;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.doThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;

class OfflineVoteClearCommandRegistrationTest {
	@Test
	void registersBothCommandsWithAndWithoutExplicitQuiescenceAssertion() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");

		assertEquals(Arrays.asList(
				Arrays.asList("ClearOfflineVoteRewards"),
				Arrays.asList("ClearOfflineVoteRewards", "all-backends-quiesced"),
				Arrays.asList("ClearOfflineVotes"),
				Arrays.asList("ClearOfflineVotes", "all-backends-quiesced")), fixture.patterns());
	}

	@Test
	void registeredHandlersDispatchTheConfirmationAndDoNotAcceptAFalseLiteral() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		CommandSender console = mock(CommandSender.class);

		for (CommandHandler handler : fixture.handlers()) {
			String[] args = handler.getArgs();
			assertTrue(handler.argsMatch(args[0], 0));
			if (args.length == 2) {
				assertTrue(handler.argsMatch(args[1], 1));
				assertTrue(!handler.argsMatch("not-confirmed", 1));
			}
			handler.execute(console, args);
			fixture.lastMutation.get().run();
		}

		verify(fixture.votingUsers, times(2)).clearAllOfflineVotes(false);
		verify(fixture.votingUsers, times(2)).clearAllOfflineVotes(true);
		verify(fixture.users, times(2)).removeAllKeyValues("OfflineRewards", DataType.STRING);
	}

	@Test
	void confirmationCannotBeUsedByAPlayerSender() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		Player player = mock(Player.class);

		for (CommandHandler handler : fixture.handlers()) {
			if (handler.getArgs().length == 2) {
				handler.execute(player, handler.getArgs());
			}
		}

		assertEquals(null, fixture.lastMutation.get());
	}

	@Test
	void failedVoteClearNeverClearsTheSeparateGenericRewardQueue() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		doThrow(new IllegalStateException("pending batch")).when(fixture.votingUsers).clearAllOfflineVotes(true);
		CommandHandler confirmed = fixture.handlers().get(1);
		confirmed.execute(mock(CommandSender.class), confirmed.getArgs());
		assertThrows(IllegalStateException.class, () -> fixture.lastMutation.get().run());
		verify(fixture.users, never()).removeAllKeyValues("OfflineRewards", DataType.STRING);
	}

	private static final class Fixture {
		final VotingPluginMain plugin = mock(VotingPluginMain.class);
		final UserManager votingUsers = mock(UserManager.class);
		final com.bencodez.advancedcore.api.user.UserManager users =
				mock(com.bencodez.advancedcore.api.user.UserManager.class);
		final AtomicReference<Runnable> lastMutation = new AtomicReference<>();
		final TestLoader loader;

		Fixture() {
			when(plugin.getOptions()).thenReturn(mock(com.bencodez.advancedcore.AdvancedCoreConfigOptions.class));
			when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>());
			when(plugin.getVotingPluginUserManager()).thenReturn(votingUsers);
			when(plugin.getUserManager()).thenReturn(users);
			when(users.getOfflineRewardsPath()).thenReturn("OfflineRewards");
			loader = new TestLoader(plugin, lastMutation);
		}

		List<CommandHandler> handlers() {
			return plugin.getAdminVoteCommand();
		}

		List<List<String>> patterns() {
			return handlers().stream().map(handler -> Arrays.asList(handler.getArgs())).collect(Collectors.toList());
		}
	}

	private static final class TestLoader extends CommandLoader {
		private final AtomicReference<Runnable> mutation;

		TestLoader(VotingPluginMain plugin, AtomicReference<Runnable> mutation) {
			super(plugin);
			this.mutation = mutation;
		}

		@Override
		void runBulkStorageMutation(CommandSender sender, Runnable work, Runnable success) {
			mutation.set(work);
		}
	}
}
