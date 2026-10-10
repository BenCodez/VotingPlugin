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
	void registersOnlyTheExistingCommandNames() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		assertEquals(List.of(List.of("ClearOfflineVoteRewards"), List.of("ClearOfflineVotes")), fixture.patterns());
	}

	@Test
	void nonMysqlCommandsKeepTheirSingleInvocationBehavior() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		CommandSender console = mock(CommandSender.class);
		for (CommandHandler handler : fixture.handlers()) {
			handler.execute(console, handler.getArgs());
			assertTrue(fixture.lastMutation.get().getAsBoolean());
		}
		verify(fixture.votingUsers, times(2)).clearAllOfflineVotes(false);
		verify(fixture.users).removeAllKeyValues("OfflineRewards", DataType.STRING);
	}

	@Test
	void mysqlRequiresAnIdenticalSecondInvocationAndConsumesConfirmation() {
		Fixture fixture = new Fixture();
		when(fixture.users.getDataManager().effectiveStorageType(fixture.plugin.getStorageType()))
				.thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		CommandSender console = mock(CommandSender.class);
		when(console.getName()).thenReturn("Console");
		CommandHandler handler = fixture.handlers().get(0);
		handler.execute(console, handler.getArgs());
		assertTrue(!fixture.lastMutation.get().getAsBoolean());
		verify(fixture.votingUsers, never()).clearAllOfflineVotes(true);
		verify(fixture.users, never()).removeAllKeyValues("OfflineRewards", DataType.STRING);
		handler.execute(console, handler.getArgs());
		assertTrue(fixture.lastMutation.get().getAsBoolean());
		verify(fixture.votingUsers).clearAllOfflineVotes(true);
		verify(fixture.users).removeAllKeyValues("OfflineRewards", DataType.STRING);
		handler.execute(console, handler.getArgs());
		assertTrue(!fixture.lastMutation.get().getAsBoolean());
		verify(fixture.votingUsers, times(1)).clearAllOfflineVotes(true);
	}

	@Test
	void differentBulkCommandCannotConfirmThePreviousCommand() {
		Fixture fixture = new Fixture();
		when(fixture.users.getDataManager().effectiveStorageType(fixture.plugin.getStorageType()))
				.thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		CommandSender console = mock(CommandSender.class);
		for (CommandHandler handler : fixture.handlers()) {
			handler.execute(console, handler.getArgs());
			assertTrue(!fixture.lastMutation.get().getAsBoolean());
		}
		verify(fixture.votingUsers, never()).clearAllOfflineVotes(true);
	}

	@Test
	void mysqlConfirmationCannotBeUsedByAPlayerSender() {
		Fixture fixture = new Fixture();
		when(fixture.users.getDataManager().effectiveStorageType(fixture.plugin.getStorageType()))
				.thenReturn(com.bencodez.advancedcore.api.user.UserStorage.MYSQL);
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		Player player = mock(Player.class);
		fixture.handlers().get(0).execute(player, new String[] {"ClearOfflineVoteRewards"});
		assertEquals(null, fixture.lastMutation.get());
		fixture.handlers().get(1).execute(player, new String[] {"ClearOfflineVotes"});
		assertTrue(!fixture.lastMutation.get().getAsBoolean());
		verify(fixture.votingUsers, never()).clearAllOfflineVotes(true);
	}

	@Test
	void failedVoteClearNeverClearsTheSeparateGenericRewardQueue() {
		Fixture fixture = new Fixture();
		fixture.loader.registerOfflineVoteClearCommands("VotingPlugin.Admin");
		doThrow(new IllegalStateException("pending batch")).when(fixture.votingUsers).clearAllOfflineVotes(false);
		CommandHandler handler = fixture.handlers().get(0);
		handler.execute(mock(CommandSender.class), handler.getArgs());
		assertThrows(IllegalStateException.class, () -> fixture.lastMutation.get().getAsBoolean());
		verify(fixture.users, never()).removeAllKeyValues("OfflineRewards", DataType.STRING);
	}

	private static final class Fixture {
		final VotingPluginMain plugin = mock(VotingPluginMain.class);
		final UserManager votingUsers = mock(UserManager.class);
		final com.bencodez.advancedcore.api.user.UserManager users =
				mock(com.bencodez.advancedcore.api.user.UserManager.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		final AtomicReference<java.util.function.BooleanSupplier> lastMutation = new AtomicReference<>();
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
		private final AtomicReference<java.util.function.BooleanSupplier> mutation;

		TestLoader(VotingPluginMain plugin, AtomicReference<java.util.function.BooleanSupplier> mutation) {
			super(plugin);
			this.mutation = mutation;
		}

		@Override
		void runForCommandSender(CommandSender sender, Runnable task) { task.run(); }

		@Override
		void runConfirmedBulkStorageMutation(CommandSender sender, java.util.function.BooleanSupplier work, Runnable success) {
			mutation.set(work);
		}
	}
}
