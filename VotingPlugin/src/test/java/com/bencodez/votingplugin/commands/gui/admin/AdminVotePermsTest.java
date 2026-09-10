package com.bencodez.votingplugin.commands.gui.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.PluginDescriptionFile;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.AdvancedCoreConfigOptions;
import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.commands.AdminAuthorization;
import com.bencodez.votingplugin.config.Config;

class AdminVotePermsTest {
	@Test
	void listsSharedBulkMetadataForEveryPlayerCommand() {
		PlayerCommandHandler setPoints = playerHandler("/av User (player) SetPoints (number)",
				"VotingPlugin.Commands.AdminVote.SetPoints.All");
		PlayerCommandHandler addPoints = playerHandler("/av User (player) AddPoints (number)",
				"VotingPlugin.Commands.AdminVote.AddPoints.All");

		assertEquals(Collections.singletonList("VotingPlugin.Commands.AdminVote.SetPoints.All"),
				AdminVotePerms.additionalPermissions(setPoints));
		assertEquals(Collections.singletonList("VotingPlugin.Commands.AdminVote.AddPoints.All"),
				AdminVotePerms.additionalPermissions(addPoints));
	}

	@Test
	void ordinaryCommandsDoNotInventAdditionalPermissions() {
		assertEquals(Collections.emptyList(), AdminVotePerms.additionalPermissions(mock(CommandHandler.class)));
	}

	@Test
	void senderListingAssociatesBulkPermissionAndUsesEffectiveAuthorization() {
		Player sender = mock(Player.class);
		PlayerCommandHandler handler = playerHandler("/av User (player) SetPoints (number)",
				"VotingPlugin.Commands.AdminVote.SetPoints.All");
		when(handler.hasPerm(sender)).thenReturn(true);
		when(handler.hasAllPermission(sender)).thenReturn(true);
		AdminVotePerms perms = fixture(sender, List.of(), List.of(handler), 20);

		String output = String.join("\n", perms.listPerms(sender));

		assertTrue(output.contains("/av User (player) SetPoints (number) : Additional permission for all target: "
				+ "VotingPlugin.Commands.AdminVote.SetPoints.All : §atrue"));
	}

	@Test
	void namedPlayerListingChecksInspectedPlayerNotViewer() {
		Player viewer = mock(Player.class);
		Player inspected = mock(Player.class);
		PlayerCommandHandler handler = playerHandler("/vote Example (player)", "VotingPlugin.Commands.Example.All");
		when(handler.hasAllPermission(inspected)).thenReturn(true);
		AdminVotePerms perms = fixture(viewer, List.of(handler), List.of(), 20);

		ArrayList<String> output;
		try (MockedStatic<Bukkit> bukkit = mockStatic(Bukkit.class)) {
			bukkit.when(() -> Bukkit.getPlayer("Target")).thenReturn(inspected);
			output = perms.listPerms(viewer, "Target", 1);
		}

		assertTrue(String.join("\n", output).contains("VotingPlugin.Commands.Example.All : §atrue"));
		verify(handler).hasAllPermission(inspected);
		verify(handler, never()).hasAllPermission(viewer);
	}

	@Test
	void consoleListingShowsBulkMetadataWithoutInventingAccessStatus() {
		CommandSender console = mock(CommandSender.class);
		PlayerCommandHandler handler = playerHandler("/av User (player) AddPoints (number)",
				"VotingPlugin.Commands.AdminVote.AddPoints.All");
		AdminVotePerms perms = fixture(console, List.of(), List.of(handler), 20);

		String output = String.join("\n", perms.listPerms(console));

		assertTrue(output.contains("Additional permission for all target: VotingPlugin.Commands.AdminVote.AddPoints.All"));
		assertFalse(output.contains("VotingPlugin.Commands.AdminVote.AddPoints.All : true"));
		assertFalse(output.contains("VotingPlugin.Commands.AdminVote.AddPoints.All : false"));
	}

	@Test
	void paginationRetainsBulkEntryOnItsExpectedPage() {
		CommandSender console = mock(CommandSender.class);
		PlayerCommandHandler handler = playerHandler("/av User (player) SetPoints (number)",
				"VotingPlugin.Commands.AdminVote.SetPoints.All");
		String[] secondPage = new AdminVotePerms(plugin(List.of(), List.of(handler), 1), console, 2).listPerms(console);

		assertTrue(Arrays.stream(secondPage)
				.anyMatch(line -> line.contains("Additional permission for all target: ")));
	}

	@Test
	void developerOutputLabelsBulkMetadataForVoteAndAdminHandlers() {
		CommandSender consoleSub = mock(CommandSender.class);
		PlayerCommandHandler vote = playerHandler("/vote Example (player)", "VotingPlugin.Commands.Example.All");
		PlayerCommandHandler admin = playerHandler("/av User (player) RemovePoints (number)",
				"VotingPlugin.Commands.AdminVote.RemovePoints.All");
		AdminVotePerms perms = fixture(consoleSub, List.of(vote), List.of(admin), 20);

		String output = String.join("\n", perms.listPermsDev(consoleSub));

		assertTrue(output.contains("Additional permission for all target: VotingPlugin.Commands.Example.All"));
		assertTrue(output.contains(
				"Additional permission for all target: VotingPlugin.Commands.AdminVote.RemovePoints.All"));
	}

	@Test
	void editorPermissionStatusIncludesAdminOverride() {
		CommandSender sender = mock(CommandSender.class);
		when(sender.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);
		assertTrue(AdminVotePerms.hasEffectivePermission(sender, "VotingPlugin.Commands.AdminVote.Edit.Config"));
	}

	@Test
	void administratorOverrideListingMatchesSharedAuthorization() {
		Player admin = mock(Player.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);
		VotingPluginMain plugin = plugin(List.of(), List.of(), 20);
		PlayerCommandHandler handler = new PlayerCommandHandler(plugin,
				new String[] { "User", "(player)", "SetPoints", "(number)" },
				"VotingPlugin.Commands.AdminVote.SetPoints|" + AdminAuthorization.ADMIN_PERMISSION, "Set points") {
			@Override
			public void executeAll(CommandSender sender, String[] args) {
			}

			@Override
			public void executeSinglePlayer(CommandSender sender, String[] args) {
			}
		};
		configureAllPermissionOverride(handler);
		when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>(List.of(handler)));

		assertTrue(handler.hasAllPermission(admin));
		String output = String.join("\n", new AdminVotePerms(plugin, admin, 1).listPerms(admin));
		assertTrue(output.contains("VotingPlugin.Commands.AdminVote.SetPoints.All : §atrue"));
	}

	@Test
	void disabledMultiplePermissionChecksDisableAdminAlternativeInListing() {
		Player admin = mock(Player.class);
		when(admin.hasPermission(AdminAuthorization.ADMIN_PERMISSION)).thenReturn(true);
		VotingPluginMain plugin = plugin(List.of(), List.of(), 20, false);
		PlayerCommandHandler handler = new PlayerCommandHandler(plugin, new String[] { "User", "(player)" },
				"VotingPlugin.Commands.AdminVote.SetPoints|" + AdminAuthorization.ADMIN_PERMISSION, "Set points") {
			@Override
			public void executeAll(CommandSender sender, String[] args) {
			}

			@Override
			public void executeSinglePlayer(CommandSender sender, String[] args) {
			}
		};
		configureAllPermissionOverride(handler);
		when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>(List.of(handler)));

		assertFalse(handler.hasAllPermission(admin));
		String output = String.join("\n", new AdminVotePerms(plugin, admin, 1).listPerms(admin));
		assertTrue(output.contains("VotingPlugin.Commands.AdminVote.SetPoints.All : §cfalse"));
	}

	private static PlayerCommandHandler playerHandler(String helpLine, String additionalPermission) {
		PlayerCommandHandler handler = mock(PlayerCommandHandler.class);
		String basePermission = additionalPermission.substring(0, additionalPermission.length() - ".All".length());
		when(handler.getPerm()).thenReturn(basePermission + "|VotingPlugin.Admin");
		when(handler.getHelpLineCommand("/vote")).thenReturn(helpLine);
		when(handler.getHelpLineCommand("/av")).thenReturn(helpLine);
		when(handler.getHelpMessage()).thenReturn("Help");
		when(handler.getAdditionalPermissions()).thenReturn(List.of(additionalPermission));
		return handler;
	}

	private static void configureAllPermissionOverride(PlayerCommandHandler handler) {
		try {
			PlayerCommandHandler.class.getMethod("withAllPermissionOverrides", String[].class)
					.invoke(handler, (Object) new String[] { AdminAuthorization.ADMIN_PERMISSION });
		} catch (NoSuchMethodException ignored) {
			// The published pre-#316 API already treats later alternatives as overrides.
		} catch (ReflectiveOperationException exception) {
			throw new AssertionError(exception);
		}
	}

	private static AdminVotePerms fixture(CommandSender viewer, List<CommandHandler> vote,
			List<CommandHandler> admin, int pageSize) {
		return new AdminVotePerms(plugin(vote, admin, pageSize), viewer, 1);
	}

	private static VotingPluginMain plugin(List<CommandHandler> vote, List<CommandHandler> admin, int pageSize) {
		return plugin(vote, admin, pageSize, true);
	}

	private static VotingPluginMain plugin(List<CommandHandler> vote, List<CommandHandler> admin, int pageSize,
			boolean multiplePermissions) {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		Config config = mock(Config.class);
		AdvancedCoreConfigOptions options = mock(AdvancedCoreConfigOptions.class);
		PluginDescriptionFile description = mock(PluginDescriptionFile.class);
		when(plugin.getVoteCommand()).thenReturn(new ArrayList<>(vote));
		when(plugin.getAdminVoteCommand()).thenReturn(new ArrayList<>(admin));
		when(plugin.getConfigFile()).thenReturn(config);
		when(plugin.getOptions()).thenReturn(options);
		when(options.isMultiplePermissionChecks()).thenReturn(multiplePermissions);
		when(config.getFormatPageSize()).thenReturn(pageSize);
		when(plugin.getDescription()).thenReturn(description);
		when(description.getPermissions()).thenReturn(Collections.emptyList());
		return plugin;
	}
}
