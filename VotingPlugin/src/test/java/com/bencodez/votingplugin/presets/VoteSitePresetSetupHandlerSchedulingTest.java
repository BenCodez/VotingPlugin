package com.bencodez.votingplugin.presets;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.command.CommandHandler;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class VoteSitePresetSetupHandlerSchedulingTest {
	@Test
	void votePresetCommandHandlerUsesAsyncExecutionLane() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player player = mock(Player.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicBoolean executed = new AtomicBoolean();
		CommandHandler command = new CommandHandler(plugin, new String[] { "VotePresets" }, "", "", false) {
			@Override public void execute(CommandSender sender, String[] args) { executed.set(true); }
			@Override public void debug(String debug) { }
			@Override public String formatNoPerms() { return ""; }
			@Override public String formatNotNumber() { return ""; }
			@Override public BukkitScheduler getBukkitScheduler() { return scheduler; }
			@Override public String getHelpLine() { return ""; }
		};

		assertTrue(command.runCommand(player, new String[] { "VotePresets" }));
		assertFalse(executed.get(), "execute must not run inline on the command caller");
		verify(scheduler).runTaskAsynchronously(eq(plugin), any(Runnable.class));
	}

	@Test
	void presetLoadStaysOnCommandWorkerAndUiReturnsToPlayerLane() {
		Fixture fixture = fixture();

		new VoteSitePresetSetupHandler(fixture.plugin, fixture.loader).startSetup(fixture.player);

		assertTrue(fixture.loaded.get(), "preset load should run directly on the already-async command worker");
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));
		verify(fixture.scheduler, never()).runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
	}

	@Test
	void urlLookupStaysOnCommandWorkerAndUiReturnsToPlayerLane() {
		Fixture fixture = fixture();

		new VoteSitePresetSetupHandler(fixture.plugin, fixture.loader)
				.findPresetForURL(fixture.player, "https://example.com/vote");

		assertTrue(fixture.loaded.get(), "preset lookup should run directly on the already-async command worker");
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class), eq(fixture.player));
		verify(fixture.scheduler, never()).runTaskAsynchronously(eq(fixture.plugin), any(Runnable.class));
	}

	private Fixture fixture() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		Player player = mock(Player.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		AtomicBoolean loaded = new AtomicBoolean();

		GitHubVoteSitePresetLoader loader = new GitHubVoteSitePresetLoader("BenCodez", "VotingPlugin-Presets", "main") {
			@Override
			public synchronized List<VoteSitePreset> listAllVoteSitePresets() throws IOException {
				loaded.set(true);
				return Collections.emptyList();
			}
		};
		return new Fixture(plugin, scheduler, player, loader, loaded);
	}

	private record Fixture(VotingPluginMain plugin, BukkitScheduler scheduler, Player player,
			GitHubVoteSitePresetLoader loader, AtomicBoolean loaded) { }
}
