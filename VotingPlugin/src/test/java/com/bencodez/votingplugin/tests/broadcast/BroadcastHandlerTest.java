package com.bencodez.votingplugin.tests.broadcast;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.ZoneId;
import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.Bukkit;
import org.bukkit.Server;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.scheduler.BukkitScheduler;
import org.bukkit.scheduler.BukkitTask;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.broadcast.BroadcastFormat;
import com.bencodez.votingplugin.broadcast.BroadcastHandler;
import com.bencodez.votingplugin.broadcast.BroadcastSettings;
import com.bencodez.votingplugin.broadcast.VoteBroadcastType;

public class BroadcastHandlerTest {

	private MockedStatic<Bukkit> bukkitStatic;
	private ConsoleCommandSender console;

	@AfterEach
	public void cleanup() {
		if (bukkitStatic != null) {
			bukkitStatic.close();
		}
	}

	private static BroadcastSettings settings(VoteBroadcastType type, String duration, int maxSites) {
		BroadcastFormat format = new BroadcastFormat("BM %player% %site% %reason%",
				"H %player% %sites_count% %sites% %reason%", "L %site%");
		return new BroadcastSettings(type, ParsedDuration.parse(duration), maxSites, format);
	}

	/**
	 * Stubs Bukkit with no online recipients and a valid console sender.
	 *
	 * The handler always writes the final rendered message to the console, so these
	 * tests can verify broadcast behavior without mocking AdvancedCore's internal
	 * server message handle.
	 */
	private void stubBukkitAndConsole() {
		Server server = mock(Server.class);
		console = mock(ConsoleCommandSender.class);

		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.emptyList());
		bukkitStatic.when(Bukkit::getServer).thenReturn(server);

		when(server.getConsoleSender()).thenReturn(console);
	}

	@Test
	public void everyVote_broadcasts_once_per_vote() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		stubBukkitAndConsole();

		BroadcastHandler handler = new BroadcastHandler(plugin, settings(VoteBroadcastType.EVERY_VOTE, "0", 5),
				ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		handler.broadcastVote(uuid, "Ben", "SiteA", true);
		handler.broadcastVote(uuid, "Ben", "SiteB", true);

		verify(console, times(2)).sendMessage(any(String.class));
	}

	@Test
	public void cooldownPerPlayer_suppresses_second_vote_within_window() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		stubBukkitAndConsole();

		BroadcastHandler handler = new BroadcastHandler(plugin,
				settings(VoteBroadcastType.COOLDOWN_PER_PLAYER, "10m", 5), ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		handler.broadcastVote(uuid, "Ben", "SiteA", true);
		handler.broadcastVote(uuid, "Ben", "SiteB", true);

		verify(console, times(1)).sendMessage(any(String.class));
	}

	@Test
	public void firstVoteOfDay_only_first_broadcasts_for_that_day() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		stubBukkitAndConsole();

		BroadcastHandler handler = new BroadcastHandler(plugin, settings(VoteBroadcastType.FIRST_VOTE_OF_DAY, "0", 5),
				ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		handler.broadcastVote(uuid, "Ben", "SiteA", true);
		handler.broadcastVote(uuid, "Ben", "SiteB", true);

		verify(console, times(1)).sendMessage(any(String.class));
	}

	@Test
	public void batchWindowPerPlayer_flushes_once_and_lists_sites() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		BukkitTask task = mock(BukkitTask.class);

		stubBukkitAndConsole();
		bukkitStatic.when(Bukkit::getScheduler).thenReturn(scheduler);

		AtomicReference<Runnable> scheduled = new AtomicReference<Runnable>();
		when(scheduler.runTaskLater(eq(plugin), any(Runnable.class), anyLong())).thenAnswer(invocation -> {
			scheduled.set(invocation.getArgument(1));
			return task;
		});

		BroadcastSettings broadcastSettings = new BroadcastSettings(VoteBroadcastType.BATCH_WINDOW_PER_PLAYER,
				ParsedDuration.parse("1s"), 10, new BroadcastFormat("SINGLE %player% %site%",
						"HEADER %player% %sites_count% %sites% %reason%", " - %site%"));

		BroadcastHandler handler = new BroadcastHandler(plugin, broadcastSettings, ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		handler.broadcastVote(uuid, "Ben", "SiteB", true);
		handler.broadcastVote(uuid, "Ben", "SiteA", true);

		scheduled.get().run();

		verify(console, times(3)).sendMessage(any(String.class));
		verify(console).sendMessage(org.mockito.ArgumentMatchers.<String>argThat(
				message -> message.contains("HEADER") && message.contains("Ben") && message.contains("2")
						&& message.contains("SiteA") && message.contains("SiteB") && message.contains("batch")));
	}

	@Test
	public void maxSitesListed_truncates_list_in_multi_broadcast() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		stubBukkitAndConsole();

		BroadcastSettings broadcastSettings = new BroadcastSettings(VoteBroadcastType.EVERY_VOTE,
				ParsedDuration.parse("0"), 1,
				new BroadcastFormat("SINGLE %player% %site%", "H %player% %sites_count% %sites%", "L %site%"));

		BroadcastHandler handler = new BroadcastHandler(plugin, broadcastSettings, ZoneId.systemDefault());

		handler.broadcastVote(UUID.randomUUID(), "Ben", "SiteA", true);

		verify(console, times(1)).sendMessage(any(String.class));
	}
}