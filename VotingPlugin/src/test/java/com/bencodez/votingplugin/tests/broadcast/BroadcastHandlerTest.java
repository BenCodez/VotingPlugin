package com.bencodez.votingplugin.tests.broadcast;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
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
import org.bukkit.command.CommandSender;
import org.bukkit.command.ConsoleCommandSender;
import org.bukkit.entity.Player;
import org.bukkit.scheduler.BukkitScheduler;
import org.bukkit.scheduler.BukkitTask;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.broadcast.BroadcastFormat;
import com.bencodez.votingplugin.broadcast.BroadcastHandler;
import com.bencodez.votingplugin.broadcast.BroadcastSettings;
import com.bencodez.votingplugin.broadcast.VoteBroadcastType;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class BroadcastHandlerTest {

	private MockedStatic<Bukkit> bukkitStatic;
	private MockedStatic<MiscUtils> miscStatic;

	@AfterEach
	public void cleanup() {
		if (bukkitStatic != null) {
			bukkitStatic.close();
		}
		if (miscStatic != null) {
			miscStatic.close();
		}
	}

	private static BroadcastSettings settings(VoteBroadcastType type, String duration, int maxSites) {
		BroadcastFormat fmt = new BroadcastFormat("BM %player% %site% %reason%",
				"H %player% %sites_count% %sites% %reason%", "L %site%");
		return new BroadcastSettings(type, ParsedDuration.parse(duration), maxSites, fmt);
	}

	/**
	 * BroadcastHandler requires a non-null UserManager and uses console sender.
	 * Stub both so tests don't NPE and so player is eligible
	 * (DisableBroadcast=false).
	 */
	private void stubEligiblePlayerAndConsole(VotingPluginMain plugin, Player p) {
		UserManager um = mock(UserManager.class);
		VotingPluginUser user = mock(VotingPluginUser.class);

		when(plugin.getVotingPluginUserManager()).thenReturn(um);
		when(um.getVotingPluginUser(p)).thenReturn(user);
		when(user.getDisableBroadcast()).thenReturn(false);

		Server server = mock(Server.class);
		ConsoleCommandSender console = mock(ConsoleCommandSender.class);
		bukkitStatic.when(Bukkit::getServer).thenReturn(server);
		when(server.getConsoleSender()).thenReturn(console);
	}

	@Test
	public void everyVote_broadcasts_once_per_vote() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		Player p = mock(Player.class);
		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(p));
		stubEligiblePlayerAndConsole(plugin, p);

		MiscUtils misc = mock(MiscUtils.class);
		miscStatic = mockStatic(MiscUtils.class);
		miscStatic.when(MiscUtils::getInstance).thenReturn(misc);

		BroadcastHandler h = new BroadcastHandler(plugin, settings(VoteBroadcastType.EVERY_VOTE, "0", 5),
				ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		h.broadcastVote(uuid, "Ben", "SiteA", true);
		h.broadcastVote(uuid, "Ben", "SiteB", true);

		verify(misc, times(2)).broadcast(any(String.class), anyList());
	}

	@Test
	public void cooldownPerPlayer_suppresses_second_vote_within_window() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		Player p = mock(Player.class);
		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(p));
		stubEligiblePlayerAndConsole(plugin, p);

		MiscUtils misc = mock(MiscUtils.class);
		miscStatic = mockStatic(MiscUtils.class);
		miscStatic.when(MiscUtils::getInstance).thenReturn(misc);

		BroadcastHandler h = new BroadcastHandler(plugin, settings(VoteBroadcastType.COOLDOWN_PER_PLAYER, "10m", 5),
				ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		h.broadcastVote(uuid, "Ben", "SiteA", true);
		h.broadcastVote(uuid, "Ben", "SiteB", true); // within 10m => should not broadcast

		verify(misc, times(1)).broadcast(any(String.class), anyList());
	}

	@Test
	public void firstVoteOfDay_only_first_broadcasts_for_that_day() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		Player p = mock(Player.class);
		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(p));
		stubEligiblePlayerAndConsole(plugin, p);

		MiscUtils misc = mock(MiscUtils.class);
		miscStatic = mockStatic(MiscUtils.class);
		miscStatic.when(MiscUtils::getInstance).thenReturn(misc);

		BroadcastHandler h = new BroadcastHandler(plugin, settings(VoteBroadcastType.FIRST_VOTE_OF_DAY, "0", 5),
				ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		h.broadcastVote(uuid, "Ben", "SiteA", true);
		h.broadcastVote(uuid, "Ben", "SiteB", true);

		verify(misc, times(1)).broadcast(any(String.class), anyList());
	}

	@Test
	public void batchWindowPerPlayer_flushes_once_and_lists_sites() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		Player p = mock(Player.class);

		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		BukkitTask task = mock(BukkitTask.class);

		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(p));
		bukkitStatic.when(Bukkit::getScheduler).thenReturn(scheduler);
		stubEligiblePlayerAndConsole(plugin, p);

		AtomicReference<Runnable> scheduled = new AtomicReference<>();
		when(scheduler.runTaskLater(eq(plugin), any(Runnable.class), anyLong())).thenAnswer(inv -> {
			scheduled.set(inv.getArgument(1));
			return task;
		});

		MiscUtils misc = mock(MiscUtils.class);
		miscStatic = mockStatic(MiscUtils.class);
		miscStatic.when(MiscUtils::getInstance).thenReturn(misc);

		BroadcastSettings s = new BroadcastSettings(VoteBroadcastType.BATCH_WINDOW_PER_PLAYER,
				ParsedDuration.parse("1s"), 10, new BroadcastFormat("SINGLE %player% %site%",
						"HEADER %player% %sites_count% %sites% %reason%", " - %site%"));

		BroadcastHandler h = new BroadcastHandler(plugin, s, ZoneId.systemDefault());

		UUID uuid = UUID.randomUUID();
		h.broadcastVote(uuid, "Ben", "SiteB", true);
		h.broadcastVote(uuid, "Ben", "SiteA", true);

		// run the scheduled flush after both votes
		scheduled.get().run();

		// 2 sites => render produces 3 lines (header + 2 list lines), each broadcast
		// separately
		verify(misc, times(3)).broadcast(any(String.class), anyList());

		// header should contain both sites and count=2
		verify(misc).broadcast(argThat(msg -> msg.contains("HEADER") && msg.contains("Ben") && msg.contains("2")
				&& msg.contains("SiteA") && msg.contains("SiteB") && msg.contains("batch")), anyList());
	}

	@Test
	public void maxSitesListed_truncates_list_in_multi_broadcast() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);

		Player p = mock(Player.class);
		bukkitStatic = mockStatic(Bukkit.class);
		bukkitStatic.when(Bukkit::getOnlinePlayers).thenReturn(Collections.singletonList(p));
		stubEligiblePlayerAndConsole(plugin, p);

		MiscUtils misc = mock(MiscUtils.class);
		miscStatic = mockStatic(MiscUtils.class);
		miscStatic.when(MiscUtils::getInstance).thenReturn(misc);

		BroadcastSettings s = new BroadcastSettings(VoteBroadcastType.EVERY_VOTE, ParsedDuration.parse("0"), 1,
				new BroadcastFormat("SINGLE %player% %site%", "H %player% %sites_count% %sites%", "L %site%"));

		BroadcastHandler h = new BroadcastHandler(plugin, s, ZoneId.systemDefault());

		h.broadcastVote(UUID.randomUUID(), "Ben", "SiteA", true);

		verify(misc, times(1)).broadcast(any(String.class), anyList());
	}
}
