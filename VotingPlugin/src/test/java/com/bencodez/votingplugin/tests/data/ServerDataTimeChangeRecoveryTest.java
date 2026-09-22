package com.bencodez.votingplugin.tests.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSection;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSnapshot;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardTarget;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;

class ServerDataTimeChangeRecoveryTest {
	@Test
	void checkpointRetainsPhaseCursorAndRewardReceiptsForTheTransition() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		ServerData data = new ServerData(plugin);

		data.beginTimeChangeRecovery(transition);
		assertFalse(data.hasTimeChangePhase(transition, "SNAPSHOT"));
		data.completeTimeChangePhase(transition, "SNAPSHOT");
		data.completeTimeChangePhase(transition, "USER_UPDATES");
		data.completeTimeChangeUser(transition, "00000000-0000-0000-0000-000000000002");
		data.completeTimeChangeReward(transition, "00000000-0000-0000-0000-000000000001");

		assertTrue(data.hasTimeChangePhase(transition, "SNAPSHOT"));
		assertTrue(data.hasTimeChangePhase(transition, "COPY_TOTALS"));
		assertTrue(data.hasTimeChangePhase(transition, "USER_UPDATES"));
		assertTrue(data.getTimeChangeCursor(transition).endsWith("0002"));
		assertTrue(data.hasTimeChangeRewardReceipt(transition, "00000000-0000-0000-0000-000000000001"));
		verify(coreData, atLeastOnce()).saveData();
	}

	@Test
	void newPeriodReplacesOnlyThatTimeTypesOldCheckpoint() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition oldTransition = transition("MONTH:2026-08", "2026-08", TimeType.MONTH);
		TimeChangeTransition newTransition = transition("MONTH:2026-09", "2026-09", TimeType.MONTH);

		data.beginTimeChangeRecovery(oldTransition);
		data.completeTimeChangeReward(oldTransition, "00000000-0000-0000-0000-000000000001");
		data.beginTimeChangeRecovery(newTransition);

		assertFalse(data.hasTimeChangeRewardReceipt(newTransition, "00000000-0000-0000-0000-000000000001"));
		assertFalse(data.hasTimeChangePhase(oldTransition, "START"));
	}

	@Test
	void interruptedUserKeepsItsAbsoluteStreakTargetUntilCursorAdvances() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		String uuid = "00000000-0000-0000-0000-000000000001";
		data.beginTimeChangeRecovery(transition);

		TimeChangeUserProgress first = data.prepareTimeChangeUserStreak(transition, uuid, 8, true);
		TimeChangeUserProgress retry = data.prepareTimeChangeUserStreak(transition, uuid, 9, true);

		assertEquals(8, first.streakTarget());
		assertEquals(8, retry.streakTarget());
		assertFalse(retry.rewardComplete());
		data.completeTimeChangeUserStreakReward(transition, uuid);
		assertTrue(data.prepareTimeChangeUserStreak(transition, uuid, 9, true).rewardComplete());
		data.completeTimeChangeUser(transition, uuid);
		assertEquals(uuid, data.getTimeChangeCursor(transition));
	}

	@Test
	void retryKeepsTheFirstDurableRankedRewardSelection() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		data.beginTimeChangeRecovery(transition);
		List<TimeChangeRewardTarget> original = List.of(
				new TimeChangeRewardTarget("00000000-0000-0000-0000-000000000001", "first", 1, "1", 20),
				new TimeChangeRewardTarget("00000000-0000-0000-0000-000000000002", "second", 2, "2", 10));
		List<TimeChangeRewardTarget> changed = List.of(
				new TimeChangeRewardTarget("00000000-0000-0000-0000-000000000003", "late", 1, "1", 99));

		assertEquals(original, data.prepareTimeChangeRewardTargets(transition, original));
		assertEquals(original, data.prepareTimeChangeRewardTargets(transition, changed));
		assertEquals(original, new ServerData(plugin).getTimeChangeRewardTargets(transition));
	}

	@Test
	void retryKeepsTheFirstDurableArchiveSnapshot() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		TimeChangeArchiveSnapshot original = new TimeChangeArchiveSnapshot(List.of(
				new TimeChangeArchiveSection("Daily", List.of("Combined total: 20", "1: first: 20"))));
		TimeChangeArchiveSnapshot changed = new TimeChangeArchiveSnapshot(List.of(
				new TimeChangeArchiveSection("Daily", List.of("Combined total: 99", "1: late: 99"))));
		data.beginTimeChangeRecovery(transition);

		assertEquals(original, data.prepareTimeChangeArchive(transition, original));
		assertEquals(original, data.prepareTimeChangeArchive(transition, changed));
		assertEquals(original, new ServerData(plugin).getTimeChangeArchive(transition));
	}

	@Test
	void rewardAndArchiveBoundaryDataShareOneDurableCheckpoint() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		List<TimeChangeRewardTarget> targets = List.of(new TimeChangeRewardTarget(
				"00000000-0000-0000-0000-000000000001", "first", 1, "1", 20));
		TimeChangeArchiveSnapshot archive = new TimeChangeArchiveSnapshot(List.of(
				new TimeChangeArchiveSection("Daily", List.of("Combined total: 20", "1: first: 20"))));
		data.beginTimeChangeRecovery(transition);

		data.prepareTimeChangeSnapshot(transition, targets, archive);
		data.prepareTimeChangeSnapshot(transition, List.of(), new TimeChangeArchiveSnapshot(List.of()));

		assertEquals(targets, data.getTimeChangeRewardTargets(transition));
		assertEquals(archive, data.getTimeChangeArchive(transition));
	}

	private TimeChangeTransition transition(String id, String period, TimeType type) {
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(transition.getId()).thenReturn(id);
		when(transition.getPeriodKey()).thenReturn(period);
		when(transition.getType()).thenReturn(type);
		return transition;
	}
}
