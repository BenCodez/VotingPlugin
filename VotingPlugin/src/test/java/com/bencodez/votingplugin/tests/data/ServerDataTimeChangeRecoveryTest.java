package com.bencodez.votingplugin.tests.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.nio.file.Path;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSection;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSnapshot;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardTarget;
import com.bencodez.votingplugin.data.ServerData.TimeChangeTopPolicy;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardState;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserPolicy;

class ServerDataTimeChangeRecoveryTest {
	@TempDir
	Path temporaryDirectory;

	@Test
	void perUserCursorUsesTheCompactDurableCheckpointWithoutRewritingServerData() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		ServerData data = new ServerData(plugin);
		String uuid = "00000000-0000-0000-0000-000000000002";

		data.beginTimeChangeRecovery(transition);
		clearInvocations(coreData);
		data.completeTimeChangeUser(transition, uuid);
		String nextUuid = "00000000-0000-0000-0000-000000000003";
		data.prepareTimeChangeUserStreak(transition, nextUuid, 4, true);
		data.claimTimeChangeUserStreakReward(transition, nextUuid);

		verify(coreData, never()).saveData();
		ServerData recovered = new ServerData(plugin);
		assertEquals(uuid, recovered.getTimeChangeCursor(transition));
		assertEquals(TimeChangeRewardState.CLAIMED,
				recovered.getTimeChangeUserStreakRewardState(transition, nextUuid));
		assertTrue(temporaryDirectory.resolve("TimeChangeUserCheckpoint.properties").toFile().length() < 4096L);
	}

	@Test
	void compactCheckpointImportsAnExistingServerDataCursor() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getDataFolder()).thenReturn(temporaryDirectory.toFile());
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("MONTH:2026-08", "2026-08", TimeType.MONTH);
		String cursor = "00000000-0000-0000-0000-000000000009";
		yaml.set("VotingPlugin.TimeChangeRecovery.MONTH.Id", transition.getId());
		yaml.set("VotingPlugin.TimeChangeRecovery.MONTH.Cursor", cursor);

		ServerData data = new ServerData(plugin);
		data.beginTimeChangeRecovery(transition);

		assertEquals(cursor, data.getTimeChangeCursor(transition));
		assertEquals(cursor, new ServerData(plugin).getTimeChangeCursor(transition));
	}

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
	void retryKeepsTheDailyStreakBoundaryDecisionFromTransitionStart() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		ServerData data = new ServerData(plugin);
		data.beginTimeChangeRecovery(transition);

		TimeChangeUserPolicy original = new TimeChangeUserPolicy(false, true, false, true, 50, 60, 70, true, false, true);
		TimeChangeUserPolicy changed = new TimeChangeUserPolicy(true, false, true, false, 1, 2, 3, false, true, false);
		assertEquals(original, data.prepareTimeChangeUserPolicy(transition, original));
		assertEquals(original, data.prepareTimeChangeUserPolicy(transition, changed));
		assertEquals(original, new ServerData(plugin).getTimeChangeUserPolicy(transition));
	}

	@Test
	void failedDailyStreakBoundarySaveDoesNotPublishTheDecision() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		ServerData data = new ServerData(plugin);
		data.beginTimeChangeRecovery(transition);
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();

		TimeChangeUserPolicy policy = new TimeChangeUserPolicy(true, true, true, true, 50, 60, 70, true, false, true);
		assertThrows(IllegalStateException.class, () -> data.prepareTimeChangeUserPolicy(transition, policy));

		assertThrows(IllegalStateException.class, () -> data.getTimeChangeUserPolicy(transition));
		doNothing().when(coreData).saveData();
		assertEquals(policy, data.prepareTimeChangeUserPolicy(transition, policy));
	}

	@Test
	void retryKeepsTheVoteShopTargetsSelectedBeforeAnyReset() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		ServerData data = new ServerData(plugin);
		data.beginTimeChangeRecovery(transition);

		assertEquals(List.of("weekly"),
				data.prepareTimeChangeVoteShopTargets(transition, List.of("weekly")));
		assertEquals(List.of("weekly"),
				data.prepareTimeChangeVoteShopTargets(transition, List.of("newly-enabled")));
		assertEquals(List.of("weekly"), new ServerData(plugin).getTimeChangeVoteShopTargets(transition));
	}

	@Test
	void retryKeepsTopRewardAndArchivePolicySelectedBeforeTheBoundary() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		ServerData data = new ServerData(plugin);
		data.beginTimeChangeRecovery(transition);
		TimeChangeTopPolicy original = new TimeChangeTopPolicy(true, true, true, true,
				List.of("1", "2-4"), List.of("blocked"));
		TimeChangeTopPolicy changed = new TimeChangeTopPolicy(false, false, false, false,
				List.of("5"), List.of());

		assertEquals(original, data.prepareTimeChangeTopPolicy(transition, original));
		assertEquals(original, data.prepareTimeChangeTopPolicy(transition, changed));
		assertEquals(original, new ServerData(plugin).getTimeChangeTopPolicy(transition));
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
	void durableRewardClaimsPreventAmbiguousAutomaticReplay() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		String uuid = "00000000-0000-0000-0000-000000000001";
		data.beginTimeChangeRecovery(transition);

		assertEquals(TimeChangeRewardState.UNCLAIMED, data.getTimeChangeRewardState(transition, uuid));
		data.claimTimeChangeReward(transition, uuid);
		assertEquals(TimeChangeRewardState.CLAIMED, data.getTimeChangeRewardState(transition, uuid));
		assertThrows(IllegalStateException.class, () -> data.claimTimeChangeReward(transition, uuid));
		data.completeTimeChangeReward(transition, uuid);
		assertEquals(TimeChangeRewardState.COMPLETE, data.getTimeChangeRewardState(transition, uuid));
		assertTrue(data.hasTimeChangeRewardReceipt(transition, uuid));
	}

	@Test
	void failedRewardClaimSaveRollsBackToUnclaimed() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		String uuid = "00000000-0000-0000-0000-000000000001";
		data.beginTimeChangeRecovery(transition);
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();

		assertThrows(IllegalStateException.class, () -> data.claimTimeChangeReward(transition, uuid));
		assertEquals(TimeChangeRewardState.UNCLAIMED, data.getTimeChangeRewardState(transition, uuid));
	}

	@Test
	void failedRewardCompletionRemainsClaimed() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("DAY:2026-09-21", "2026-09-21", TimeType.DAY);
		String uuid = "00000000-0000-0000-0000-000000000001";
		data.beginTimeChangeRecovery(transition);
		data.claimTimeChangeReward(transition, uuid);
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();

		assertThrows(IllegalStateException.class, () -> data.completeTimeChangeReward(transition, uuid));
		assertEquals(TimeChangeRewardState.CLAIMED, data.getTimeChangeRewardState(transition, uuid));
		assertFalse(data.hasTimeChangeRewardReceipt(transition, uuid));
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

	@Test
	void votePartyStateResetIsPreparedBeforeItsEffectReceipt() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		data.beginTimeChangeRecovery(transition);
		data.getData().set("VoteParty.Total", 12);
		data.getData().set("VoteParty.Voted", List.of("player"));

		data.prepareTimeChangeVotePartyReset(transition, "VotePartyWeekReset");
		data.completeTimeChangeVotePartyReset(transition, "VotePartyWeekReset");

		assertEquals(0, data.getData().getInt("VoteParty.Total"));
		assertTrue(data.getData().getStringList("VoteParty.Voted").isEmpty());
		assertTrue(data.hasTimeChangeEffect(transition, "VotePartyWeekReset"));
	}

	@Test
	void failedVotePartyStateSaveKeepsTheLogicalResetForLaterVotes() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData = mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(plugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(plugin);
		TimeChangeTransition transition = transition("WEEK:2026-W38", "2026-W38", TimeType.WEEK);
		data.beginTimeChangeRecovery(transition);
		data.getData().set("VoteParty.Total", 12);
		data.getData().set("VoteParty.Voted", List.of("player"));
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();

		assertThrows(IllegalStateException.class,
				() -> data.prepareTimeChangeVotePartyReset(transition, "VotePartyWeekReset"));

		assertEquals(0, data.getData().getInt("VoteParty.Total"));
		assertTrue(data.getData().getStringList("VoteParty.Voted").isEmpty());
		assertFalse(data.hasTimeChangeEffect(transition, "VotePartyWeekReset"));

		data.getData().set("VoteParty.Total", 2);
		data.getData().set("VoteParty.Voted", List.of("new-player"));
		doNothing().when(coreData).saveData();
		data.prepareTimeChangeVotePartyReset(transition, "VotePartyWeekReset");
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();
		assertThrows(IllegalStateException.class,
				() -> data.completeTimeChangeVotePartyReset(transition, "VotePartyWeekReset"));
		assertEquals(2, data.getData().getInt("VoteParty.Total"));
		assertEquals(List.of("new-player"), data.getData().getStringList("VoteParty.Voted"));
		assertFalse(data.hasTimeChangeEffect(transition, "VotePartyWeekReset"));

		doNothing().when(coreData).saveData();
		data.completeTimeChangeVotePartyReset(transition, "VotePartyWeekReset");
		assertEquals(2, data.getData().getInt("VoteParty.Total"));
		assertEquals(List.of("new-player"), data.getData().getStringList("VoteParty.Voted"));
		assertTrue(data.hasTimeChangeEffect(transition, "VotePartyWeekReset"));

		data.getData().set("VotePartyExtraRequired", 7);
		doThrow(new IllegalStateException("disk unavailable")).when(coreData).saveData();
		assertThrows(IllegalStateException.class,
				() -> data.completeTimeChangeVotePartyExtraReset(transition, "VotePartyWeekExtraVotes"));
		assertEquals(7, data.getData().getInt("VotePartyExtraRequired"));
		assertFalse(data.hasTimeChangeEffect(transition, "VotePartyWeekExtraVotes"));
	}

	private TimeChangeTransition transition(String id, String period, TimeType type) {
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		when(transition.getId()).thenReturn(id);
		when(transition.getPeriodKey()).thenReturn(period);
		when(transition.getType()).thenReturn(type);
		return transition;
	}
}
