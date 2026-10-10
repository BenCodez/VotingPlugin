package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;

class UserManagerOfflineVoteClearTest {
	private static final UUID FIRST = UUID.fromString("00000000-0000-0000-0000-000000000001");
	private static final UUID SECOND = UUID.fromString("00000000-0000-0000-0000-000000000002");

	@Test
	void pendingBatchRejectsBulkClearBeforeAnyQueueIsWiped() {
		Fixture fixture = fixture();
		when(fixture.second.getPendingOfflineVoteRewardBatch()).thenReturn(new ArrayList<>(List.of("Site1")));
		assertThrows(IllegalStateException.class, fixture.manager::clearAllOfflineVotes);
		verify(fixture.plugin.getUserManager(), never()).removeAllKeyValues(any(), any());
	}

	@Test
	void activeDeliveryRejectsBulkClearEvenBeforePendingIsVisible() {
		Fixture fixture = fixture();
		when(fixture.first.isOfflineVoteRewardReplayActive()).thenReturn(true);
		assertThrows(IllegalStateException.class, fixture.manager::clearAllOfflineVotes);
		verify(fixture.plugin.getUserManager(), never()).removeAllKeyValues(any(), any());
	}

	@Test
	void ordinaryBulkClearRetainsExistingColumnOperation() {
		Fixture fixture = fixture();
		fixture.manager.clearAllOfflineVotes();
		verify(fixture.first).cache();
		verify(fixture.second).cache();
		verify(fixture.plugin.getUserManager()).removeAllKeyValues("OfflineVotes", DataType.STRING);
	}

	private static Fixture fixture() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
		UserManager manager = spy(new UserManager(plugin));
		doReturn(new ArrayList<>(List.of(FIRST.toString(), SECOND.toString()))).when(manager).getAllUUIDs();
		VotingPluginUser first = mock(VotingPluginUser.class);
		VotingPluginUser second = mock(VotingPluginUser.class);
		when(first.getPendingOfflineVoteRewardBatch()).thenReturn(new ArrayList<>());
		when(second.getPendingOfflineVoteRewardBatch()).thenReturn(new ArrayList<>());
		doReturn(first).when(manager).getVotingPluginUser(FIRST, false);
		doReturn(second).when(manager).getVotingPluginUser(SECOND, false);
		return new Fixture(plugin, manager, first, second);
	}

	private record Fixture(VotingPluginMain plugin, UserManager manager,
			VotingPluginUser first, VotingPluginUser second) { }
}
