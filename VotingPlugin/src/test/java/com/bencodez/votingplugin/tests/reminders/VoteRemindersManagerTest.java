package com.bencodez.votingplugin.tests.reminders;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.votereminding.VoteRemindersManager;
import com.bencodez.votingplugin.votereminding.store.VoteReminderCooldownStore;

@ExtendWith(MockitoExtension.class)
public class VoteRemindersManagerTest {

	@Test
	public void cooldownWrapper_tryAcquireGlobal_usesStoreWhenNonZero() {
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID uuid = UUID.randomUUID();

		ParsedDuration global = ParsedDuration.parse("10s");
		VoteRemindersManager.VoteReminderCooldowns cd = new VoteRemindersManager.VoteReminderCooldowns(store, global);

		when(store.tryClaimGlobal(eq(uuid), eq(1000L), eq(10_000L))).thenReturn(true);

		assertTrue(cd.tryAcquireGlobal(uuid, 1000L));
		verify(store).tryClaimGlobal(uuid, 1000L, 10_000L);
	}

	@Test
	public void cooldownWrapper_canFireReminder_usesMaxOfCooldownAndInterval() {
	    VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
	    UUID uuid = UUID.randomUUID();

	    // last fire was at t=1000 (non-zero, so cooldown applies)
	    when(store.getPerReminderMap(uuid)).thenReturn(Collections.singletonMap("Basic", 1000L));

	    VoteRemindersManager.VoteReminderCooldowns cd =
	            new VoteRemindersManager.VoteReminderCooldowns(store, ParsedDuration.parse(""));

	    // req = max(5s, 30s) = 30s
	    assertFalse(cd.canFireReminder(uuid, "Basic", 10_000L,
	            ParsedDuration.parse("5s"), ParsedDuration.parse("30s"))); // 9s since last -> should be blocked

	    assertTrue(cd.canFireReminder(uuid, "Basic", 50_000L,
	            ParsedDuration.parse("5s"), ParsedDuration.parse("30s"))); // 49s since last -> allowed
	}


	@Test
	public void cooldownWrapper_markFired_delegatesToStore() {
		VoteReminderCooldownStore store = mock(VoteReminderCooldownStore.class);
		UUID uuid = UUID.randomUUID();

		VoteRemindersManager.VoteReminderCooldowns cd = new VoteRemindersManager.VoteReminderCooldowns(store,
				ParsedDuration.parse(""));

		cd.markFired(uuid, "Basic", 123L);
		verify(store).setPerReminderLast(uuid, "Basic", 123L);
	}
}
