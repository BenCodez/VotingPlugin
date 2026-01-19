package com.bencodez.votingplugin.tests.reminders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votereminding.store.UserDataVoteReminderCooldownStore;

public class UserDataVoteReminderCooldownStoreTest {

	@Test
	public void tryClaimGlobal_returnsTrue_whenCooldownDisabled() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		UserDataVoteReminderCooldownStore store = new UserDataVoteReminderCooldownStore(plugin);

		assertTrue(store.tryClaimGlobal(UUID.randomUUID(), 1000L, 0L));
	}

	@Test
	public void tryClaimGlobal_enforcesCooldown_andWritesLast() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);

		UUID uuid = UUID.randomUUID();
		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);

		// First claim should write nowMs
		when(user.getUserData().getString(UserDataVoteReminderCooldownStore.KEY_GLOBAL_LAST)).thenReturn("0");
		UserDataVoteReminderCooldownStore store = new UserDataVoteReminderCooldownStore(plugin);

		assertTrue(store.tryClaimGlobal(uuid, 10_000L, 5_000L));
		verify(user.getUserData()).setString(UserDataVoteReminderCooldownStore.KEY_GLOBAL_LAST, "10000");

		// Second claim within cooldown should fail (simulate stored last=10000)
		reset(user);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);
		when(user.getUserData().getString(UserDataVoteReminderCooldownStore.KEY_GLOBAL_LAST)).thenReturn("10000");

		assertFalse(store.tryClaimGlobal(uuid, 12_000L, 5_000L)); // 2s later, needs 5s
		verify(user.getUserData(), never()).setString(eq(UserDataVoteReminderCooldownStore.KEY_GLOBAL_LAST), anyString());
	}

	@Test
	public void getPerReminderMap_parsesAndCaches() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		UUID uuid = UUID.randomUUID();

		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);

		when(user.getUserData().getString(UserDataVoteReminderCooldownStore.KEY_MAP))
				.thenReturn("Basic=100;OnLogin=200;Bad=abc;Zero=0;;NoEq");

		UserDataVoteReminderCooldownStore store = new UserDataVoteReminderCooldownStore(plugin);

		Map<String, Long> m1 = store.getPerReminderMap(uuid);
		assertEquals(100L, m1.get("Basic"));
		assertEquals(200L, m1.get("OnLogin"));
		assertFalse(m1.containsKey("Bad"));
		assertFalse(m1.containsKey("Zero"));

		// cached: second call should not need another getString read
		store.getPerReminderMap(uuid);
		verify(user.getUserData(), times(1)).getString(UserDataVoteReminderCooldownStore.KEY_MAP);
	}

	@Test
	public void setPerReminderLast_updatesMap_andEncodesSorted() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		UUID uuid = UUID.randomUUID();

		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);

		// Start with unsorted
		when(user.getUserData().getString(UserDataVoteReminderCooldownStore.KEY_MAP)).thenReturn("z=1;A=2");

		UserDataVoteReminderCooldownStore store = new UserDataVoteReminderCooldownStore(plugin);

		store.setPerReminderLast(uuid, "m", 50L);

		// encodeMap sorts case-insensitive by key, and only keeps >0 values
		verify(user.getUserData()).setString(eq(UserDataVoteReminderCooldownStore.KEY_MAP), eq("A=2;m=50;z=1"));
	}

	@Test
	public void clearCache_forcesReparseNextTime() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		UUID uuid = UUID.randomUUID();

		VotingPluginUser user = mock(VotingPluginUser.class, RETURNS_DEEP_STUBS);
		when(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false)).thenReturn(user);

		when(user.getUserData().getString(UserDataVoteReminderCooldownStore.KEY_MAP)).thenReturn("A=1");

		UserDataVoteReminderCooldownStore store = new UserDataVoteReminderCooldownStore(plugin);

		store.getPerReminderMap(uuid);
		store.clearCache(uuid);
		store.getPerReminderMap(uuid);

		verify(user.getUserData(), times(2)).getString(UserDataVoteReminderCooldownStore.KEY_MAP);
	}
}
