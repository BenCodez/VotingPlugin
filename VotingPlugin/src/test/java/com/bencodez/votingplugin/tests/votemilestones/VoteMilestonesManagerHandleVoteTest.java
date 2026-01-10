package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TimeZone;

import org.bukkit.configuration.MemoryConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.bencodez.advancedcore.api.user.UserData;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.specialrewards.votemilestones.AtMatcher;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneGroupSelect;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneLimit;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneTotal;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesConfig;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesManager;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.var;

class VoteMilestonesManagerHandleVoteTest {

	private AutoCloseable mocks;
	private TimeZone oldTz;

	@Mock
	private VotingPluginMain plugin;

	@Mock
	private VotingPluginUser user;

	@BeforeEach
	void setUp() throws Exception {
		mocks = MockitoAnnotations.openMocks(this);

		oldTz = TimeZone.getDefault();
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

		// Provide a FileConfiguration instance (avoid MemoryConfiguration vs
		// FileConfiguration mismatch)
		SpecialRewardsConfig rewardsConfig = mock(SpecialRewardsConfig.class);
		when(plugin.getSpecialRewardsConfig()).thenReturn(rewardsConfig);
		when(rewardsConfig.getData()).thenReturn(new YamlConfiguration());

		// Silence debug output
		doNothing().when(plugin).debug(anyString());

		// Common user basics
		when(user.getJavaUUID()).thenReturn(java.util.UUID.randomUUID());
		when(user.getPlayerName()).thenReturn("TestPlayer");
	}

	@AfterEach
	void tearDown() throws Exception {
		TimeZone.setDefault(oldTz);
		mocks.close();
	}

	@Test
	void handleVote_groupHighest_executesOnlyBestMatch_viaLimitStoreWrite() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		// Group modes: group "g" uses HIGHEST selection
		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("g", VoteMilestoneGroupSelect.HIGHEST);
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		// Total value used by VoteMilestoneTotal.ALLTIME_VOTES
		when(user.getTotal(TopVoter.AllTime)).thenReturn(5);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);
		when(ud.getString("VoteMilestoneLimits")).thenReturn(null);

		AtMatcher at5 = atMatcherSingle(5);

		// Use a limit so we can observe execution via blob write; rewardPath is empty
		// to avoid RewardHandler init.
		VoteMilestoneLimit cooldown = new VoteMilestoneLimit(VoteMilestoneLimit.Type.COOLDOWN, 60_000L, 0);

		VoteMilestone A = new VoteMilestone("A", true, VoteMilestoneTotal.ALLTIME_VOTES, at5, null, "",
				VoteMilestoneGroupSelect.ALL, "g", cooldown);

		VoteMilestone B = new VoteMilestone("B", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 5, "",
				VoteMilestoneGroupSelect.ALL, "g", cooldown);

		setField(mgr, "config", configWith(A, B));

		mgr.handleVote(user, null, null);

		ArgumentCaptor<String> blobCaptor = ArgumentCaptor.forClass(String.class);
		verify(ud, times(1)).setString(eq("VoteMilestoneLimits"), blobCaptor.capture());

		String blob = blobCaptor.getValue();
		assertTrue(blob.contains("A|"), "Expected limits blob to contain A");
		assertFalse(blob.contains("B|"), "Expected limits blob NOT to contain B (HIGHEST should pick AT milestone)");
	}

	@Test
	void handleVote_limitWindowDay_blocksSameDayRetrigger_noWrite() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		when(user.getTotal(TopVoter.AllTime)).thenReturn(5);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);

		AtMatcher at5 = atMatcherSingle(5);

		VoteMilestoneLimit day = new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0L, 0);

		VoteMilestone C = new VoteMilestone("C", true, VoteMilestoneTotal.ALLTIME_VOTES, at5, null, "",
				VoteMilestoneGroupSelect.ALL, "default", day);

		setField(mgr, "config", configWith(C));

		// Use "now" so it is guaranteed to be in the same window under any sane
		// WINDOW_DAY interpretation
		long lastTriggered = System.currentTimeMillis();
		when(ud.getString("VoteMilestoneLimits")).thenReturn("C|" + lastTriggered);

		mgr.handleVote(user, null, null);

		verify(ud, never()).setString(eq("VoteMilestoneLimits"), anyString());
	}

	@Test
	void handleVote_groupFirst_executesFirstEncountered_viaLimitStoreWrite() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("g", VoteMilestoneGroupSelect.FIRST);
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		when(user.getTotal(TopVoter.AllTime)).thenReturn(10);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);
		when(ud.getString("VoteMilestoneLimits")).thenReturn(null);

		VoteMilestoneLimit cd = new VoteMilestoneLimit(VoteMilestoneLimit.Type.COOLDOWN, 60_000L, 0);

		// Both match: EVERY 5 at total 10. FIRST should pick the first encountered
		// ("First").
		VoteMilestone first = new VoteMilestone("First", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 5, "",
				VoteMilestoneGroupSelect.ALL, "g", cd);

		VoteMilestone second = new VoteMilestone("Second", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 5, "",
				VoteMilestoneGroupSelect.ALL, "g", cd);

		setField(mgr, "config", configWith(first, second));

		mgr.handleVote(user, null, null);

		ArgumentCaptor<String> blobCaptor = ArgumentCaptor.forClass(String.class);
		verify(ud, times(1)).setString(eq("VoteMilestoneLimits"), blobCaptor.capture());

		String blob = blobCaptor.getValue();
		assertTrue(blob.contains("First|"));
		assertFalse(blob.contains("Second|"));
	}

	@Test
	void handleVote_twoGroups_bothExecute_andBothPersisted() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("g1", VoteMilestoneGroupSelect.ALL);
		modes.put("g2", VoteMilestoneGroupSelect.ALL);
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		when(user.getTotal(TopVoter.AllTime)).thenReturn(10);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);
		when(ud.getString("VoteMilestoneLimits")).thenReturn(null);

		VoteMilestoneLimit cd = new VoteMilestoneLimit(VoteMilestoneLimit.Type.COOLDOWN, 60_000L, 0);

		VoteMilestone m1 = new VoteMilestone("M1", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 5, "",
				VoteMilestoneGroupSelect.ALL, "g1", cd);

		VoteMilestone m2 = new VoteMilestone("M2", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 10, "",
				VoteMilestoneGroupSelect.ALL, "g2", cd);

		setField(mgr, "config", configWith(m1, m2));

		mgr.handleVote(user, null, null);

		ArgumentCaptor<String> blobCaptor = ArgumentCaptor.forClass(String.class);
		verify(ud, times(1)).setString(eq("VoteMilestoneLimits"), blobCaptor.capture());

		String blob = blobCaptor.getValue();
		assertTrue(blob.contains("M1|"));
		assertTrue(blob.contains("M2|"));
	}

	@Test
	void handleVote_cooldownBlocksImmediateRetrigger_secondCallNoWrite() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		when(user.getTotal(TopVoter.AllTime)).thenReturn(5);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);

		AtMatcher at5 = atMatcherSingle(5);

		VoteMilestoneLimit cd = new VoteMilestoneLimit(VoteMilestoneLimit.Type.COOLDOWN, 60_000L, 0);

		VoteMilestone m = new VoteMilestone("CD", true, VoteMilestoneTotal.ALLTIME_VOTES, at5, null, "",
				VoteMilestoneGroupSelect.ALL, "default", cd);

		setField(mgr, "config", configWith(m));

		// First call: empty blob => should write
		when(ud.getString("VoteMilestoneLimits")).thenReturn(null);

		mgr.handleVote(user, null, null);

		verify(ud, times(1)).setString(eq("VoteMilestoneLimits"), contains("CD|"));

		// Capture written blob and feed it back for second call
		ArgumentCaptor<String> blobCaptor = ArgumentCaptor.forClass(String.class);
		verify(ud).setString(eq("VoteMilestoneLimits"), blobCaptor.capture());
		String saved = blobCaptor.getValue();

		// Reset interaction history for second call
		reset(ud);
		when(user.getUserData()).thenReturn(ud);
		when(ud.getString("VoteMilestoneLimits")).thenReturn(saved);

		mgr.handleVote(user, null, null);

		verify(ud, never()).setString(eq("VoteMilestoneLimits"), anyString());
	}

	@Test
	void handleVote_limitWindowDay_allowsNextDay_writeOccurs() throws Exception {
		VoteMilestonesManager mgr = new VoteMilestonesManager(plugin);

		Map<String, VoteMilestoneGroupSelect> modes = new LinkedHashMap<>();
		modes.put("default", VoteMilestoneGroupSelect.ALL);
		setField(mgr, "groupModes", modes);

		when(user.getTotal(TopVoter.AllTime)).thenReturn(5);

		UserData ud = mock(UserData.class);
		when(user.getUserData()).thenReturn(ud);

		AtMatcher at5 = atMatcherSingle(5);

		VoteMilestoneLimit day = new VoteMilestoneLimit(VoteMilestoneLimit.Type.WINDOW_DAY, 0L, 0);

		VoteMilestone C = new VoteMilestone("C", true, VoteMilestoneTotal.ALLTIME_VOTES, at5, null, "",
				VoteMilestoneGroupSelect.ALL, "default", day);

		setField(mgr, "config", configWith(C));

		// Set lastTriggered safely in the past: 2 days ago
		long lastTriggered = ZonedDateTime.now(ZoneId.of("UTC")).minusDays(2).toInstant().toEpochMilli();
		when(ud.getString("VoteMilestoneLimits")).thenReturn("C|" + lastTriggered);

		mgr.handleVote(user, null, null);

		verify(ud, times(1)).setString(eq("VoteMilestoneLimits"), contains("C|"));
	}

	/* ---------------- helpers ---------------- */

	private static AtMatcher atMatcherSingle(long value) {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", value);
		return AtMatcher.fromConfig(c, "At");
	}

	private static VoteMilestonesConfig configWith(VoteMilestone... milestones) throws Exception {
		var ctor = VoteMilestonesConfig.class.getDeclaredConstructor(Map.class);
		ctor.setAccessible(true);

		Map<String, VoteMilestone> map = new LinkedHashMap<>();
		for (VoteMilestone m : milestones) {
			map.put(m.getId(), m);
		}
		return ctor.newInstance(map);
	}

	private static void setField(Object target, String fieldName, Object value) throws Exception {
		Field f = target.getClass().getDeclaredField(fieldName);
		f.setAccessible(true);
		f.set(target, value);
	}
}
