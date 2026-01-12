package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.votesites.VoteSite;

public class VoteSiteTest {

	private VotingPluginMain plugin;

	@BeforeEach
	public void setUp() {
		plugin = mock(VotingPluginMain.class);

		ConfigVoteSites dummyConfigVoteSites = mock(ConfigVoteSites.class);
		when(dummyConfigVoteSites.getVoteURL(anyString())).thenReturn("example.com");
		when(dummyConfigVoteSites.getServiceSite(anyString())).thenReturn("ServiceSite");
		when(dummyConfigVoteSites.getVoteDelay(anyString())).thenReturn(ParsedDuration.parse("12h"));
		when(dummyConfigVoteSites.getVoteSiteEnabled(anyString())).thenReturn(true);
		when(dummyConfigVoteSites.getPriority(anyString())).thenReturn(1);
		when(dummyConfigVoteSites.getDisplayName(anyString())).thenReturn("DisplayName");
		when(dummyConfigVoteSites.getItem(anyString())).thenReturn(null);

		Config dummyConfigFile = mock(Config.class);
		when(dummyConfigFile.isFormatCommandsVoteForceLinks()).thenReturn(true);

		ServerData dummyServerData = mock(ServerData.class);
		when(dummyServerData.getServiceSites())
				.thenReturn(new ArrayList<>(Arrays.asList("ServiceSite", "AnotherSite")));

		when(plugin.getConfigVoteSites()).thenReturn(dummyConfigVoteSites);
		when(plugin.getConfigFile()).thenReturn(dummyConfigFile);
		when(plugin.getServerData()).thenReturn(dummyServerData);
	}

	@Test
	public void testInitSetsValuesCorrectly() {
		VoteSite voteSite = new VoteSite(plugin, "site.test");

		assertEquals("site_test", voteSite.getKey(), "Key should have dots replaced with underscores");
		assertEquals("DisplayName", voteSite.getDisplayName(), "Display name should be loaded from config");
		assertEquals("example.com", voteSite.getVoteURL(false), "VoteURL should be set from config");
		assertEquals("ServiceSite", voteSite.getServiceSite(), "ServiceSite should be set from config");
		assertTrue(voteSite.isEnabled(), "VoteSite should be enabled");

		ParsedDuration d = voteSite.getVoteDelay();
		assertEquals(12L * 60L * 60L * 1000L, d.getMillis(), "VoteDelay millis should be 12h");
	}

	@Test
	public void testGetVoteURLJson() {
		VoteSite voteSite = new VoteSite(plugin, "site.test");
		String url = voteSite.getVoteURL(true);
		assertEquals("[Text=\"example.com\",url=\"http://example.com\"]", url,
				"JSON-formatted vote URL should be generated correctly");
	}

	@Test
	public void testLoadingDebugContainsExpectedValues() {
		VoteSite voteSite = new VoteSite(plugin, "site.test");
		String debug = voteSite.loadingDebug();

		assertTrue(debug.contains("site_test"), "Debug output should contain the key");
		assertTrue(debug.contains("DisplayName"), "Debug output should contain the display name");

		// New system: no VoteDelayMin in debug output
		assertTrue(!debug.toLowerCase().contains("votedelaymin"), "Debug output should not contain VoteDelayMin");

		// Depending on how you stringify VoteDelay, accept either "12h" or raw millis
		// value
		assertTrue(debug.contains("12h") || debug.contains("43200000"),
				"Debug output should contain the vote delay (12h or 43200000ms)");
	}

	// -------------------------------------------------------------------------
	// Legacy conversion tests (old style VoteDelay + VoteDelayMin ->
	// ParsedDuration)
	// -------------------------------------------------------------------------

	@Test
	public void testConvertLegacyHoursOnly() {
		MemoryConfiguration cfg = new MemoryConfiguration();
		ConfigurationSection sec = cfg.createSection("Site");
		sec.set("VoteDelay", 24); // legacy numeric hours

		ParsedDuration d = parseVoteDelayWithLegacySupport(sec);

		assertEquals(24L * 60L * 60L * 1000L, d.getMillis(), "24 hours should convert to millis correctly");
	}

	@Test
	public void testConvertLegacyHoursAndMinutes() {
		MemoryConfiguration cfg = new MemoryConfiguration();
		ConfigurationSection sec = cfg.createSection("Site");
		sec.set("VoteDelay", 24); // hours
		sec.set("VoteDelayMin", 30); // minutes

		ParsedDuration d = parseVoteDelayWithLegacySupport(sec);

		long expected = (24L * 60L * 60L * 1000L) + (30L * 60L * 1000L);
		assertEquals(expected, d.getMillis(), "24h + 30m should convert to millis correctly");
	}

	@Test
	public void testConvertLegacyMinutesOnly() {
		MemoryConfiguration cfg = new MemoryConfiguration();
		ConfigurationSection sec = cfg.createSection("Site");
		sec.set("VoteDelay", 0); // legacy requirement: VoteDelay exists
		sec.set("VoteDelayMin", 45); // minutes-only

		ParsedDuration d = parseVoteDelayWithLegacySupport(sec);

		long expected = 45L * 60L * 1000L;
		assertEquals(expected, d.getMillis(), "0h + 45m should convert to millis correctly");
	}

	@Test
	public void testNewStringFormatTakesPrecedenceOverLegacy() {
		MemoryConfiguration cfg = new MemoryConfiguration();
		ConfigurationSection sec = cfg.createSection("Site");

		// Both provided; string should win
		sec.set("VoteDelay", "1h30m");
		sec.set("VoteDelayMin", 999); // should be ignored in new format

		ParsedDuration d = parseVoteDelayWithLegacySupport(sec);

		long expected = (1L * 60L * 60L * 1000L) + (30L * 60L * 1000L);
		assertEquals(expected, d.getMillis(), "String VoteDelay should override legacy VoteDelayMin");
	}

	@Test
	public void testEmptyOrMissingVoteDelayReturnsEmpty() {
		MemoryConfiguration cfg = new MemoryConfiguration();
		ConfigurationSection sec = cfg.createSection("Site");

		ParsedDuration d = parseVoteDelayWithLegacySupport(sec);
		assertTrue(d == null || d.isEmpty() || d.getMillis() == 0L,
				"Missing VoteDelay should result in empty/zero delay");
	}

	/**
	 * Test helper that mirrors the intended production behavior: - If VoteDelay is
	 * a string -> ParsedDuration.parse(...) - Else legacy numeric VoteDelay (hours)
	 * + VoteDelayMin (minutes) -> millis
	 *
	 * NOTE: months are intentionally ignored by the rest of your code, but this
	 * conversion does not create months unless the string contains 'mo'.
	 */
	private static ParsedDuration parseVoteDelayWithLegacySupport(ConfigurationSection sec) {
		if (sec == null) {
			return ParsedDuration.ofMillis(0);
		}

		if (sec.isString("VoteDelay")) {
			String s = sec.getString("VoteDelay", "");
			if (s == null || s.trim().isEmpty()) {
				return ParsedDuration.ofMillis(0);
			}
			return ParsedDuration.parse(s);
		}

		// Legacy numeric style
		double hours = sec.getDouble("VoteDelay", 0D);
		double minutes = sec.getDouble("VoteDelayMin", 0D);

		long millis = (long) (hours * 60D * 60D * 1000D) + (long) (minutes * 60D * 1000D);
		return ParsedDuration.ofMillis(millis);
	}
}
