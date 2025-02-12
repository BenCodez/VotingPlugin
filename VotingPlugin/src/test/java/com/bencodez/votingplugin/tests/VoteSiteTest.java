package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.objects.VoteSite;

public class VoteSiteTest {

	private VotingPluginMain plugin;

	@BeforeEach
	public void setUp() {
		// Create a mock instance of VotingPluginMain.
		plugin = mock(VotingPluginMain.class);

		// Create dummy stubs using Mockito for ConfigVoteSites.
		ConfigVoteSites dummyConfigVoteSites = mock(ConfigVoteSites.class);
		when(dummyConfigVoteSites.getVoteURL(anyString())).thenReturn("example.com");
		when(dummyConfigVoteSites.getServiceSite(anyString())).thenReturn("ServiceSite");
		when(dummyConfigVoteSites.getVoteDelay(anyString())).thenReturn(30.0);
		when(dummyConfigVoteSites.getVoteDelayMin(anyString())).thenReturn(15.0);
		when(dummyConfigVoteSites.getVoteSiteEnabled(anyString())).thenReturn(true);
		when(dummyConfigVoteSites.getPriority(anyString())).thenReturn(1);
		when(dummyConfigVoteSites.getDisplayName(anyString())).thenReturn("DisplayName");
		when(dummyConfigVoteSites.getItem(anyString())).thenReturn(null);
		// (Stub additional methods as needed)

		// Create a dummy Config.
		Config dummyConfigFile = mock(Config.class);
		when(dummyConfigFile.isFormatCommandsVoteForceLinks()).thenReturn(true);
		when(dummyConfigFile.getFormatBroadCastMsg()).thenReturn("Broadcast: {player} voted on {sitename}");

		// Create a dummy ServerData.
		ServerData dummyServerData = mock(ServerData.class);
		// Instead of casting Arrays.asList, wrap it in a new ArrayList.
		when(dummyServerData.getServiceSites())
				.thenReturn(new ArrayList<>(Arrays.asList("ServiceSite", "AnotherSite")));

		// Stub the plugin methods to return our dummy configuration objects.
		when(plugin.getConfigVoteSites()).thenReturn(dummyConfigVoteSites);
		when(plugin.getConfigFile()).thenReturn(dummyConfigFile);
		when(plugin.getServerData()).thenReturn(dummyServerData);
	}

	@Test
	public void testInitSetsValuesCorrectly() {
		// Construct a VoteSite with site name "site.test".
		VoteSite voteSite = new VoteSite(plugin, "site.test");

		// The key is created by replacing dots with underscores.
		assertEquals("site_test", voteSite.getKey(), "Key should have dots replaced with underscores");

		// The dummy config returns "DisplayName" for display name.
		assertEquals("DisplayName", voteSite.getDisplayName(), "Display name should be loaded from config");

		// voteURL is loaded from dummy and should be "example.com"
		assertEquals("example.com", voteSite.getVoteURL(false), "VoteURL should be set from config");

		// Service site, vote delays, and enabled flag are set as expected.
		assertEquals("ServiceSite", voteSite.getServiceSite(), "ServiceSite should be set from config");
		assertEquals(30.0, voteSite.getVoteDelay(), "VoteDelay should be set from config");
		assertEquals(15.0, voteSite.getVoteDelayMin(), "VoteDelayMin should be set from config");
		assertTrue(voteSite.isEnabled(), "VoteSite should be enabled");
	}

	@Test
	public void testGetVoteURLJson() {
		VoteSite voteSite = new VoteSite(plugin, "site.test");
		// After init, voteURL is "example.com" (from dummy config)
		// In getVoteURL(true), if the format-forced links option is enabled,
		// and since "example.com" does not start with "http", the returned string
		// should be:
		// [Text="example.com",url="http://example.com"]
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
		assertTrue(debug.contains("30.0"), "Debug output should contain the vote delay");
	}
}
