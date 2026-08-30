package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteManager;

/**
 * Regression tests for configured-but-disabled vote sites.
 */
public class VoteSiteManagerDisabledVoteSiteTest {

	private VotingPluginMain plugin;

	private ConfigVoteSites voteSitesConfig;

	private Config configFile;

	private VoteSiteManager manager;

	@BeforeEach
	public void setUp() {
		plugin = mock(VotingPluginMain.class);
		voteSitesConfig = mock(ConfigVoteSites.class);
		configFile = mock(Config.class);

		when(plugin.getConfigVoteSites()).thenReturn(voteSitesConfig);
		when(plugin.getConfigFile()).thenReturn(configFile);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("VoteSiteManagerDisabledVoteSiteTest"));

		when(voteSitesConfig.getVoteURL(anyString())).thenReturn("example.com");
		when(voteSitesConfig.getServiceSite(anyString())).thenReturn("ServiceSite");
		when(voteSitesConfig.getVoteDelay(anyString())).thenReturn(ParsedDuration.parse("12h", TimeUnit.HOURS));
		when(voteSitesConfig.getVoteSiteEnabled(anyString())).thenReturn(true);
		when(voteSitesConfig.getPriority(anyString())).thenReturn(1);
		when(voteSitesConfig.getDisplayName(anyString())).thenReturn("DisplayName");
		when(voteSitesConfig.getItem(anyString())).thenReturn(null);
		when(voteSitesConfig.getVoteSiteResetVoteDelayDaily(anyString())).thenReturn(false);
		when(voteSitesConfig.getVoteSiteGiveOffline(anyString())).thenReturn(false);
		when(voteSitesConfig.getWaitUntilVoteDelay(anyString())).thenReturn(false);
		when(voteSitesConfig.getVoteDelayDailyHour(anyString())).thenReturn(0);
		when(voteSitesConfig.getVoteSiteHidden(anyString())).thenReturn(false);
		when(voteSitesConfig.getVoteSiteIgnoreCanVote(anyString())).thenReturn(false);
		when(voteSitesConfig.getPermissionToView(anyString())).thenReturn("");

		ServerData serverData = mock(ServerData.class);
		when(serverData.getServiceSites()).thenReturn(new ArrayList<String>(Arrays.asList("ServiceSite")));
		when(plugin.getServerData()).thenReturn(serverData);

		manager = new VoteSiteManager(plugin);
	}

	/**
	 * Configures one disabled vote-site section that is intentionally absent from
	 * the manager's loaded vote-site list.
	 *
	 * @param key the configured vote-site key
	 * @param serviceSite the configured service site
	 * @param displayName the configured display name
	 */
	private void configureDisabledVoteSite(String key, String serviceSite, String displayName) {
		when(voteSitesConfig.getRawVoteSiteNames())
				.thenReturn(new ArrayList<String>(Arrays.asList(key)));
		when(voteSitesConfig.getVoteSiteEnabled(key)).thenReturn(false);
		when(voteSitesConfig.getServiceSite(key)).thenReturn(serviceSite);
		when(voteSitesConfig.getDisplayName(key)).thenReturn(displayName);

		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>()));
	}

	@Test
	public void testDisabledConfiguredVoteSiteMatchesEverySupportedIdentifier() {
		configureDisabledVoteSite("site_key", "disabled.example.com", "Disabled Voting Site");

		assertEquals("site_key", manager.getVoteSiteName(false, "SITE_KEY"),
				"Configured keys should be matched case-insensitively");
		assertEquals("site_key", manager.getVoteSiteName(false, "DISABLED.EXAMPLE.COM"),
				"Disabled sites should still match their ServiceSite");
		assertEquals("site_key", manager.getVoteSiteName(false, "DISABLED VOTING SITE"),
				"Disabled sites should still match their display name");
		assertEquals("site_key", manager.getVoteSiteName(false, "site.key"),
				"Generated-key normalization should match dots to underscores");
	}

	@Test
	public void testDisabledConfiguredVoteSiteDoesNotResolveForEnabledOnlyLookup() {
		configureDisabledVoteSite("DisabledSite", "disabled.example.com", "Disabled Voting Site");

		assertEquals("disabled.example.com", manager.getVoteSiteName(true, "disabled.example.com"),
				"Enabled-only lookup must not return a disabled configured site");
		assertEquals("Disabled Voting Site", manager.getVoteSiteName(true, "Disabled Voting Site"),
				"Enabled-only display-name lookup must not return a disabled site");
	}

	@Test
	public void testDisabledConfiguredVoteSiteIsConfiguredButNotLoaded() {
		configureDisabledVoteSite("site_key", "disabled.example.com", "Disabled Voting Site");

		assertTrue(manager.hasConfiguredVoteSite("site_key"));
		assertTrue(manager.hasConfiguredVoteSite("SITE_KEY"));
		assertTrue(manager.hasConfiguredVoteSite("disabled.example.com"));
		assertTrue(manager.hasConfiguredVoteSite("Disabled Voting Site"));
		assertTrue(manager.hasConfiguredVoteSite("site.key"));

		assertFalse(manager.hasVoteSite("disabled.example.com"),
				"hasVoteSite must continue to describe the loaded vote-site list");
		assertFalse(manager.hasVoteSite("site_key"),
				"A configured disabled site must not appear loaded");
	}

	@Test
	public void testDisabledConfiguredVoteSiteIsNeverAutoCreated() {
		when(configFile.isAutoCreateVoteSites()).thenReturn(true);
		configureDisabledVoteSite("DisabledSite", "disabled.example.com", "Disabled Voting Site");

		assertNull(manager.getVoteSite("disabled.example.com", false),
				"A configured disabled site should remain unavailable");
		assertNull(manager.getVoteSite("disabled.example.com", true),
				"Enabled-only lookup should return no disabled VoteSite");

		verify(voteSitesConfig, never()).tryAutoGenerateVoteSite(anyString());
	}

	@Test
	public void testConfiguredVoteSiteCanMatchSecondIdentifier() {
		configureDisabledVoteSite("DisabledSite", "disabled.example.com", "Disabled Voting Site");

		assertEquals("DisabledSite",
				manager.getVoteSiteName(false, "unmatched-service.example.net", "disabled.example.com"),
				"The advanced ServiceSite fallback must be checked");
	}

	@Test
	public void testEmptyConfiguredAliasesDoNotMatchEmptyInput() {
		configureDisabledVoteSite("DisabledSite", "", "");

		assertFalse(manager.hasConfiguredVoteSite(""),
				"Empty ServiceSite and display-name values must not match");
		assertEquals("", manager.getVoteSiteName(false, ""),
				"Empty input should retain the existing fallback behavior");
	}

	@Test
	public void testNullConfiguredSiteInputIsSafe() {
		when(configFile.isAutoCreateVoteSites()).thenReturn(true);
		configureDisabledVoteSite("DisabledSite", "disabled.example.com", "Disabled Voting Site");

		assertNull(manager.getVoteSiteName(false, (String) null));
		assertFalse(manager.hasConfiguredVoteSite((String) null));
		assertFalse(manager.hasConfiguredVoteSite((String[]) null));
		assertFalse(manager.hasVoteSite(null));

		verify(voteSitesConfig, never()).tryAutoGenerateVoteSite(anyString());
	}

	@Test
	public void testUnknownSiteStillAutoCreatesWhenDisabledSitesAreConfigured() {
		when(configFile.isAutoCreateVoteSites()).thenReturn(true);
		when(voteSitesConfig.tryAutoGenerateVoteSite("new.example.com")).thenReturn(true);
		configureDisabledVoteSite("DisabledSite", "disabled.example.com", "Disabled Voting Site");

		VoteSite generated = manager.getVoteSite("new.example.com", false);

		assertNotNull(generated, "An unrelated unknown site should still be auto-created");
		assertEquals("new_example_com", generated.getKey());
		verify(voteSitesConfig).tryAutoGenerateVoteSite("new.example.com");
	}
}
