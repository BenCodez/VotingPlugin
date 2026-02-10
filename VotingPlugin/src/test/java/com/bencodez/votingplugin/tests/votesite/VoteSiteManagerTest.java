package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.TimeUnit;

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
 * Unit tests for {@link VoteSiteManager}.
 */
public class VoteSiteManagerTest {

	private VotingPluginMain plugin;
	private ConfigVoteSites voteSitesConfig;
	private Config configFile;
	private VoteSiteManager manager;

	@BeforeEach
	public void setUp() {
		plugin = mock(VotingPluginMain.class);
		voteSitesConfig = mock(ConfigVoteSites.class);
		configFile = mock(Config.class);

		// Common plugin wiring
		when(plugin.getConfigVoteSites()).thenReturn(voteSitesConfig);
		when(plugin.getConfigFile()).thenReturn(configFile);

		// Used by VoteSite constructor init() if auto-create path returns a VoteSite
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

		// ServiceSites list used by VoteSite.isVaidServiceSite()
		ServerData dummyServerData = mock(ServerData.class);
		when(dummyServerData.getServiceSites()).thenReturn(new ArrayList<>(Arrays.asList("ServiceSite")));
		when(plugin.getServerData()).thenReturn(dummyServerData);

		manager = new VoteSiteManager(plugin);
	}

	@Test
	public void testGetVoteSiteNameMatchesConfiguredServiceSiteUrl() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("TopSite")));
		when(voteSitesConfig.getServiceSite("TopSite")).thenReturn("minecraftservers.org");

		assertEquals("TopSite", manager.getVoteSiteName(true, "minecraftservers.org"));
	}

	@Test
	public void testGetVoteSiteNameMatchesSiteNameIgnoringCase() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("PlanetMinecraft")));
		when(voteSitesConfig.getServiceSite("PlanetMinecraft")).thenReturn("planetminecraft.com");

		assertEquals("PlanetMinecraft", manager.getVoteSiteName(true, "planetminecraft"));
	}

	@Test
	public void testGetVoteSiteNameReturnsNullWhenInputNull() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("SiteA")));
		assertNull(manager.getVoteSiteName(true, (String) null));
	}

	@Test
	public void testGetVoteSiteNameFallsBackToFirstProvidedValueWhenNoMatch() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("SiteA")));
		when(voteSitesConfig.getServiceSite("SiteA")).thenReturn("example.com");

		assertEquals("unknown-site", manager.getVoteSiteName(true, "unknown-site"));
	}

	@Test
	public void testGetVoteSiteReturnsExistingByKey() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("DisplayName");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<>(Arrays.asList(site))));

		// site.getKey() = "site_test"
		assertSame(site, manager.getVoteSite("site_test", false));
	}

	@Test
	public void testGetVoteSiteReturnsExistingByDisplayName() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("My Fancy Name");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<>(Arrays.asList(site))));

		assertSame(site, manager.getVoteSite("My Fancy Name", false));
	}

	@Test
	public void testGetVoteSiteAutoCreatesWhenEnabledAndNotPresentInConfig() {
		when(configFile.isAutoCreateVoteSites()).thenReturn(true);
		when(voteSitesConfig.getVoteSitesNames(false)).thenReturn(new ArrayList<String>());

		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>()));

		VoteSite created = manager.getVoteSite("new.site", false);
		assertNotNull(created, "Should auto-create VoteSite when enabled and not configured");

		// VoteSite constructor replaces '.' with '_' in key
		assertEquals("new_site", created.getKey());
		verify(voteSitesConfig).generateVoteSite("new.site");
	}

	@Test
	public void testGetVoteSitesEnabledFiltersOnlyEnabled() {
		VoteSite a = mock(VoteSite.class);
		VoteSite b = mock(VoteSite.class);
		when(a.isEnabled()).thenReturn(true);
		when(b.isEnabled()).thenReturn(false);

		manager.setVoteSites(Collections.synchronizedList(new ArrayList<>(Arrays.asList(a, b))));
		assertEquals(1, manager.getVoteSitesEnabled().size());
		assertSame(a, manager.getVoteSitesEnabled().get(0));
	}

	@Test
	public void testGetVoteSiteServiceSiteReturnsMappedUrl() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("MCSL")));
		when(voteSitesConfig.getServiceSite("MCSL")).thenReturn("minecraft-server-list.com");

		assertEquals("minecraft-server-list.com", manager.getVoteSiteServiceSite("MCSL"));
		assertEquals("minecraft-server-list.com", manager.getVoteSiteServiceSite("minecraft-server-list.com"));
	}

	@Test
	public void testGetVoteSiteServiceSiteReturnsInputWhenNoMapping() {
		when(voteSitesConfig.getVoteSitesNames(true)).thenReturn(new ArrayList<>(Arrays.asList("MCSL")));
		when(voteSitesConfig.getServiceSite("MCSL")).thenReturn("minecraft-server-list.com");

		assertEquals("unknown.com", manager.getVoteSiteServiceSite("unknown.com"));
	}

	@Test
	public void testHasVoteSiteTrueWhenPresent() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("DisplayName");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<>(Arrays.asList(site))));

		assertTrue(manager.hasVoteSite("site_test"));
	}

	@Test
	public void testIsVoteSiteTrueWhenKeyPresent() {
		VoteSite site = new VoteSite(plugin, "site.test");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<>(Arrays.asList(site))));

		assertTrue(manager.isVoteSite("site_test"));
		assertFalse(manager.isVoteSite("missing"));
	}

	@Test
	public void testLoadVoteSitesReplacesBackingList() {
		VoteSite s1 = mock(VoteSite.class);
		when(voteSitesConfig.getVoteSitesLoad()).thenReturn(new ArrayList<>(Arrays.asList(s1)));

		manager.loadVoteSites();

		assertEquals(1, manager.getVoteSites().size());
		assertSame(s1, manager.getVoteSites().get(0));
		verify(voteSitesConfig).setup();
	}
}
