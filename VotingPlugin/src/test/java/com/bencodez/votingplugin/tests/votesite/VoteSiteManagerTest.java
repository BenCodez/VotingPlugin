package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
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

		when(plugin.getConfigVoteSites()).thenReturn(voteSitesConfig);
		when(plugin.getConfigFile()).thenReturn(configFile);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("VoteSiteManagerTest"));

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

		ServerData dummyServerData = mock(ServerData.class);
		when(dummyServerData.getServiceSites()).thenReturn(new ArrayList<String>(Arrays.asList("ServiceSite")));
		when(plugin.getServerData()).thenReturn(dummyServerData);

		manager = new VoteSiteManager(plugin);
	}

	/**
	 * Creates a mocked vote site with the given values.
	 *
	 * @param key the key
	 * @param displayName the display name
	 * @param serviceSite the service site
	 * @param enabled the enabled state
	 * @return the mocked vote site
	 */
	private VoteSite createMockVoteSite(String key, String displayName, String serviceSite, boolean enabled) {
		VoteSite site = mock(VoteSite.class);
		when(site.getKey()).thenReturn(key);
		when(site.getDisplayName()).thenReturn(displayName);
		when(site.getServiceSite()).thenReturn(serviceSite);
		when(site.isEnabled()).thenReturn(enabled);
		return site;
	}

	@Test
	public void testGetVoteSiteNameMatchesConfiguredServiceSiteUrl() {
		VoteSite site = createMockVoteSite("TopSite", "TopSite", "minecraftservers.org", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertEquals("TopSite", manager.getVoteSiteName(true, "minecraftservers.org"));
	}

	@Test
	public void testGetVoteSiteNameMatchesSiteNameIgnoringCase() {
		VoteSite site = createMockVoteSite("PlanetMinecraft", "Planet Minecraft", "planetminecraft.com", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertEquals("PlanetMinecraft", manager.getVoteSiteName(true, "planetminecraft"));
	}

	@Test
	public void testGetVoteSiteNameReturnsNullWhenInputNull() {
		VoteSite site = createMockVoteSite("SiteA", "Site A", "example.com", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertNull(manager.getVoteSiteName(true, (String) null));
	}

	@Test
	public void testGetVoteSiteNameFallsBackToFirstProvidedValueWhenNoMatch() {
		VoteSite site = createMockVoteSite("SiteA", "Site A", "example.com", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertEquals("unknown-site", manager.getVoteSiteName(true, "unknown-site"));
	}

	@Test
	public void testGetVoteSiteReturnsExistingByKey() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("DisplayName");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertSame(site, manager.getVoteSite("site_test", false));
	}

	@Test
	public void testGetVoteSiteReturnsExistingByDisplayName() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("My Fancy Name");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertSame(site, manager.getVoteSite("My Fancy Name", false));
	}

	@Test
	public void testGetVoteSiteAutoCreatesWhenEnabledAndNotPresentInConfig() {
		when(configFile.isAutoCreateVoteSites()).thenReturn(true);

		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>()));

		VoteSite created = manager.getVoteSite("new.site", false);
		assertNotNull(created, "Should auto-create VoteSite when enabled and not configured");

		assertEquals("new_site", created.getKey());
		verify(voteSitesConfig).generateVoteSite("new.site");
	}

	@Test
	public void testGetVoteSitesEnabledFiltersOnlyEnabled() {
		VoteSite a = mock(VoteSite.class);
		VoteSite b = mock(VoteSite.class);
		when(a.isEnabled()).thenReturn(true);
		when(b.isEnabled()).thenReturn(false);

		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(a, b))));

		assertEquals(1, manager.getVoteSitesEnabled().size());
		assertSame(a, manager.getVoteSitesEnabled().get(0));
	}

	@Test
	public void testGetVoteSiteServiceSiteReturnsMappedUrl() {
		VoteSite site = createMockVoteSite("MCSL", "MCSL", "minecraft-server-list.com", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertEquals("minecraft-server-list.com", manager.getVoteSiteServiceSite("MCSL"));
		assertEquals("minecraft-server-list.com", manager.getVoteSiteServiceSite("minecraft-server-list.com"));
	}

	@Test
	public void testGetVoteSiteServiceSiteReturnsInputWhenNoMapping() {
		VoteSite site = createMockVoteSite("MCSL", "MCSL", "minecraft-server-list.com", true);
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertEquals("unknown.com", manager.getVoteSiteServiceSite("unknown.com"));
	}

	@Test
	public void testHasVoteSiteTrueWhenPresent() {
		VoteSite site = new VoteSite(plugin, "site.test");
		site.setDisplayName("DisplayName");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertTrue(manager.hasVoteSite("site_test"));
	}

	@Test
	public void testIsVoteSiteTrueWhenKeyPresent() {
		VoteSite site = new VoteSite(plugin, "site.test");
		manager.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertTrue(manager.isVoteSite("site_test"));
		assertFalse(manager.isVoteSite("missing"));
	}

	@Test
	public void testLoadVoteSitesReplacesBackingList() {
		VoteSite s1 = mock(VoteSite.class);
		when(s1.getKey()).thenReturn("TopSite");
		when(voteSitesConfig.getVoteSitesLoad()).thenReturn(new ArrayList<VoteSite>(Arrays.asList(s1)));

		manager.loadVoteSites();

		assertEquals(1, manager.getVoteSites().size());
		assertSame(s1, manager.getVoteSites().get(0));
		verify(voteSitesConfig).setup();
	}
}