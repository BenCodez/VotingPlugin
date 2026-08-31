package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.bencodez.votingplugin.votesites.VoteSiteRegistry;
import com.bencodez.votingplugin.votesites.VoteSiteResolver;
import com.bencodez.votingplugin.votesites.VoteSiteValidator;

public class VoteSiteResolverTest {

	private ConfigVoteSites config;
	private VoteSiteRegistry registry;
	private VoteSiteResolver resolver;

	@BeforeEach
	public void setUp() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		config = mock(ConfigVoteSites.class);
		when(plugin.getConfigVoteSites()).thenReturn(config);

		registry = new VoteSiteRegistry();
		resolver = new VoteSiteResolver(plugin, registry, new VoteSiteValidator(plugin));
	}

	@Test
	public void testResolveLoadedSiteByServiceSite() {
		VoteSite site = mock(VoteSite.class);
		when(site.getKey()).thenReturn("TopSite");
		when(site.getDisplayName()).thenReturn("Top Site");
		when(site.getServiceSite()).thenReturn("minecraftservers.org");
		when(site.isEnabled()).thenReturn(true);
		registry.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertSame(site, resolver.resolveVoteSite("minecraftservers.org", true));
	}

	@Test
	public void testResolveDisabledLoadedSiteHonorsCheckEnabled() {
		VoteSite site = mock(VoteSite.class);
		when(site.getKey()).thenReturn("DisabledSite");
		when(site.getDisplayName()).thenReturn("Disabled Site");
		when(site.isEnabled()).thenReturn(false);
		registry.setVoteSites(Collections.synchronizedList(new ArrayList<VoteSite>(Arrays.asList(site))));

		assertNull(resolver.resolveVoteSite("DisabledSite", true));
		assertNull(resolver.resolveVoteSite("Disabled Site", true));
		assertSame(site, resolver.resolveVoteSite("DisabledSite", false));
	}

	@Test
	public void testConfiguredDisabledSiteCanResolveNameWithoutCreating() {
		when(config.getRawVoteSiteNames()).thenReturn(new ArrayList<String>(Arrays.asList("DisabledSite")));
		when(config.getServiceSite("DisabledSite")).thenReturn("disabled.example.com");
		when(config.getDisplayName("DisabledSite")).thenReturn("Disabled Site");

		assertEquals("DisabledSite", resolver.getVoteSiteName(false, "disabled.example.com"));
		verify(config, never()).tryGenerateVoteSite(anyString());
		verify(config, never()).tryAutoGenerateVoteSite(anyString());
	}

	@Test
	public void testResolveMissingSiteNeverGeneratesConfig() {
		assertNull(resolver.resolveVoteSite("new.site", false));
		verify(config, never()).tryGenerateVoteSite(anyString());
		verify(config, never()).tryAutoGenerateVoteSite(anyString());
	}
}
