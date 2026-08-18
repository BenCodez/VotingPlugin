package com.bencodez.votingplugin.tests.votesite;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.ArrayList;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.config.ConfigVoteSites;

/**
 * Tests the unfiltered configured vote-site section lookup.
 */
public class ConfigVoteSitesRawNamesTest {

	/**
	 * Creates a partially real ConfigVoteSites mock whose data comes from the
	 * supplied in-memory YAML configuration.
	 *
	 * @param data in-memory configuration data
	 * @return the configuration object
	 */
	private ConfigVoteSites configWithData(YamlConfiguration data) {
		ConfigVoteSites config = mock(ConfigVoteSites.class, CALLS_REAL_METHODS);
		doReturn(data).when(config).getData();
		return config;
	}

	@Test
	public void testRawNamesIncludesEnabledAndDisabledSections() {
		YamlConfiguration data = new YamlConfiguration();

		data.createSection("VoteSites.EnabledSite");
		data.set("VoteSites.EnabledSite.Enabled", true);
		data.set("VoteSites.EnabledSite.ServiceSite", "enabled.example.com");

		data.createSection("VoteSites.DisabledSite");
		data.set("VoteSites.DisabledSite.Enabled", false);
		data.set("VoteSites.DisabledSite.ServiceSite", "disabled.example.com");

		ArrayList<String> names = configWithData(data).getRawVoteSiteNames();

		assertEquals(2, names.size());
		assertTrue(names.contains("EnabledSite"));
		assertTrue(names.contains("DisabledSite"),
				"Disabled sections must remain visible to existence checks");
	}

	@Test
	public void testRawNamesIgnoresMalformedScalarChildren() {
		YamlConfiguration data = new YamlConfiguration();

		data.createSection("VoteSites.RealSite");
		data.set("VoteSites.RealSite.Enabled", false);
		data.set("VoteSites.MalformedSite", "this is a scalar and not a vote-site section");

		ArrayList<String> names = configWithData(data).getRawVoteSiteNames();

		assertEquals(1, names.size());
		assertEquals("RealSite", names.get(0));
	}

	@Test
	public void testRawNamesReturnsEmptyWhenVoteSitesSectionIsMissing() {
		YamlConfiguration data = new YamlConfiguration();

		assertTrue(configWithData(data).getRawVoteSiteNames().isEmpty());
	}

	@Test
	public void testRawNamesReturnsEmptyWhenVoteSitesValueIsMalformed() {
		YamlConfiguration data = new YamlConfiguration();
		data.set("VoteSites", "not a configuration section");

		assertTrue(configWithData(data).getRawVoteSiteNames().isEmpty());
	}
}
