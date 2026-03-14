package com.bencodez.votingplugin.tests.presets;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.bencodez.votingplugin.presets.PlaceholderDef;
import com.bencodez.votingplugin.presets.VoteSiteJsonPresetManager;
import com.bencodez.votingplugin.presets.VoteSitePreset;

/**
 * Tests for {@link VoteSiteJsonPresetManager}.
 */
public class VoteSiteJsonPresetManagerTest {

	private VotingPluginMain plugin;
	private ConfigVoteSites configVoteSites;
	private VoteSiteJsonPresetManager manager;
	private ConfigurationSection siteData;

	/**
	 * Sets up mocks.
	 */
	@BeforeEach
	public void setUp() {
		plugin = mock(VotingPluginMain.class);
		configVoteSites = mock(ConfigVoteSites.class);
		siteData = mock(ConfigurationSection.class);

		when(plugin.getConfigVoteSites()).thenReturn(configVoteSites);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("VotePresetTest"));
		when(configVoteSites.getData("crafty_gg")).thenReturn(siteData);
		when(configVoteSites.getItem("crafty_gg")).thenReturn(null);
		when(siteData.getConfigurationSection("Rewards")).thenReturn(null);

		manager = new VoteSiteJsonPresetManager(plugin);
	}

	/**
	 * Verifies preset defaults are applied.
	 */
	@Test
	public void testApplyPresetUsesDefaults() {
		VoteSitePreset preset = createPreset();

		manager.applyPreset(preset, new HashMap<String, Object>());

		verify(configVoteSites).setEnabled("crafty_gg", true);
		verify(configVoteSites).setDisplayName("crafty_gg", "crafty.gg");
		verify(configVoteSites).setServiceSite("crafty_gg", "crafty.gg");
		verify(configVoteSites).setVoteURL("crafty_gg", "VOTEURL");
		verify(configVoteSites).setVoteDelay("crafty_gg", "24h");
		verify(configVoteSites).saveData();
		verify(plugin).loadVoteSites();
	}

	/**
	 * Verifies overrides replace defaults.
	 */
	@Test
	public void testApplyPresetUsesOverrides() {
		VoteSitePreset preset = createPreset();

		Map<String, Object> overrides = new HashMap<String, Object>();
		overrides.put("displayName", "Custom Crafty");
		overrides.put("voteURL", "https://crafty.gg/servers/test/vote");
		overrides.put("voteDelay", "12h");

		manager.applyPreset(preset, overrides);

		verify(configVoteSites).setDisplayName("crafty_gg", "Custom Crafty");
		verify(configVoteSites).setVoteURL("crafty_gg", "https://crafty.gg/servers/test/vote");
		verify(configVoteSites).setVoteDelay("crafty_gg", "12h");
	}

	/**
	 * Verifies booleans and numeric values are applied when present.
	 */
	@Test
	public void testApplyPresetAppliesBooleanAndNumberFields() {
		VoteSitePreset preset = createPreset();

		Map<String, Object> overrides = new HashMap<String, Object>();
		overrides.put("waitUntilVoteDelay", "true");
		overrides.put("voteDelayDaily", "true");
		overrides.put("voteDelayDailyHour", "8");

		manager.applyPreset(preset, overrides);

		verify(configVoteSites).set("crafty_gg", "WaitUntilVoteDelay", true);
		verify(configVoteSites).setVoteDelayDaily("crafty_gg", true);
		verify(configVoteSites).setVoteDelayDailyHour("crafty_gg", 8);
	}

	/**
	 * Verifies no changes are applied when siteKey is missing.
	 */
	@Test
	public void testApplyPresetMissingSiteKey() {
		VoteSitePreset preset = createPreset();
		preset.getPlaceholders().remove("siteKey");

		manager.applyPreset(preset, new HashMap<String, Object>());

		verify(configVoteSites, never()).setEnabled("crafty_gg", true);
		verify(configVoteSites, never()).saveData();
		verify(plugin, never()).loadVoteSites();
	}

	/**
	 * Creates a standard preset for tests.
	 *
	 * @return preset
	 */
	private VoteSitePreset createPreset() {
		VoteSitePreset preset = new VoteSitePreset();

		Map<String, PlaceholderDef> placeholders = new HashMap<String, PlaceholderDef>();
		placeholders.put("siteKey", createPlaceholder("crafty_gg"));
		placeholders.put("displayName", createPlaceholder("crafty.gg"));
		placeholders.put("serviceSite", createPlaceholder("crafty.gg"));
		placeholders.put("voteURL", createPlaceholder("VOTEURL"));
		placeholders.put("voteDelay", createPlaceholder("24h"));
		placeholders.put("waitUntilVoteDelay", createPlaceholder(""));
		placeholders.put("voteDelayDaily", createPlaceholder(""));
		placeholders.put("voteDelayDailyHour", createPlaceholder(""));

		preset.setPlaceholders(placeholders);
		return preset;
	}

	/**
	 * Creates a placeholder definition with a default value.
	 *
	 * @param defaultValue default value
	 * @return placeholder
	 */
	private PlaceholderDef createPlaceholder(String defaultValue) {
		PlaceholderDef def = new PlaceholderDef();
		def.setDefaultValue(defaultValue);
		return def;
	}
}