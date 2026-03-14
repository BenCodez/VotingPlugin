package com.bencodez.votingplugin.presets;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.ConfigVoteSites;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * Manager responsible for loading vote site presets defined in JSON and
 * applying them to the VotingPlugin configuration. Unlike the web preset
 * generator, this manager ignores YAML fragments and relies solely on values
 * defined in the meta JSON file.
 */
public class VoteSiteJsonPresetManager {

	/**
	 * The main plugin instance used to access the configuration.
	 */
	private final VotingPluginMain plugin;

	/**
	 * Cached reference to the vote sites configuration helper.
	 */
	private final ConfigVoteSites configVoteSites;

	/**
	 * Constructs a new manager for applying vote site presets.
	 *
	 * @param plugin the main plugin instance
	 */
	public VoteSiteJsonPresetManager(VotingPluginMain plugin) {
		this.plugin = Objects.requireNonNull(plugin, "plugin must not be null");
		this.configVoteSites = plugin.getConfigVoteSites();
	}

	/**
	 * Loads a vote site preset from the specified resource path.
	 *
	 * @param resourcePath the path to the meta JSON file within the JAR
	 * @return the deserialized preset
	 * @throws IOException if the resource cannot be read or parsed
	 */
	public VoteSitePreset loadPreset(String resourcePath) throws IOException {
		InputStream in = plugin.getResource(resourcePath);
		if (in == null) {
			throw new IOException("Preset resource not found: " + resourcePath);
		}
		try (Reader reader = new InputStreamReader(in, StandardCharsets.UTF_8)) {
			Gson gson = new GsonBuilder().create();
			return gson.fromJson(reader, VoteSitePreset.class);
		}
	}

	/**
	 * Applies the given vote site preset to the VotingPlugin configuration.
	 * Placeholder values will be populated using the defaults defined in the
	 * preset and overridden by any values provided in the {@code overrides} map.
	 *
	 * @param preset the preset to apply
	 * @param overrides a map of placeholder names to override values
	 */
	public void applyPreset(VoteSitePreset preset, Map<String, Object> overrides) {
		if (preset == null) {
			return;
		}

		Map<String, Object> values = new HashMap<String, Object>();

		if (preset.getPlaceholders() != null) {
			for (Map.Entry<String, PlaceholderDef> entry : preset.getPlaceholders().entrySet()) {
				PlaceholderDef def = entry.getValue();
				if ((def != null) && (def.getDefaultValue() != null)) {
					values.put(entry.getKey(), def.getDefaultValue());
				}
			}
		}

		if (overrides != null) {
			for (Map.Entry<String, Object> entry : overrides.entrySet()) {
				if (entry.getValue() != null) {
					values.put(entry.getKey(), entry.getValue());
				}
			}
		}

		String siteKey = getString(values.get("siteKey")).trim();
		String displayName = getString(values.get("displayName"));
		String serviceSite = getString(values.get("serviceSite"));
		String voteURL = getString(values.get("voteURL"));
		String voteDelay = getString(values.get("voteDelay"));

		Boolean waitUntilDelay = getBoolean(values.get("WaitUnitVoteDelay"));
		if (waitUntilDelay == null) {
			waitUntilDelay = getBoolean(values.get("waitUntilVoteDelay"));
		}

		Boolean voteDelayDaily = getBoolean(values.get("VoteDelayDaily"));
		if (voteDelayDaily == null) {
			voteDelayDaily = getBoolean(values.get("voteDelayDaily"));
		}

		Integer voteDelayDailyHour = getInteger(values.get("voteDelayDailyHour"));
		if (voteDelayDailyHour == null) {
			voteDelayDailyHour = getInteger(values.get("VoteDelayDailyHour"));
		}

		if (siteKey.isEmpty()) {
			plugin.getLogger().warning("Cannot apply preset: siteKey is missing");
			return;
		}

		configVoteSites.setEnabled(siteKey, true);

		if (!displayName.isEmpty()) {
			configVoteSites.setDisplayName(siteKey, displayName);
		}

		if (!serviceSite.isEmpty()) {
			configVoteSites.setServiceSite(siteKey, serviceSite);
		}

		if (!voteURL.isEmpty()) {
			configVoteSites.setVoteURL(siteKey, voteURL);
		}

		if (!voteDelay.isEmpty()) {
			configVoteSites.setVoteDelay(siteKey, voteDelay);
		}

		if (waitUntilDelay != null) {
			configVoteSites.set(siteKey, "WaitUntilVoteDelay", waitUntilDelay.booleanValue());
		}

		if (voteDelayDaily != null) {
			configVoteSites.setVoteDelayDaily(siteKey, voteDelayDaily.booleanValue());
		}

		if (voteDelayDailyHour != null) {
			configVoteSites.setVoteDelayDailyHour(siteKey, voteDelayDailyHour.intValue());
		}

		if (configVoteSites.getItem(siteKey) == null) {
			configVoteSites.set(siteKey, "DisplayItem.Material", "STONE");
			configVoteSites.set(siteKey, "DisplayItem.Amount", 1);
		}

		if (configVoteSites.getData(siteKey).getConfigurationSection("Rewards") == null) {
			configVoteSites.set(siteKey, "Rewards.Messages.Player", "&aThanks for voting on %ServiceSite%!");
		}

		configVoteSites.saveData();
		plugin.loadVoteSites();
	}

	/**
	 * Converts an object to a string.
	 *
	 * @param value the value
	 * @return the string form, or an empty string if null
	 */
	private String getString(Object value) {
		return value != null ? String.valueOf(value) : "";
	}

	/**
	 * Converts an object to a boolean if possible.
	 *
	 * @param value the value
	 * @return the parsed boolean, or null if not set
	 */
	private Boolean getBoolean(Object value) {
		if (value == null) {
			return null;
		}

		if (value instanceof Boolean) {
			return (Boolean) value;
		}

		String text = String.valueOf(value).trim().toLowerCase();

		if (text.isEmpty()) {
			return null;
		}

		if (text.equals("true") || text.equals("yes") || text.equals("1")) {
			return Boolean.TRUE;
		}

		if (text.equals("false") || text.equals("no") || text.equals("0")) {
			return Boolean.FALSE;
		}

		return null;
	}

	/**
	 * Converts an object to an integer if possible.
	 *
	 * @param value the value
	 * @return the parsed integer, or null if not set or invalid
	 */
	private Integer getInteger(Object value) {
		if (value == null) {
			return null;
		}

		if (value instanceof Number) {
			return Integer.valueOf(((Number) value).intValue());
		}

		String text = String.valueOf(value).trim();

		if (text.isEmpty()) {
			return null;
		}

		try {
			return Integer.valueOf(Integer.parseInt(text));
		} catch (NumberFormatException e) {
			plugin.getLogger().warning("Invalid integer value: " + text);
			return null;
		}
	}
}