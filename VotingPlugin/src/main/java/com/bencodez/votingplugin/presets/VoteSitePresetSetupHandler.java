package com.bencodez.votingplugin.presets;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.bukkit.entity.Player;

import com.bencodez.simpleapi.valuerequest.MultiValueField;
import com.bencodez.simpleapi.valuerequest.MultiValueField.FieldType;
import com.bencodez.simpleapi.valuerequest.MultiValueListener;
import com.bencodez.simpleapi.valuerequest.MultiValueResult;
import com.bencodez.simpleapi.valuerequest.StringListener;
import com.bencodez.simpleapi.valuerequest.ValueRequest;
import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Handler class that guides a player through selecting a vote site preset and
 * configuring its placeholder values using the SimpleAPI {@link ValueRequest}
 * system.
 */
public class VoteSitePresetSetupHandler {

	/**
	 * Main plugin instance.
	 */
	private final VotingPluginMain plugin;

	/**
	 * Loader used to fetch vote site presets.
	 */
	private final GitHubVoteSitePresetLoader loader;

	/**
	 * Creates a new setup handler.
	 *
	 * @param plugin plugin instance
	 */
	public VoteSitePresetSetupHandler(VotingPluginMain plugin) {
		this.plugin = Objects.requireNonNull(plugin, "plugin must not be null");
		this.loader = new GitHubVoteSitePresetLoader("BenCodez", "VotingPlugin-Presets", "main");
	}

	/**
	 * Starts the preset setup process.
	 *
	 * @param player player running setup
	 */
	public void startSetup(Player player) {
		List<VoteSitePreset> presets;

		try {
			presets = loader.listAllVoteSitePresets();
		} catch (Exception e) {
			player.sendMessage("§cUnable to load vote site presets.");
			plugin.getLogger().warning("Failed to load vote site presets: " + e.getMessage());
			return;
		}

		if (presets == null || presets.isEmpty()) {
			player.sendMessage("§cNo vote site presets are available.");
			return;
		}

		List<String> options = new ArrayList<String>();
		for (VoteSitePreset preset : presets) {
			options.add(preset.getId());
		}

		ValueRequest request = new ValueRequest(plugin, plugin.getDialogService());

		request.requestString(player, null, options, false, "Select a vote site preset", new StringListener() {
			@Override
			public void onInput(Player p, String value) {

				VoteSitePreset selected = null;

				for (VoteSitePreset preset : presets) {
					if (preset.getId() != null && preset.getId().equalsIgnoreCase(value)) {
						selected = preset;
						break;
					}
				}

				if (selected == null) {
					p.sendMessage("§cInvalid preset: " + value);
					return;
				}

				promptPlaceholders(p, selected);
			}
		});
	}

	/**
	 * Prompts placeholder values using the multi-value request system.
	 *
	 * @param player player
	 * @param preset preset
	 */
	private void promptPlaceholders(Player player, VoteSitePreset preset) {

		Map<String, PlaceholderDef> defs = preset.getPlaceholders();

		if (defs == null || defs.isEmpty()) {
			new VoteSiteJsonPresetManager(plugin).applyPreset(preset, new HashMap<String, Object>());
			player.sendMessage("§aPreset applied successfully.");
			return;
		}

		List<MultiValueField> fields = new ArrayList<MultiValueField>();
		List<String> keys = new ArrayList<String>();

		for (Map.Entry<String, PlaceholderDef> entry : defs.entrySet()) {

			String key = entry.getKey();

			if (key == null) {
				continue;
			}

			// skip auto values
			if (key.equalsIgnoreCase("siteKey") || key.equalsIgnoreCase("serviceSite")) {
				continue;
			}

			PlaceholderDef def = entry.getValue();

			String label = def != null && def.getLabel() != null ? def.getLabel() : key;
			String defaultValue = def != null && def.getDefaultValue() != null ? def.getDefaultValue() : "";

			MultiValueField field = createField(key, label, defaultValue);

			fields.add(field);
			keys.add(key);
		}

		if (fields.isEmpty()) {
			new VoteSiteJsonPresetManager(plugin).applyPreset(preset, new HashMap<String, Object>());
			player.sendMessage("§aPreset applied successfully.");
			return;
		}

		ValueRequest request = new ValueRequest(plugin, plugin.getDialogService());

		request.requestMultipleValues(player, fields, new MultiValueListener() {

			@Override
			public void onInput(Player p, MultiValueResult result) {

				Map<String, Object> overrides = new HashMap<String, Object>();

				for (String key : keys) {

					if (isBooleanField(key)) {
						Boolean value = result.getBoolean(key);
						overrides.put(key, value != null ? value : Boolean.FALSE);
					} else {
						String value = result.getString(key);
						overrides.put(key, value != null ? value : "");
					}
				}

				new VoteSiteJsonPresetManager(plugin).applyPreset(preset, overrides);

				p.sendMessage("§aVote site configured and preset applied.");
			}
		});
	}

	/**
	 * Creates a field based on placeholder key.
	 *
	 * @param key key
	 * @param label label
	 * @param defaultValue default value
	 * @return field
	 */
	private MultiValueField createField(String key, String label, String defaultValue) {

		if (isBooleanField(key)) {

			boolean boolValue = Boolean.parseBoolean(defaultValue);

			return new MultiValueField(key, label + " (default: " + boolValue + ")", FieldType.BOOLEAN)
					.booleanValue(Boolean.valueOf(boolValue))
					.required(false);
		}

		return new MultiValueField(key, label + " (default: " + defaultValue + ")", FieldType.STRING)
				.stringValue(defaultValue)
				.required(true);
	}

	/**
	 * Determines if the placeholder should be treated as a boolean.
	 *
	 * @param key placeholder key
	 * @return true if boolean field
	 */
	private boolean isBooleanField(String key) {
		return key.equalsIgnoreCase("WaitUntilVoteDelay")
				|| key.equalsIgnoreCase("VoteDelayDaily");
	}
}