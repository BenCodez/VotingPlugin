package com.bencodez.votingplugin.presets;

import java.io.IOException;
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
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

import lombok.Getter;

/**
 * Handler class that guides a player through selecting a vote site preset and
 * configuring its placeholder values using the SimpleAPI {@link ValueRequest}
 * system.
 *
 * CommandHandler executes command callbacks asynchronously. Network/cache work is
 * intentionally kept on that command worker; Bukkit/Folia player UI work is
 * scheduled back to the player's owner.
 */
public class VoteSitePresetSetupHandler {

	private final VotingPluginMain plugin;

	@Getter
	private final GitHubVoteSitePresetLoader loader;

	public VoteSitePresetSetupHandler(VotingPluginMain plugin) {
		this(plugin, new GitHubVoteSitePresetLoader("BenCodez", "VotingPlugin-Presets", "main"));
	}

	VoteSitePresetSetupHandler(VotingPluginMain plugin, GitHubVoteSitePresetLoader loader) {
		this.plugin = Objects.requireNonNull(plugin, "plugin must not be null");
		this.loader = Objects.requireNonNull(loader, "loader must not be null");
	}

	/** Called from the asynchronous CommandHandler execution lane. */
	public void startSetup(Player player) {
		final List<VoteSitePreset> presets;
		try {
			presets = loader.listAllVoteSitePresets();
		} catch (InterruptedException failure) {
			Thread.currentThread().interrupt();
			runForPlayer(player, () -> player.sendMessage("§cUnable to load vote site presets."));
			return;
		} catch (IOException | RuntimeException failure) {
			plugin.getLogger().warning("Failed to load vote site presets: " + failure.getMessage());
			runForPlayer(player, () -> player.sendMessage("§cUnable to load vote site presets."));
			return;
		}

		runForPlayer(player, () -> {
			if (presets.isEmpty()) {
				player.sendMessage("§cNo vote site presets are available.");
				return;
			}
			openPresetSelection(player, presets);
		});
	}

	/** Called from the asynchronous CommandHandler execution lane. */
	public void findPresetForURL(Player player, String voteURL) {
		final VoteSitePreset preset;
		try {
			preset = loader.findVoteSitePresetForURL(voteURL);
		} catch (InterruptedException failure) {
			Thread.currentThread().interrupt();
			runForPlayer(player, () -> player.sendMessage("Could not determine a preset for that URL."));
			return;
		} catch (IOException | RuntimeException failure) {
			plugin.getLogger().warning("Failed to search presets: " + failure.getMessage());
			runForPlayer(player, () -> player.sendMessage("Could not determine a preset for that URL."));
			return;
		}

		runForPlayer(player, () -> {
			if (preset == null) {
				player.sendMessage("No vote preset matches that URL.");
				return;
			}
			promptPlaceholders(player, preset);
		});
	}

	private void runForPlayer(Player player, Runnable task) {
		BukkitCompletionScheduler.run(plugin, player, task, () -> { },
				() -> plugin.getLogger().warning("Unable to schedule vote preset UI"));
	}

	private void openPresetSelection(Player player, List<VoteSitePreset> presets) {
		List<String> options = new ArrayList<String>();
		for (VoteSitePreset preset : presets) {
			if (preset != null && preset.getId() != null) options.add(preset.getId());
		}

		if (options.isEmpty()) {
			player.sendMessage("§cNo vote site presets are available.");
			return;
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

	public void promptPlaceholders(Player player, VoteSitePreset preset) {
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
			if (key == null) continue;
			if (key.equalsIgnoreCase("siteKey") || key.equalsIgnoreCase("serviceSite")) continue;

			PlaceholderDef def = entry.getValue();
			String label = def != null && def.getLabel() != null ? def.getLabel() : key;
			String defaultValue = def != null && def.getDefaultValue() != null ? def.getDefaultValue() : "";
			fields.add(createField(key, label, defaultValue));
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

	private boolean isBooleanField(String key) {
		return key.equalsIgnoreCase("WaitUntilVoteDelay") || key.equalsIgnoreCase("VoteDelayDaily");
	}
}
