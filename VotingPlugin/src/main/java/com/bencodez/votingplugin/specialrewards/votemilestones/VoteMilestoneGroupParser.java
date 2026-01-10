package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.HashMap;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;

public final class VoteMilestoneGroupParser {

	private VoteMilestoneGroupParser() {
	}

	public static Map<String, VoteMilestoneGroupSelect> parseGroups(ConfigurationSection root) {
		HashMap<String, VoteMilestoneGroupSelect> out = new HashMap<>();

		if (root == null) {
			out.put("default", VoteMilestoneGroupSelect.ALL);
			return out;
		}

		ConfigurationSection opts = root.getConfigurationSection("VoteMilestonesOptions");
		if (opts == null) {
			out.put("default", VoteMilestoneGroupSelect.ALL);
			return out;
		}

		ConfigurationSection groups = opts.getConfigurationSection("Groups");
		if (groups == null) {
			out.put("default", VoteMilestoneGroupSelect.ALL);
			return out;
		}

		// Default mode for groups not explicitly listed
		VoteMilestoneGroupSelect defaultMode =
				parseSelect(groups.getString("Default", "ALL"), VoteMilestoneGroupSelect.ALL);

		out.put("default", defaultMode);

		// Parse other group mappings
		for (String key : groups.getKeys(false)) {
			if (key == null) {
				continue;
			}
			String groupId = key.trim();
			if (groupId.isEmpty()) {
				continue;
			}
			if (groupId.equalsIgnoreCase("Default")) {
				continue;
			}

			String raw = groups.getString(key, null);
			VoteMilestoneGroupSelect mode = parseSelect(raw, defaultMode);

			// If you want case-insensitive ids everywhere, normalize here:
			// groupId = groupId.toLowerCase();

			out.put(groupId, mode);
		}

		return out;
	}

	/**
	 * Parse selection mode with optional aliases. Falls back to fallback.
	 */
	private static VoteMilestoneGroupSelect parseSelect(String raw, VoteMilestoneGroupSelect fallback) {
		if (raw == null) {
			return fallback;
		}
		String v = raw.trim().toUpperCase();
		if (v.isEmpty()) {
			return fallback;
		}

		// Optional aliases
		switch (v) {
			case "ALL":
			case "ANY":
			case "EVERY":
			case "0":
				return VoteMilestoneGroupSelect.ALL;

			case "HIGHEST":
			case "MAX":
			case "TOP":
			case "2":
				return VoteMilestoneGroupSelect.HIGHEST;

			case "FIRST":
			case "ONE":
			case "EARLIEST":
			case "1":
				return VoteMilestoneGroupSelect.FIRST;

			default:
				break;
		}

		try {
			return VoteMilestoneGroupSelect.valueOf(v);
		} catch (Exception e) {
			return fallback;
		}
	}
}
