package com.bencodez.votingplugin.votereminding;

import java.io.File;
import java.io.IOException;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;

import com.bencodez.votingplugin.VotingPluginMain;

public final class VoteRemindersLegacyMigrator {

	private VoteRemindersLegacyMigrator() {
	}

	/**
	 * Migrates legacy VoteReminding -> VoteReminderOptions + VoteReminders in
	 * Config.yml.
	 *
	 * - NO BACKUPS (per request) - Runs once
	 * (VoteReminderOptions.MigratedFromLegacy) - Skips if new sections already
	 * exist
	 */
	public static boolean migrateIfNeeded(VotingPluginMain plugin, File configYmlFile, FileConfiguration cfg) {
		// already migrated
		if (cfg.getBoolean("VoteReminderOptions.MigratedFromLegacy", false)) {
			return false;
		}

		// if new system exists, don't touch anything
		if (cfg.isConfigurationSection("VoteReminderOptions") || cfg.isConfigurationSection("VoteReminders")) {
			return false;
		}

		ConfigurationSection legacy = cfg.getConfigurationSection("VoteReminding");
		if (legacy == null) {
			return false;
		}

		plugin.getLogger().info("[VoteReminders] Migrating legacy VoteReminding from Config.yml...");

		// Options
		cfg.set("VoteReminderOptions.Enabled", legacy.getBoolean("Enabled", true));
		cfg.set("VoteReminderOptions.StopAfterMatch", true);
		cfg.set("VoteReminderOptions.GlobalCooldown", "");
		cfg.set("VoteReminderOptions.DefaultPriority", 0);

		cfg.set("VoteReminderOptions.Defaults.Cooldown", "");
		cfg.set("VoteReminderOptions.Defaults.Delay", "");

		// Default rewards copy
		if (legacy.isConfigurationSection("Rewards")) {
			copySection(legacy.getConfigurationSection("Rewards"),
					ensureSection(cfg, "VoteReminderOptions.Defaults.Rewards"));
		}

		// RemindDelay -> Interval reminder
		int remindDelayMinutes = legacy.getInt("RemindDelay", 30);

		if (remindDelayMinutes > 0) {
			String base = "VoteReminders.Basic";
			cfg.set(base + ".Type", "INTERVAL");
			cfg.set(base + ".Interval", remindDelayMinutes + "m");
			cfg.set(base + ".Conditions.CanVoteAny", true);

		}

		// RemindOnLogin -> Login reminder
		if (legacy.getBoolean("RemindOnLogin", true)) {
			String base = "VoteReminders.OnLogin";
			cfg.set(base + ".Type", "LOGIN");
			cfg.set(base + ".Conditions.CanVoteAny", true);
		}

		// mark migrated
		cfg.set("VoteReminderOptions.MigratedFromLegacy", true);

		// save Config.yml
		try {
			cfg.save(configYmlFile);
		} catch (IOException e) {
			plugin.getLogger().severe("[VoteReminders] Failed saving Config.yml after migration: " + e.getMessage());
		}

		plugin.getLogger().info("[VoteReminders] Legacy VoteReminding migration complete.");
		return true;
	}

	private static ConfigurationSection ensureSection(FileConfiguration cfg, String path) {
		ConfigurationSection sec = cfg.getConfigurationSection(path);
		if (sec != null) {
			return sec;
		}
		String[] parts = path.split("\\.");
		String cur = "";
		ConfigurationSection out = null;
		for (String p : parts) {
			cur = cur.isEmpty() ? p : cur + "." + p;
			ConfigurationSection s = cfg.getConfigurationSection(cur);
			if (s == null) {
				s = cfg.createSection(cur);
			}
			out = s;
		}
		return out;
	}

	private static void copySection(ConfigurationSection from, ConfigurationSection to) {
		for (String key : from.getKeys(false)) {
			Object val = from.get(key);
			if (val instanceof ConfigurationSection) {
				copySection((ConfigurationSection) val, to.createSection(key));
			} else {
				to.set(key, val);
			}
		}
	}
}
