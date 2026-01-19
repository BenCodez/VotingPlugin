package com.bencodez.votingplugin.tests.reminders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Files;
import java.util.logging.Logger;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.votereminding.VoteRemindersLegacyMigrator;

public class VoteRemindersLegacyMigratorTest {

	@TempDir
	File tempDir;

	@Test
	public void migrateIfNeeded_returnsFalse_whenAlreadyMigrated() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("test"));

		File cfgFile = new File(tempDir, "config.yml");
		FileConfiguration cfg = new YamlConfiguration();
		cfg.set("VoteReminderOptions.MigratedFromLegacy", true);

		assertFalse(VoteRemindersLegacyMigrator.migrateIfNeeded(plugin, cfgFile, cfg));
	}

	@Test
	public void migrateIfNeeded_returnsFalse_whenNewSectionsExist() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("test"));

		File cfgFile = new File(tempDir, "config.yml");
		FileConfiguration cfg = new YamlConfiguration();
		cfg.createSection("VoteReminderOptions"); // new system exists

		assertFalse(VoteRemindersLegacyMigrator.migrateIfNeeded(plugin, cfgFile, cfg));
	}

	@Test
	public void migrateIfNeeded_returnsFalse_whenNoLegacySection() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("test"));

		File cfgFile = new File(tempDir, "config.yml");
		FileConfiguration cfg = new YamlConfiguration();

		assertFalse(VoteRemindersLegacyMigrator.migrateIfNeeded(plugin, cfgFile, cfg));
	}

	@Test
	public void migrateIfNeeded_migratesLegacyOptions_andSavesFile() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("test"));

		File cfgFile = new File(tempDir, "config.yml");
		FileConfiguration cfg = new YamlConfiguration();

		// Legacy section: VoteReminding
		cfg.set("VoteReminding.Enabled", false);
		cfg.set("VoteReminding.RemindDelay", 15);
		cfg.set("VoteReminding.RemindOnLogin", true);

		// Legacy rewards to be copied into VoteReminderOptions.Defaults.Rewards
		cfg.set("VoteReminding.Rewards.Messages.0", "Hello");
		cfg.set("VoteReminding.Rewards.Commands.0", "say test");

		boolean migrated = VoteRemindersLegacyMigrator.migrateIfNeeded(plugin, cfgFile, cfg);
		assertTrue(migrated);

		// Options copied/created
		assertEquals(false, cfg.getBoolean("VoteReminderOptions.Enabled"));
		assertEquals(true, cfg.getBoolean("VoteReminderOptions.StopAfterMatch"));
		assertEquals(true, cfg.getBoolean("VoteReminderOptions.MigratedFromLegacy"));

		// Default rewards copied
		assertEquals("Hello", cfg.getString("VoteReminderOptions.Defaults.Rewards.Messages.0"));
		assertEquals("say test", cfg.getString("VoteReminderOptions.Defaults.Rewards.Commands.0"));

		// RemindDelay -> INTERVAL reminder "Basic"
		assertEquals("INTERVAL", cfg.getString("VoteReminders.Basic.Type"));
		assertEquals("15m", cfg.getString("VoteReminders.Basic.Interval"));
		assertEquals(true, cfg.getBoolean("VoteReminders.Basic.Conditions.CanVoteAny"));

		// RemindOnLogin -> LOGIN reminder "OnLogin"
		assertEquals("LOGIN", cfg.getString("VoteReminders.OnLogin.Type"));
		assertEquals(true, cfg.getBoolean("VoteReminders.OnLogin.Conditions.CanVoteAny"));

		// Verify file saved (migrator calls cfg.save(file))
		assertTrue(cfgFile.exists(), "Expected config.yml to be created by save()");
		assertTrue(Files.size(cfgFile.toPath()) > 0, "Expected saved config.yml to be non-empty");
	}

	@Test
	public void migrateIfNeeded_doesNotCreateIntervalReminder_whenDelayZeroOrNegative() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		when(plugin.getLogger()).thenReturn(Logger.getLogger("test"));

		File cfgFile = new File(tempDir, "config.yml");
		FileConfiguration cfg = new YamlConfiguration();

		cfg.set("VoteReminding.Enabled", true);
		cfg.set("VoteReminding.RemindDelay", 0);
		cfg.set("VoteReminding.RemindOnLogin", false);

		assertTrue(VoteRemindersLegacyMigrator.migrateIfNeeded(plugin, cfgFile, cfg));

		assertFalse(cfg.isConfigurationSection("VoteReminders.Basic"));
		assertFalse(cfg.isConfigurationSection("VoteReminders.OnLogin"));
	}
}
