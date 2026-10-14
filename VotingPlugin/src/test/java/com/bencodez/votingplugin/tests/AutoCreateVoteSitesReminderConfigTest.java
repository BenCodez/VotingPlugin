package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService.Settings;

class AutoCreateVoteSitesReminderConfigTest {

	@Test
	void bundledConfigHasOnlyExistingAutoCreateSetting() throws Exception {
		var resource = VotingPluginMain.class.getClassLoader().getResourceAsStream("Config.yml");
		assertNotNull(resource);
		try (var reader = new InputStreamReader(resource, StandardCharsets.UTF_8)) {
			YamlConfiguration config = YamlConfiguration.loadConfiguration(reader);
			assertTrue(config.getBoolean("AutoCreateVoteSites"));
			assertFalse(config.contains("AutoCreateVoteSitesReminder"));
		}
	}

	@Test
	void enabledAutoCreationGetsFixedWeeklyNagWithoutExtraSettings() throws Exception {
		Config config = mock(Config.class);
		when(config.isAutoCreateVoteSites()).thenReturn(true);
		Settings settings = settings(config);
		assertTrue(settings.enabled());
		assertTrue(settings.autoCreateEnabled());
		assertEquals(TimeUnit.DAYS.toMillis(7), settings.afterMillis());
		assertEquals(TimeUnit.DAYS.toMillis(7), settings.repeatMillis());
		verify(config, never()).getData();
	}

	@Test
	void disablingExistingAutoCreateSettingStopsNag() throws Exception {
		Config config = mock(Config.class);
		when(config.isAutoCreateVoteSites()).thenReturn(false);
		assertFalse(settings(config).autoCreateEnabled());
		verify(config, never()).getData();
	}

	private static Settings settings(Config config) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		Field field = VotingPluginMain.class.getDeclaredField("configFile");
		field.setAccessible(true);
		field.set(plugin, config);
		Method method = VotingPluginMain.class.getDeclaredMethod("autoCreateVoteSitesReminderSettings");
		method.setAccessible(true);
		return (Settings) method.invoke(plugin);
	}
}
