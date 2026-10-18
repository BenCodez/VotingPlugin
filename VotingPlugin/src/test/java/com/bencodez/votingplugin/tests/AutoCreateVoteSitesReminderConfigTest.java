package com.bencodez.votingplugin.tests;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

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
	void enabledAutoCreationGetsStartupAndTwoDayNagWithoutExtraSettings() throws Exception {
		Config config = mock(Config.class);
		when(config.isAutoCreateVoteSites()).thenReturn(true);
		Settings settings = settings(config);
		assertTrue(settings.enabled());
		assertTrue(settings.autoCreateEnabled());
		assertEquals(0, settings.afterMillis());
		assertEquals(TimeUnit.HOURS.toMillis(48), settings.repeatMillis());
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
		ReflectiveAccess.set(field, plugin, config);
		Method method = VotingPluginMain.class.getDeclaredMethod("autoCreateVoteSitesReminderSettings");
		method.setAccessible(true);
		return (Settings) ReflectiveAccess.invoke(method, plugin, new Object[]{});
	}
}
