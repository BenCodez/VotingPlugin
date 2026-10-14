package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.List;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.config.Config;

class AutoCreateVoteSitesReminderConfigTest {

	private static final String ROOT = "AutoCreateVoteSitesReminder";
	private static final String ENABLED = ROOT + ".Enabled";
	private static final String AFTER = ROOT + ".AfterDays";
	private static final String REPEAT = ROOT + ".RepeatDays";

	@Test
	void absentKeysHaveSafeDefaultsThroughActualAnnotationBinder() {
		Config config = config(new YamlConfiguration());
		config.loadValues();
		assertTrue(config.isAutoCreateVoteSitesReminderEnabled());
		assertEquals(7, config.getAutoCreateVoteSitesReminderAfterDays());
		assertEquals(7, config.getAutoCreateVoteSitesReminderRepeatDays());
	}

	@Test
	void removingFalseAndCustomDaysRestoresDefaultsOnSameConfigInstance() {
		YamlConfiguration data = new YamlConfiguration();
		data.set(ENABLED, false);
		data.set(AFTER, 42);
		data.set(REPEAT, 13);
		Config config = config(data);
		config.loadValues();
		assertFalse(config.isAutoCreateVoteSitesReminderEnabled());
		assertEquals(42, config.getAutoCreateVoteSitesReminderAfterDays());
		assertEquals(13, config.getAutoCreateVoteSitesReminderRepeatDays());
		data.set(ROOT, null);
		config.loadValues();
		assertTrue(config.isAutoCreateVoteSitesReminderEnabled());
		assertEquals(7, config.getAutoCreateVoteSitesReminderAfterDays());
		assertEquals(7, config.getAutoCreateVoteSitesReminderRepeatDays());
	}

	@Test
	void removingInvalidValuesRestoresDefaultsWithoutRetainingEarlierValues() {
		YamlConfiguration data = new YamlConfiguration();
		data.set(ENABLED, false);
		data.set(AFTER, 21);
		data.set(REPEAT, 14);
		Config config = config(data);
		config.loadValues();
		assertEquals(21, config.getAutoCreateVoteSitesReminderAfterDays());
		assertEquals(14, config.getAutoCreateVoteSitesReminderRepeatDays());
		data.set(AFTER, 1.5D);
		data.set(REPEAT, 4_294_967_297L);
		config.loadValues();
		assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderAfterDays);
		assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderRepeatDays);
		data.set(ENABLED, null);
		data.set(AFTER, null);
		data.set(REPEAT, null);
		config.loadValues();
		assertTrue(config.isAutoCreateVoteSitesReminderEnabled());
		assertEquals(7, config.getAutoCreateVoteSitesReminderAfterDays());
		assertEquals(7, config.getAutoCreateVoteSitesReminderRepeatDays());
	}

	@Test
	void positiveIntegerAndLongBoundariesAreAcceptedWithoutCoercion() {
		YamlConfiguration data = new YamlConfiguration();
		Config config = config(data);
		for (Number value : new Number[] { 1, 7, 106751, 1L, 7L, 106751L }) {
			data.set(AFTER, value);
			data.set(REPEAT, value);
			config.loadValues();
			assertEquals(value.intValue(), config.getAutoCreateVoteSitesReminderAfterDays());
			assertEquals(value.intValue(), config.getAutoCreateVoteSitesReminderRepeatDays());
		}
	}

	@Test
	void fractionsOverflowStringsAndOutOfRangeValuesAreRejectedAtEachGetter() {
		YamlConfiguration data = new YamlConfiguration();
		Config config = config(data);
		Object[] invalid = { 1.5D, 1.0D, 4_294_967_297L, "7", "invalid", 0, -1, 106752,
				Long.MAX_VALUE, true, List.of(7) };
		for (Object value : invalid) {
			data.set(AFTER, value);
			data.set(REPEAT, value);
			config.loadValues();
			assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderAfterDays,
					"AfterDays must reject raw value " + value);
			assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderRepeatDays,
					"RepeatDays must reject raw value " + value);
		}
	}

	@Test
	void actualYamlDecimalsAndOverflowLongsCannotNarrowToOneDay() throws Exception {
		YamlConfiguration data = new YamlConfiguration();
		Config config = config(data);
		for (String raw : new String[] { "1.5", "1.0", "4294967297", "'7'" }) {
			data.loadFromString(ROOT + ":\n  AfterDays: " + raw + "\n  RepeatDays: " + raw + "\n");
			config.loadValues();
			assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderAfterDays);
			assertThrows(IllegalArgumentException.class, config::getAutoCreateVoteSitesReminderRepeatDays);
		}
	}

	private static Config config(YamlConfiguration data) {
		Config config = mock(Config.class, CALLS_REAL_METHODS);
		doReturn(data).when(config).getData();
		return config;
	}
}
