package com.bencodez.votingplugin.tests.presets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.presets.PlaceholderDef;
import com.google.gson.Gson;

/**
 * Tests for {@link PlaceholderDef}.
 */
public class PlaceholderDefTest {

	/**
	 * Verifies that Gson maps the JSON {@code default} field to
	 * {@code defaultValue}.
	 */
	@Test
	public void testDefaultValueDeserialization() {
		String json = "{"
				+ "\"type\":\"string\","
				+ "\"label\":\"VoteSite key\","
				+ "\"default\":\"crafty_gg\""
				+ "}";

		PlaceholderDef def = new Gson().fromJson(json, PlaceholderDef.class);

		assertNotNull(def);
		assertEquals("string", def.getType());
		assertEquals("VoteSite key", def.getLabel());
		assertEquals("crafty_gg", def.getDefaultValue());
	}

	/**
	 * Verifies blank defaults remain blank after deserialization.
	 */
	@Test
	public void testBlankDefaultValueDeserialization() {
		String json = "{"
				+ "\"type\":\"string\","
				+ "\"label\":\"VoteDelayDailyHour\","
				+ "\"default\":\"\""
				+ "}";

		PlaceholderDef def = new Gson().fromJson(json, PlaceholderDef.class);

		assertNotNull(def);
		assertEquals("", def.getDefaultValue());
	}
}