package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.MinecraftUsernameValidator;

class MinecraftUsernameValidatorTest {

	@Test
	void acceptsValidMinecraftUsernames() {
		for (String username : new String[] { "MchtName15Chars", "MchtNameOver16xx", "Player_123" }) {
			assertTrue(MinecraftUsernameValidator.isValid(username), username);
		}
	}

	@Test
	void acceptsOnlyTheConfiguredBedrockPrefix() {
		assertTrue(MinecraftUsernameValidator.isValid(".Player", "."));
		assertTrue(MinecraftUsernameValidator.isValid("-Player", "-"));
		assertTrue(MinecraftUsernameValidator.isValid("+-Player", "+-"));
		assertTrue(MinecraftUsernameValidator.isValid("-MchtNameOver16xx", "-"));

		assertFalse(MinecraftUsernameValidator.isValid(".Player", "-"));
		assertFalse(MinecraftUsernameValidator.isValid("-Player", "."));
		assertFalse(MinecraftUsernameValidator.isValid(".Player", ""));
		assertFalse(MinecraftUsernameValidator.isValid(".Player", null));
	}

	@Test
	void rejectsInvalidBedrockBaseNames() {
		for (String username : new String[] { ".", ".MchtNameOver16xxx", "../MchtTraversal", ".Mcht/Slash",
				".Mcht\\Slash", ".Mcht Space", ".Mcht\tTab", ".Mcht\u00E9Unicode" }) {
			assertFalse(MinecraftUsernameValidator.isValid(username, "."), username);
		}
	}

	@Test
	void rejectsInvalidMinecraftUsernames() {
		for (String username : new String[] { "MchtNameOver16xxx", "../MchtTraversal", "Mcht/Slash", "Mcht\\Slash",
				"Mcht Space", "Mcht\tTab", "Mcht\nLine", "Mcht\u0000Control", "Mcht- punctuation",
				"Mcht\u00E9Unicode", "\uD800" }) {
			assertFalse(MinecraftUsernameValidator.isValid(username), username);
		}
	}
}
