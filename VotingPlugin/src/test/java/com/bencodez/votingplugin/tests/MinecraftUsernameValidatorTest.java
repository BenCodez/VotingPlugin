package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import com.bencodez.votingplugin.util.MinecraftUsernameValidator;

class MinecraftUsernameValidatorTest {

	@ParameterizedTest
	@ValueSource(strings = { "MchtName15Chars", "MchtNameOver16xx", "Player_123" })
	void acceptsValidMinecraftUsernames(String username) {
		assertTrue(MinecraftUsernameValidator.isValid(username));
	}

	@ParameterizedTest
	@ValueSource(strings = { "MchtNameOver16xxx", "../MchtTraversal", "Mcht/Slash", "Mcht\\Slash", "Mcht Space",
			"Mcht\tTab", "Mcht\nLine", "Mcht\u0000Control", "Mcht- punctuation", "Mcht\u00E9Unicode", "\uD800" })
	void rejectsInvalidMinecraftUsernames(String username) {
		assertFalse(MinecraftUsernameValidator.isValid(username));
	}
}
