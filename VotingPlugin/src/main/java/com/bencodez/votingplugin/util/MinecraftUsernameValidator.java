package com.bencodez.votingplugin.util;

import java.util.regex.Pattern;

/**
 * Validates player names received from external vote sources.
 */
public final class MinecraftUsernameValidator {
	private static final int MAX_LENGTH = 16;
	private static final int MAX_LOG_LENGTH = 32;
	private static final Pattern VALID_USERNAME = Pattern.compile("[A-Za-z0-9_]{1," + MAX_LENGTH + "}");

	private MinecraftUsernameValidator() {
	}

	/**
	 * Tests whether a value uses the Minecraft Java username syntax.
	 *
	 * @param username username supplied by an external source
	 * @return {@code true} for a 1-16 character ASCII letter, digit, or underscore
	 */
	public static boolean isValid(String username) {
		return username != null && VALID_USERNAME.matcher(username).matches();
	}

	/**
	 * Tests a Java username or a Bedrock username using the configured prefix. The
	 * prefix is matched literally and the remaining player name must still use the
	 * normal 1-16 character username syntax.
	 *
	 * @param username            username supplied by an external source
	 * @param bedrockPlayerPrefix configured Bedrock player prefix
	 * @return {@code true} when the username is valid for Java or for the configured
	 *         Bedrock prefix
	 */
	public static boolean isValid(String username, String bedrockPlayerPrefix) {
		if (isValid(username)) {
			return true;
		}
		if (username == null || bedrockPlayerPrefix == null || bedrockPlayerPrefix.isEmpty()
				|| !username.startsWith(bedrockPlayerPrefix)) {
			return false;
		}
		return isValid(username.substring(bedrockPlayerPrefix.length()));
	}

	/**
	 * Produces a bounded, single-line representation safe to include in logs.
	 *
	 * @param value untrusted external value
	 * @return sanitized value
	 */
	public static String sanitizeForLog(String value) {
		if (value == null) {
			return "<null>";
		}

		StringBuilder sanitized = new StringBuilder(Math.min(value.length(), MAX_LOG_LENGTH));
		int length = Math.min(value.length(), MAX_LOG_LENGTH);
		for (int i = 0; i < length; i++) {
			char character = value.charAt(i);
			if ((character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
					|| (character >= '0' && character <= '9') || character == '_') {
				sanitized.append(character);
			} else {
				sanitized.append('?');
			}
		}
		if (value.length() > MAX_LOG_LENGTH) {
			sanitized.append("...");
		}
		return sanitized.toString();
	}
}
