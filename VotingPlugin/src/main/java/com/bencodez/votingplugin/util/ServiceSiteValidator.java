package com.bencodez.votingplugin.util;

import java.util.regex.Pattern;

/**
 * Validates service-site names received from vote sources.
 */
public final class ServiceSiteValidator {
	private static final int MAX_LENGTH = 128;
	private static final Pattern VALID_SERVICE_SITE = Pattern
			.compile("[A-Za-z0-9][A-Za-z0-9 ._:/-]{0," + (MAX_LENGTH - 1) + "}");

	private ServiceSiteValidator() {
	}

	/**
	 * Tests whether a service-site name contains only supported characters.
	 *
	 * @param serviceSite service-site name supplied by a vote source
	 * @return {@code true} for a bounded service-site name using supported ASCII
	 *         characters
	 */
	public static boolean isValid(String serviceSite) {
		return serviceSite != null && VALID_SERVICE_SITE.matcher(serviceSite).matches();
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

		StringBuilder sanitized = new StringBuilder(Math.min(value.length(), MAX_LENGTH));
		int length = Math.min(value.length(), MAX_LENGTH);
		for (int i = 0; i < length; i++) {
			char character = value.charAt(i);
			if ((character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
					|| (character >= '0' && character <= '9') || character == ' ' || character == '.'
					|| character == '_' || character == ':' || character == '/' || character == '-') {
				sanitized.append(character);
			} else {
				sanitized.append('?');
			}
		}
		if (value.length() > MAX_LENGTH) {
			sanitized.append("...");
		}
		return sanitized.toString();
	}
}
