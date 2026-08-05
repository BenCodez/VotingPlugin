package com.bencodez.votingplugin.util;

/**
 * Validates service-site names received from vote sources.
 */
public final class ServiceSiteValidator {
	private static final int MAX_LENGTH = 2048;
	private static final int MAX_LOG_LENGTH = 128;

	private ServiceSiteValidator() {
	}

	/**
	 * Tests whether a service-site name contains only supported characters.
	 *
	 * @param serviceSite service-site name supplied by a vote source
	 * @return {@code true} for a bounded, visible service-site name that does not
	 *         contain unsupported delimiters
	 */
	public static boolean isValid(String serviceSite) {
		if (serviceSite == null || serviceSite.isBlank() || serviceSite.length() > MAX_LENGTH) {
			return false;
		}

		for (int offset = 0; offset < serviceSite.length();) {
			int codePoint = serviceSite.codePointAt(offset);
			if (isDisallowed(codePoint)) {
				return false;
			}
			offset += Character.charCount(codePoint);
		}
		return true;
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
		int offset = 0;
		while (offset < value.length() && sanitized.length() < MAX_LOG_LENGTH) {
			int codePoint = value.codePointAt(offset);
			if (isDisallowed(codePoint)) {
				sanitized.append('?');
			} else {
				sanitized.appendCodePoint(codePoint);
			}
			offset += Character.charCount(codePoint);
		}
		if (offset < value.length()) {
			sanitized.append("...");
		}
		return sanitized.toString();
	}

	private static boolean isDisallowed(int codePoint) {
		if (codePoint == '[' || codePoint == ']' || codePoint == '\'' || codePoint == '"' || codePoint == '`'
				|| codePoint == '\\') {
			return true;
		}

		int type = Character.getType(codePoint);
		return type == Character.CONTROL || type == Character.FORMAT || type == Character.LINE_SEPARATOR
				|| type == Character.PARAGRAPH_SEPARATOR || type == Character.SURROGATE;
	}
}
