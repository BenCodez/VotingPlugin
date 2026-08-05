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
		if (serviceSite == null || serviceSite.length() > MAX_LENGTH) {
			return false;
		}

		boolean hasVisibleCharacter = false;
		for (int offset = 0; offset < serviceSite.length();) {
			int codePoint = serviceSite.codePointAt(offset);
			if (isDisallowed(codePoint)) {
				return false;
			}
			if (isVisibleBaseCharacter(codePoint)) {
				hasVisibleCharacter = true;
			}
			offset += Character.charCount(codePoint);
		}
		return hasVisibleCharacter;
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

	private static boolean isVisibleBaseCharacter(int codePoint) {
		if (Character.isWhitespace(codePoint) || Character.isSpaceChar(codePoint)
				|| isDefaultIgnorable(codePoint)) {
			return false;
		}

		int type = Character.getType(codePoint);
		return type != Character.NON_SPACING_MARK && type != Character.COMBINING_SPACING_MARK
				&& type != Character.ENCLOSING_MARK;
	}

	private static boolean isDefaultIgnorable(int codePoint) {
		return codePoint == 0x00AD || codePoint == 0x034F || codePoint == 0x061C
				|| (codePoint >= 0x115F && codePoint <= 0x1160)
				|| (codePoint >= 0x17B4 && codePoint <= 0x17B5)
				|| (codePoint >= 0x180B && codePoint <= 0x180F)
				|| (codePoint >= 0x200B && codePoint <= 0x200F)
				|| (codePoint >= 0x202A && codePoint <= 0x202E)
				|| (codePoint >= 0x2060 && codePoint <= 0x206F) || codePoint == 0x3164
				|| (codePoint >= 0xFE00 && codePoint <= 0xFE0F) || codePoint == 0xFEFF
				|| codePoint == 0xFFA0 || (codePoint >= 0xFFF0 && codePoint <= 0xFFF8)
				|| (codePoint >= 0x1BCA0 && codePoint <= 0x1BCA3)
				|| (codePoint >= 0x1D173 && codePoint <= 0x1D17A)
				|| (codePoint >= 0xE0000 && codePoint <= 0xE0FFF);
	}
}
