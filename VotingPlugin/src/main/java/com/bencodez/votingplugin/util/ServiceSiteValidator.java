package com.bencodez.votingplugin.util;

/**
 * Validates service-site names received from vote sources.
 */
public final class ServiceSiteValidator {
	/** Maximum accepted UTF-16 length for a service-site identifier. */
	public static final int MAX_LENGTH = 2048;
	private static final int MAX_LOG_LENGTH = 128;
	private static final String FORMATTING_BOUNDARY = "\u2060";

	private ServiceSiteValidator() {
	}

	/**
	 * Tests whether a service-site name contains only supported characters.
	 *
	 * @param serviceSite service-site name supplied by a vote source
	 * @return {@code true} for a bounded, visible service-site name that does not
	 *         contain structural formatting delimiters or control characters
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

	/**
	 * Keeps a validated external identifier visually unchanged while preventing a
	 * surrounding trusted template from completing a formatting token across a
	 * substitution boundary.
	 */
	public static String inertForFormatting(String value) {
		if (value == null || value.isEmpty()) return value == null ? "" : value;
		StringBuilder inert = new StringBuilder(value.length() + 2).append(FORMATTING_BOUNDARY);
		for (int offset = 0; offset < value.length();) {
			int codePoint = value.codePointAt(offset);
			inert.appendCodePoint(codePoint);
			if (codePoint == '%' || codePoint == '&') inert.append(FORMATTING_BOUNDARY);
			offset += Character.charCount(codePoint);
		}
		return inert.append(FORMATTING_BOUNDARY).toString();
	}

	/**
	 * Keeps ordinary service identifiers byte-for-byte compatible in reward actions
	 * while breaking placeholder/color-token syntax supplied by an external vote
	 * source. Unlike {@link #inertForFormatting(String)}, this does not add boundary
	 * markers around otherwise safe values.
	 */
	public static String inertForActions(String value) {
		return inertForActions(value, false);
	}

	/** Guards an action value, optionally breaking a token opened by its trusted template. */
	public static String inertForActions(String value, boolean leadingBoundary) {
		if (value == null || value.isEmpty()) return value == null ? "" : value;
		StringBuilder inert = new StringBuilder(value.length() + 2);
		if (leadingBoundary) inert.append(FORMATTING_BOUNDARY);
		for (int offset = 0; offset < value.length();) {
			int codePoint = value.codePointAt(offset);
			inert.appendCodePoint(codePoint);
			if (codePoint == '%' && opensPlaceholderIn(value, offset)
					|| codePoint == '&' && opensColorIn(value, offset)) inert.append(FORMATTING_BOUNDARY);
			offset += Character.charCount(codePoint);
		}
		return inert.toString();
	}

	private static boolean opensPlaceholderIn(String value, int offset) {
		int closing = value.indexOf('%', offset + 1);
		if (closing <= offset + 1) return false;
		return isPlaceholderFragment(value, offset + 1, closing);
	}

	private static boolean opensColorIn(String value, int offset) {
		if (offset + 1 >= value.length()) return false;
		char code = value.charAt(offset + 1);
		if (isLegacyColorCode(code)) return true;
		if (code != '#' || offset + 8 > value.length()) return false;
		for (int index = offset + 2; index < offset + 8; index++) {
			if (Character.digit(value.charAt(index), 16) < 0) return false;
		}
		return true;
	}

	private static boolean isLegacyColorCode(char code) {
		char lower = Character.toLowerCase(code);
		return lower >= '0' && lower <= '9' || lower >= 'a' && lower <= 'f'
				|| lower >= 'k' && lower <= 'o' || lower == 'r' || lower == 'x';
	}

	/** Returns whether substitution occurs inside open placeholder/color syntax. */
	public static boolean requiresLeadingActionBoundary(String template, String placeholder) {
		if (template == null || placeholder == null || placeholder.isEmpty()) return false;
		String token = "%" + placeholder + "%";
		for (int offset = 0; offset <= template.length() - token.length(); offset++) {
			if (template.regionMatches(true, offset, token, 0, token.length())
					&& opensActionTokenAt(template, offset)) return true;
		}
		return false;
	}

	/** Breaks only token openers that a trusted template places before the placeholder. */
	public static String inertTemplateBoundaries(String template, String placeholder) {
		if (template == null || placeholder == null || placeholder.isEmpty()) return template;
		String token = "%" + placeholder + "%";
		StringBuilder result = null;
		int copiedThrough = 0;
		for (int offset = 0; offset <= template.length() - token.length(); offset++) {
			if (!template.regionMatches(true, offset, token, 0, token.length())
					|| !opensActionTokenAt(template, offset)) continue;
			if (result == null) result = new StringBuilder(template.length() + 4);
			result.append(template, copiedThrough, offset).append(FORMATTING_BOUNDARY);
			copiedThrough = offset;
		}
		return result == null ? template : result.append(template, copiedThrough, template.length()).toString();
	}

	private static boolean opensActionTokenAt(String template, int offset) {
		if (offset <= 0) return false;
		if (opensColorTokenAt(template, offset)) return true;
		int opener = template.lastIndexOf('%', offset - 1);
		if (opener < 0) return false;
		if (opener + 1 < offset) return isPlaceholderFragment(template, opener + 1, offset);
		int prior = template.lastIndexOf('%', opener - 1);
		return prior < 0 || !isPlaceholderFragment(template, prior + 1, opener);
	}

	private static boolean isPlaceholderFragment(String value, int start, int end) {
		if (start >= end) return false;
		char first = value.charAt(start);
		if (!Character.isLetter(first) && first != '_') return false;
		for (int index = start + 1; index < end; index++) {
			char character = value.charAt(index);
			if (Character.isWhitespace(character) || character == '%') return false;
		}
		return true;
	}

	private static boolean opensColorTokenAt(String template, int offset) {
		int ampersand = template.lastIndexOf('&', offset - 1);
		if (ampersand < 0) return false;
		int partialLength = offset - ampersand - 1;
		if (partialLength == 0) return true;
		if (template.charAt(ampersand + 1) != '#' || partialLength > 6) return false;
		for (int index = ampersand + 2; index < offset; index++) {
			if (Character.digit(template.charAt(index), 16) < 0) return false;
		}
		return true;
	}

	private static boolean isDisallowed(int codePoint) {
		if (codePoint == '[' || codePoint == ']' || codePoint == '\'' || codePoint == '"' || codePoint == '`'
				|| codePoint == '\u00A7'
				|| codePoint == '\\' || codePoint == '{' || codePoint == '}') {
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
