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
			if (codePoint == '%' || codePoint == '&') inert.append(FORMATTING_BOUNDARY);
			offset += Character.charCount(codePoint);
		}
		return inert.toString();
	}

	/** Returns whether a trusted template opens placeholder/color syntax immediately before this token. */
	public static boolean requiresLeadingActionBoundary(String template, String placeholder) {
		if (template == null || placeholder == null || placeholder.isEmpty()) return false;
		String token = "%" + placeholder + "%";
		for (int offset = 0; offset <= template.length() - token.length(); offset++) {
			if (template.regionMatches(true, offset, token, 0, token.length()) && offset > 0) {
				char previous = template.charAt(offset - 1);
				if (previous == '%' || previous == '&') return true;
			}
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
			if (!template.regionMatches(true, offset, token, 0, token.length()) || offset == 0) continue;
			char previous = template.charAt(offset - 1);
			if (previous != '%' && previous != '&') continue;
			if (result == null) result = new StringBuilder(template.length() + 4);
			result.append(template, copiedThrough, offset).append(FORMATTING_BOUNDARY);
			copiedThrough = offset;
		}
		return result == null ? template : result.append(template, copiedThrough, template.length()).toString();
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
