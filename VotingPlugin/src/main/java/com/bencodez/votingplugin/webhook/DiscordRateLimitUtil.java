package com.bencodez.votingplugin.webhook;

/**
 * Utility methods for handling Discord webhook rate-limit responses.
 */
public final class DiscordRateLimitUtil {

	private DiscordRateLimitUtil() {
		// utility
	}

	/**
	 * Extracts Discord's "retry_after" value from a JSON string.
	 *
	 * <p>Discord rate-limit bodies typically look like:
	 * {"message":"You are being rate limited.","retry_after":1.234,"global":false}</p>
	 *
	 * <p>This method avoids requiring a JSON library by doing a small string parse.
	 * If parsing fails, returns 0.</p>
	 *
	 * @param body response body
	 * @return retry_after in milliseconds, or 0 if not found/parseable
	 */
	public static long extractRetryAfterMs(String body) {
		if (body == null || body.isEmpty()) {
			return 0;
		}

		int idx = body.indexOf("\"retry_after\"");
		if (idx == -1) {
			idx = body.indexOf("retry_after");
			if (idx == -1) {
				return 0;
			}
		}

		int colon = body.indexOf(':', idx);
		if (colon == -1) {
			return 0;
		}

		int start = colon + 1;
		while (start < body.length() && Character.isWhitespace(body.charAt(start))) {
			start++;
		}

		int end = start;
		while (end < body.length()) {
			char c = body.charAt(end);
			if ((c >= '0' && c <= '9') || c == '.' || c == '-') {
				end++;
				continue;
			}
			break;
		}

		if (end <= start) {
			return 0;
		}

		try {
			double seconds = Double.parseDouble(body.substring(start, end));
			if (seconds <= 0) {
				return 0;
			}
			// cushion so we don't immediately collide again
			return (long) Math.ceil(seconds * 1000.0) + 250L;
		} catch (NumberFormatException ignored) {
			return 0;
		}
	}
}
