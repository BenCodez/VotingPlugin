package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.ArrayList;
import java.util.List;

/**
 * Parses: - "10..200 step 10" - "10..200"
 *
 * Expands to discrete values (simple + predictable for server owners).
 */
public final class RangeSpec {

	/** The start value of the range. */
	public final long start;
	/** The end value of the range. */
	public final long end;
	/** The step increment for the range. */
	public final long step;

	/**
	 * Constructs a new RangeSpec.
	 *
	 * @param start the start value
	 * @param end the end value
	 * @param step the step increment
	 */
	public RangeSpec(long start, long end, long step) {
		this.start = start;
		this.end = end;
		this.step = step;
	}

	/**
	 * Attempts to parse a range specification string.
	 *
	 * @param s the string to parse
	 * @return the parsed RangeSpec or null if parsing fails
	 */
	public static RangeSpec tryParse(String s) {
		// Normalize whitespace
		String str = s.trim().replaceAll("\\s+", " ");

		if (!str.contains("..")) {
			return null;
		}

		String[] parts = str.split("\\.\\.");
		if (parts.length != 2) {
			return null;
		}

		String left = parts[0].trim();
		String right = parts[1].trim();

		long step = 1;

		// Right side might contain "step"
		String[] rightParts = right.split(" step ");
		String endStr = rightParts[0].trim();
		if (rightParts.length == 2) {
			try {
				step = Long.parseLong(rightParts[1].trim());
			} catch (NumberFormatException ignored) {
				return null;
			}
		} else if (rightParts.length > 2) {
			return null;
		}

		long start;
		long end;
		try {
			start = Long.parseLong(left);
			end = Long.parseLong(endStr);
		} catch (NumberFormatException ignored) {
			return null;
		}

		if (step <= 0) {
			return null;
		}

		// allow reversed ranges by swapping
		if (end < start) {
			long tmp = start;
			start = end;
			end = tmp;
		}

		return new RangeSpec(start, end, step);
	}

	/**
	 * Expands the range into a list of discrete values.
	 *
	 * @return the list of values in the range
	 */
	public List<Long> expand() {
		List<Long> out = new ArrayList<>();
		// Safety: avoid accidental huge expansions
		// If you want, expose this as a config value. For now keep sane.
		final int HARD_MAX = 250_000;

		long count = 0;
		for (long v = start; v <= end; v += step) {
			out.add(v);
			count++;
			if (count >= HARD_MAX) {
				break;
			}
		}
		return out;
	}
}
