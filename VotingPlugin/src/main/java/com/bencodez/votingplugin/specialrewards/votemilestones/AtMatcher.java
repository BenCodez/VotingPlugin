package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.bukkit.configuration.ConfigurationSection;

/**
 * Supports: - At: 0 - At: 50 - At: [20, 50, 100] - At: - 10..200 step 10 - 250
 *
 * Stateless evaluation: matches based solely on newTotal.
 */
public class AtMatcher {

	private final boolean hasZero;
	private final Set<Long> exactTotals;

	private AtMatcher(boolean hasZero, Set<Long> exactTotals) {
		this.hasZero = hasZero;
		this.exactTotals = exactTotals;
	}

	public boolean matches(long newTotal) {
		// At: 0 => first vote in that period => newTotal == 1
		if (hasZero && newTotal == 1) {
			return true;
		}
		return exactTotals.contains(newTotal);
	}

	/**
	 * Human-friendly debug string.
	 *
	 * Examples: - "0" - "20,50,100" - "0 + 10 values: [10,20,30,...,980,990,1000]"
	 */
	public String toDebugString() {
		return toDebugString(8, 3);
	}

	public boolean hasZero() {
		return hasZero;
	}

	public Set<Long> getExactTotals() {
		return java.util.Collections.unmodifiableSet(exactTotals);
	}

	/** Returns a compact string for logs. */
	public String toDebugString(int maxValues) {
		StringBuilder sb = new StringBuilder();
		boolean first = true;

		if (hasZero) {
			sb.append("0(first)");
			first = false;
		}

		java.util.List<Long> vals = new java.util.ArrayList<>(exactTotals);
		java.util.Collections.sort(vals);

		int shown = 0;
		for (Long v : vals) {
			if (shown >= maxValues) {
				sb.append(first ? "" : "|").append("...+").append(vals.size() - shown);
				break;
			}
			sb.append(first ? "" : "|").append(v);
			first = false;
			shown++;
		}

		if (sb.length() == 0) {
			return "none";
		}
		return sb.toString();
	}

	/**
	 * @param head how many smallest values to show
	 * @param tail how many largest values to show when truncating
	 */
	public String toDebugString(int head, int tail) {
		head = Math.max(0, head);
		tail = Math.max(0, tail);

		int size = exactTotals != null ? exactTotals.size() : 0;
		boolean zero = hasZero;

		if (!zero && size == 0) {
			return "";
		}

		// If it's small, print everything
		int inlineLimit = head + tail + 1;
		TreeSet<Long> sorted = new TreeSet<>();
		if (exactTotals != null) {
			sorted.addAll(exactTotals);
		}

		List<String> parts = new ArrayList<>();
		if (zero) {
			parts.add("0");
		}

		if (size == 0) {
			return String.join(",", parts);
		}

		if (size <= inlineLimit) {
			for (Long v : sorted) {
				parts.add(String.valueOf(v));
			}
			return String.join(",", parts);
		}

		// Truncated format: include count + head...tail
		List<Long> headVals = new ArrayList<>(head);
		List<Long> tailVals = new ArrayList<>(tail);

		int i = 0;
		for (Long v : sorted) {
			if (i < head) {
				headVals.add(v);
			}
			i++;
		}

		if (tail > 0) {
			List<Long> rev = new ArrayList<>(sorted);
			Collections.reverse(rev);
			for (int j = 0; j < tail && j < rev.size(); j++) {
				tailVals.add(rev.get(j));
			}
			Collections.reverse(tailVals);
		}

		StringBuilder sb = new StringBuilder();
		if (zero) {
			sb.append("0 + ");
		}
		sb.append(size).append(" values: [");

		boolean first = true;
		for (Long v : headVals) {
			if (!first)
				sb.append(",");
			first = false;
			sb.append(v);
		}

		sb.append(",...");

		for (Long v : tailVals) {
			sb.append(",").append(v);
		}

		sb.append("]");
		return sb.toString();
	}

	@Override
	public String toString() {
		// make default toString() useful in logs
		return toDebugString();
	}

	public static AtMatcher fromConfig(ConfigurationSection sec, String path) {
		if (sec == null || !sec.contains(path)) {
			return null;
		}

		boolean hasZero = false;
		Set<Long> set = new HashSet<>();

		Object raw = sec.get(path);

		List<Object> entries = new ArrayList<>();
		if (raw instanceof List) {
			entries.addAll(sec.getList(path));
		} else {
			entries.add(raw);
		}

		for (Object o : entries) {
			if (o == null) {
				continue;
			}

			if (o instanceof Number) {
				long v = ((Number) o).longValue();
				if (v == 0) {
					hasZero = true;
				} else if (v > 0) {
					set.add(v);
				}
				continue;
			}

			String s = String.valueOf(o).trim();
			if (s.isEmpty()) {
				continue;
			}

			// Allow "0" as string
			if (s.equals("0")) {
				hasZero = true;
				continue;
			}

			// Range: "A..B step S" or "A..B"
			RangeSpec r = RangeSpec.tryParse(s);
			if (r != null) {
				if (r.start == 0) {
					// if someone writes 0..something, treat 0 specially (first vote)
					hasZero = true;
				}
				for (long v : r.expand()) {
					if (v == 0) {
						hasZero = true;
					} else if (v > 0) {
						set.add(v);
					}
				}
				continue;
			}

			// Fallback: parse as long
			try {
				long v = Long.parseLong(s);
				if (v == 0) {
					hasZero = true;
				} else if (v > 0) {
					set.add(v);
				}
			} catch (NumberFormatException ignored) {
				// ignore invalid entry
			}
		}

		if (!hasZero && set.isEmpty()) {
			return null;
		}
		return new AtMatcher(hasZero, set);
	}
}
