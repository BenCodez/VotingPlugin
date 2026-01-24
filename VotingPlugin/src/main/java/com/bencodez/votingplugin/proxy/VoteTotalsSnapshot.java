package com.bencodez.votingplugin.proxy;

import java.util.UUID;
import java.util.regex.Pattern;

import lombok.Getter;
import lombok.Setter;

public class VoteTotalsSnapshot {

	private static final String SEP = "//";
	private static final Pattern SEP_PATTERN = Pattern.compile(Pattern.quote(SEP));

	// Current storage version
	private static final int CURRENT_VERSION = 2;

	@Getter
	@Setter
	private int allTimeTotal;
	@Getter
	@Setter
	private int monthTotal;
	@Getter
	@Setter
	private int weeklyTotal;
	@Getter
	@Setter
	private int dailyTotal;
	@Getter
	@Setter
	private int points;

	@Getter
	@Setter
	private int votePartyCurrent;
	@Getter
	@Setter
	private int votePartyRequired;

	@Getter
	@Setter
	private int dateMonthTotal;
	@Getter
	@Setter
	private UUID voteUUID;

	public VoteTotalsSnapshot() {
	}

	public VoteTotalsSnapshot(int allTimeTotal, int monthTotal, int weeklyTotal, int dailyTotal, int points,
			int votePartyCurrent, int votePartyRequired, int dateMonthTotal, UUID voteUUID) {
		this.allTimeTotal = allTimeTotal;
		this.monthTotal = monthTotal;
		this.weeklyTotal = weeklyTotal;
		this.dailyTotal = dailyTotal;
		this.points = points;
		this.votePartyCurrent = votePartyCurrent;
		this.votePartyRequired = votePartyRequired;
		this.dateMonthTotal = dateMonthTotal;
		this.voteUUID = voteUUID;
	}

	public static VoteTotalsSnapshot parseStorage(String raw) {
		VoteTotalsSnapshot out = new VoteTotalsSnapshot();
		if (raw == null || raw.isEmpty()) {
			return out;
		}

		String[] t = SEP_PATTERN.split(raw, -1);
		if (t.length < 5) {
			return out;
		}

		int idx = 0;
		int version = 1; // legacy default

		// Versioned?
		if (t[0].startsWith("v")) {
			version = parseInt(t[0].substring(1), 1);
			idx = 1;
		}

		switch (version) {
		case 1:
			parseV1(out, t, idx);
			break;
		case 2:
			parseV2(out, t, idx);
			break;
		default:
			// Unknown future version → best-effort using v2 layout
			parseV2(out, t, idx);
			break;
		}

		return out;
	}

	/*
	 * ========================= Writers (always v2) =========================
	 */

	public String toStorageString() {
		return "v" + CURRENT_VERSION + SEP + allTimeTotal + SEP + monthTotal + SEP + weeklyTotal + SEP + dailyTotal
				+ SEP + points + SEP + votePartyCurrent + SEP + votePartyRequired + SEP + dateMonthTotal + SEP
				+ (voteUUID != null ? voteUUID.toString() : "");
	}

	@Override
	public String toString() {
		return toStorageString();
	}

	// Legacy (milestoneCount exists, but ignored)
	private static void parseV1(VoteTotalsSnapshot out, String[] t, int i) {
		out.allTimeTotal = parseInt(t, i++, 0);
		out.monthTotal = parseInt(t, i++, 0);
		out.weeklyTotal = parseInt(t, i++, 0);
		out.dailyTotal = parseInt(t, i++, 0);
		out.points = parseInt(t, i++, 0);

		i++; // skip milestoneCount

		out.votePartyCurrent = parseInt(t, i++, 0);
		out.votePartyRequired = parseInt(t, i++, 0);
		out.dateMonthTotal = parseInt(t, i++, 0);
		out.voteUUID = parseUUID(t, i);
	}

	// Current (no milestoneCount)
	private static void parseV2(VoteTotalsSnapshot out, String[] t, int i) {
		out.allTimeTotal = parseInt(t, i++, 0);
		out.monthTotal = parseInt(t, i++, 0);
		out.weeklyTotal = parseInt(t, i++, 0);
		out.dailyTotal = parseInt(t, i++, 0);
		out.points = parseInt(t, i++, 0);

		out.votePartyCurrent = parseInt(t, i++, 0);
		out.votePartyRequired = parseInt(t, i++, 0);
		out.dateMonthTotal = parseInt(t, i++, 0);
		out.voteUUID = parseUUID(t, i);
	}

	/*
	 * ========================= Helpers =========================
	 */

	private static int parseInt(String[] t, int idx, int def) {
		if (idx < 0 || idx >= t.length)
			return def;
		try {
			return Integer.parseInt(t[idx].trim());
		} catch (Exception e) {
			return def;
		}
	}

	private static int parseInt(String s, int def) {
		try {
			return Integer.parseInt(s);
		} catch (Exception e) {
			return def;
		}
	}

	private static UUID parseUUID(String[] t, int idx) {
		if (idx < 0 || idx >= t.length)
			return null;
		String s = t[idx].trim();
		if (s.isEmpty() || "null".equalsIgnoreCase(s))
			return null;
		try {
			return UUID.fromString(s);
		} catch (Exception e) {
			return null;
		}
	}
}
