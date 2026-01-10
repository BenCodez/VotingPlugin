package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;

import org.bukkit.configuration.ConfigurationSection;

/**
 * Per-milestone retrigger limiter.
 *
 * Stored data is lastTriggeredMs (epoch millis).
 */
public final class VoteMilestoneLimit {

	public enum Type {
		NONE,
		WINDOW_DAY,
		WINDOW_WEEK,
		WINDOW_MONTH,
		COOLDOWN
	}

	private final Type type;
	private final long cooldownMillis; // for COOLDOWN when not using months
	private final int cooldownMonths; // for COOLDOWN when Duration uses "mo"

	public VoteMilestoneLimit(Type type, long cooldownMillis, int cooldownMonths) {
		this.type = type == null ? Type.NONE : type;
		this.cooldownMillis = Math.max(0L, cooldownMillis);
		this.cooldownMonths = Math.max(0, cooldownMonths);
	}

	public static VoteMilestoneLimit none() {
		return new VoteMilestoneLimit(Type.NONE, 0L, 0);
	}

	public Type getType() {
		return type;
	}

	public long getCooldownMillis() {
		return cooldownMillis;
	}

	public int getCooldownMonths() {
		return cooldownMonths;
	}

	public boolean isEnabled() {
		return type != Type.NONE;
	}

	public static VoteMilestoneLimit fromConfig(ConfigurationSection milestoneSection) {
		if (milestoneSection == null) {
			return none();
		}

		ConfigurationSection sec = milestoneSection.getConfigurationSection("Limit");
		if (sec == null) {
			return none();
		}

		String typeStr = sec.getString("Type", "NONE");
		Type type = parseType(typeStr);

		if (type == Type.COOLDOWN) {
			String durRaw = sec.getString("Duration", "");
			ParsedDuration pd = parseDurationFlexible(durRaw);

			// Allow alternate numeric key if you want later:
			// if no duration provided but CooldownMinutes exists, etc.
			if (pd.isEmpty()) {
				long minutes = sec.getLong("Minutes", 0L);
				if (minutes > 0) {
					return new VoteMilestoneLimit(Type.COOLDOWN, minutes * 60_000L, 0);
				}
				return none(); // cooldown with no duration => treat as NONE
			}

			return new VoteMilestoneLimit(Type.COOLDOWN, pd.millis, pd.months);
		}

		// window types or none
		return new VoteMilestoneLimit(type, 0L, 0);
	}

	private static Type parseType(String raw) {
		if (raw == null)
			return Type.NONE;
		String v = raw.trim().toUpperCase();
		if (v.isEmpty())
			return Type.NONE;

		// Allow friendly aliases
		switch (v) {
		case "DAY":
		case "DAILY":
		case "WINDOW_DAILY":
			return Type.WINDOW_DAY;
		case "WEEK":
		case "WEEKLY":
			return Type.WINDOW_WEEK;
		case "MONTH":
		case "MONTHLY":
			return Type.WINDOW_MONTH;
		default:
			break;
		}

		try {
			return Type.valueOf(v);
		} catch (Exception ignored) {
			return Type.NONE;
		}
	}

	private static final class ParsedDuration {
		final long millis;
		final int months;

		ParsedDuration(long millis, int months) {
			this.millis = millis;
			this.months = months;
		}

		boolean isEmpty() {
			return millis <= 0 && months <= 0;
		}
	}

	/**
	 * Parses a duration like: 30m, 12h, 1d, 2w, 1mo, 5000ms, 60s.
	 *
	 * If raw is just a number, it's interpreted as MINUTES.
	 */
	private static ParsedDuration parseDurationFlexible(String raw) {
		if (raw == null)
			return new ParsedDuration(0L, 0);

		String s = raw.trim().toLowerCase();
		if (s.isEmpty())
			return new ParsedDuration(0L, 0);

		// ISO-8601 duration support as a bonus: PT30M, PT12H, P1D, etc.
		// (Months in ISO are "P1M" which is ambiguous vs minutes. We'll not parse that here.)
		if (s.startsWith("p")) {
			try {
				Duration d = Duration.parse(s.toUpperCase());
				return new ParsedDuration(Math.max(0L, d.toMillis()), 0);
			} catch (Exception ignored) {
			}
		}

		// number-only => minutes
		if (s.matches("^[0-9]+$")) {
			long minutes = safeParseLong(s);
			return new ParsedDuration(minutes * 60_000L, 0);
		}

		// months: "1mo"
		if (s.endsWith("mo")) {
			String n = s.substring(0, s.length() - 2).trim();
			int months = (int) Math.max(0, safeParseLong(n));
			return new ParsedDuration(0L, months);
		}

		// ms: "5000ms"
		if (s.endsWith("ms")) {
			String n = s.substring(0, s.length() - 2).trim();
			long ms = Math.max(0L, safeParseLong(n));
			return new ParsedDuration(ms, 0);
		}

		// single-unit suffix
		char unit = s.charAt(s.length() - 1);
		String n = s.substring(0, s.length() - 1).trim();
		long val = Math.max(0L, safeParseLong(n));

		switch (unit) {
		case 's':
			return new ParsedDuration(val * 1000L, 0);
		case 'm':
			return new ParsedDuration(val * 60_000L, 0);
		case 'h':
			return new ParsedDuration(val * 3_600_000L, 0);
		case 'd':
			return new ParsedDuration(val * 86_400_000L, 0);
		case 'w':
			return new ParsedDuration(val * 604_800_000L, 0);
		default:
			// unknown => treat as minutes (safe fallback)
			return new ParsedDuration(val * 60_000L, 0);
		}
	}

	private static long safeParseLong(String s) {
		try {
			return Long.parseLong(s);
		} catch (Exception e) {
			return 0L;
		}
	}

	/* ---------------- enforcement ---------------- */

	public boolean allows(long lastTriggeredMs, long nowMs, ZoneId zone) {
		if (type == Type.NONE) {
			return true;
		}
		if (lastTriggeredMs <= 0) {
			return true;
		}
		if (zone == null) {
			zone = ZoneId.systemDefault();
		}

		switch (type) {
		case WINDOW_DAY:
			return !sameDay(lastTriggeredMs, nowMs, zone);
		case WINDOW_WEEK:
			return !sameWeek(lastTriggeredMs, nowMs, zone);
		case WINDOW_MONTH:
			return !sameMonth(lastTriggeredMs, nowMs, zone);
		case COOLDOWN:
			return nowMs >= nextAllowedMs(lastTriggeredMs, zone);
		default:
			return true;
		}
	}

	public long nextAllowedMs(long lastTriggeredMs, ZoneId zone) {
		if (type != Type.COOLDOWN || lastTriggeredMs <= 0) {
			return 0L;
		}
		if (zone == null) {
			zone = ZoneId.systemDefault();
		}

		// Month-aware cooldown: add calendar months
		if (cooldownMonths > 0) {
			ZonedDateTime zdt = Instant.ofEpochMilli(lastTriggeredMs).atZone(zone);
			return zdt.plusMonths(cooldownMonths).toInstant().toEpochMilli();
		}

		// Fixed millis cooldown
		return lastTriggeredMs + cooldownMillis;
	}

	private static boolean sameDay(long aMs, long bMs, ZoneId zone) {
		ZonedDateTime a = Instant.ofEpochMilli(aMs).atZone(zone);
		ZonedDateTime b = Instant.ofEpochMilli(bMs).atZone(zone);
		return a.toLocalDate().equals(b.toLocalDate());
	}

	private static boolean sameMonth(long aMs, long bMs, ZoneId zone) {
		ZonedDateTime a = Instant.ofEpochMilli(aMs).atZone(zone);
		ZonedDateTime b = Instant.ofEpochMilli(bMs).atZone(zone);
		return a.getYear() == b.getYear() && a.getMonthValue() == b.getMonthValue();
	}

	private static boolean sameWeek(long aMs, long bMs, ZoneId zone) {
		ZonedDateTime a = Instant.ofEpochMilli(aMs).atZone(zone);
		ZonedDateTime b = Instant.ofEpochMilli(bMs).atZone(zone);

		// ISO week fields is a sane default
		java.time.temporal.WeekFields wf = java.time.temporal.WeekFields.ISO;
		int aw = a.get(wf.weekOfWeekBasedYear());
		int ay = a.get(wf.weekBasedYear());

		int bw = b.get(wf.weekOfWeekBasedYear());
		int by = b.get(wf.weekBasedYear());

		return aw == bw && ay == by;
	}
}
