package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.concurrent.TimeUnit;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.time.ParsedDuration;

/**
 * Per-milestone retrigger limiter.
 *
 * Stored data is lastTriggeredMs (epoch millis).
 */
public final class VoteMilestoneLimit {

	/**
	 * Limit types for milestone retriggering.
	 */
	public enum Type {
		/**
		 * No limit.
		 */
		NONE,
		/**
		 * Once per calendar day window.
		 */
		WINDOW_DAY,
		/**
		 * Once per week window.
		 */
		WINDOW_WEEK,
		/**
		 * Once per month window.
		 */
		WINDOW_MONTH,
		/**
		 * Fixed cooldown duration.
		 */
		COOLDOWN
	}

	private final Type type;
	private final long cooldownMillis; // fixed millis cooldown

	/**
	 * Constructs a new VoteMilestoneLimit.
	 *
	 * @param type limit type
	 * @param cooldownMillis cooldown duration in milliseconds
	 */
	public VoteMilestoneLimit(Type type, long cooldownMillis) {
		this.type = type == null ? Type.NONE : type;
		this.cooldownMillis = Math.max(0L, cooldownMillis);
	}

	/**
	 * Creates a VoteMilestoneLimit with no restrictions.
	 *
	 * @return limit with NONE type
	 */
	public static VoteMilestoneLimit none() {
		return new VoteMilestoneLimit(Type.NONE, 0L);
	}

	/**
	 * Gets the limit type.
	 *
	 * @return limit type
	 */
	public Type getType() {
		return type;
	}

	/**
	 * Gets the cooldown duration in milliseconds.
	 *
	 * @return cooldown milliseconds
	 */
	public long getCooldownMillis() {
		return cooldownMillis;
	}

	/**
	 * Checks if the limit is enabled.
	 *
	 * @return true if limit is enabled
	 */
	public boolean isEnabled() {
		return type != Type.NONE;
	}

	/**
	 * Parses a VoteMilestoneLimit from configuration.
	 *
	 * @param milestoneSection milestone configuration section
	 * @return parsed limit or none()
	 */
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

			// Old behavior: number-only means MINUTES
			ParsedDuration pd = ParsedDuration.parse(durRaw, TimeUnit.MINUTES);

			// Optional legacy support:
			// if no duration provided but Minutes exists
			if (pd.isEmpty()) {
				long minutes = sec.getLong("Minutes", 0L);
				if (minutes > 0) {
					return new VoteMilestoneLimit(Type.COOLDOWN, minutes * 60_000L);
				}
				return none(); // cooldown with no duration => treat as NONE
			}

			return new VoteMilestoneLimit(Type.COOLDOWN, pd.getMillis());
		}

		// window types or none
		return new VoteMilestoneLimit(type, 0L);
	}

	private static Type parseType(String raw) {
		if (raw == null) {
			return Type.NONE;
		}
		String v = raw.trim().toUpperCase();
		if (v.isEmpty()) {
			return Type.NONE;
		}

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

	/* ---------------- enforcement ---------------- */

	/**
	 * Checks if the limit allows retriggering at the given time.
	 *
	 * @param lastTriggeredMs last triggered time in milliseconds
	 * @param nowMs current time in milliseconds
	 * @param zone time zone for window calculations
	 * @return true if retriggering is allowed
	 */
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
			return nowMs >= nextAllowedMs(lastTriggeredMs);
		default:
			return true;
		}
	}

	/**
	 * Calculates the next allowed trigger time in milliseconds.
	 *
	 * @param lastTriggeredMs last triggered time in milliseconds
	 * @return next allowed time or 0 if not applicable
	 */
	public long nextAllowedMs(long lastTriggeredMs) {
		if (type != Type.COOLDOWN || lastTriggeredMs <= 0) {
			return 0L;
		}
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
