package com.bencodez.votingplugin.broadcast;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.time.ParsedDuration;

/**
 * Immutable broadcast configuration.
 *
 * Type: NONE disables broadcasting.
 */
public final class BroadcastSettings {

	private final VoteBroadcastType type;
	private final ParsedDuration duration;
	private final int maxSitesListed;
	private final BroadcastFormat format;

	public BroadcastSettings(VoteBroadcastType type, ParsedDuration duration, int maxSitesListed, BroadcastFormat format) {
		this.type = type == null ? VoteBroadcastType.NONE : type;
		this.duration = duration == null ? ParsedDuration.empty() : duration;
		this.maxSitesListed = Math.max(0, maxSitesListed);
		this.format = format == null ? new BroadcastFormat("", "", "") : format;
	}

	public VoteBroadcastType getType() {
		return type;
	}

	public ParsedDuration getDuration() {
		return duration;
	}

	public int getMaxSitesListed() {
		return maxSitesListed;
	}

	public BroadcastFormat getFormat() {
		return format;
	}

	public boolean isDisabled() {
		return type == VoteBroadcastType.NONE;
	}

	/**
	 * Loads VoteBroadcast settings from config.yml.
	 *
	 * Expected structure:
	 * VoteBroadcast:
	 *   Type: EVERY_VOTE
	 *   Duration: 2m
	 *   MaxSitesListed: 0
	 *   Format:
	 *     BroadcastMsg: '...'
	 *     Header: '...'
	 *     ListLine: '...'
	 */
	public static BroadcastSettings load(ConfigurationSection sec) {
		VoteBroadcastType type = VoteBroadcastType.parse(sec == null ? null : sec.getString("Type"), VoteBroadcastType.NONE);

		ParsedDuration duration = ParsedDuration.parse(sec == null ? "2m" : sec.getString("Duration", "2m"));

		int maxSites = sec == null ? 0 : sec.getInt("MaxSitesListed", 0);

		ConfigurationSection fmt = sec == null ? null : sec.getConfigurationSection("Format");

		String broadcastMsg = fmt == null
				? "&6[Vote] &aThanks &e%player% &afor voting on &e%site%&a!"
				: fmt.getString("BroadcastMsg", "&6[Vote] &aThanks &e%player% &afor voting on &e%site%&a!");

		String header = fmt == null
				? "&6[Vote] &a%player% voted! &7(%sites_count%)"
				: fmt.getString("Header", "&6[Vote] &a%player% voted! &7(%sites_count%)");

		String listLine = fmt == null
				? " &7- &e%site%"
				: fmt.getString("ListLine", " &7- &e%site%");

		return new BroadcastSettings(type, duration, maxSites, new BroadcastFormat(broadcastMsg, header, listLine));
	}
}
