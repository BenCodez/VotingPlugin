package com.bencodez.votingplugin.timequeue;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents a vote delayed while a proxy time change is active.
 */
public class VoteTimeQueue {
	@Getter
	@Setter
	private String name;
	@Getter
	@Setter
	private String service;
	@Getter
	@Setter
	private long time;
	@Getter
	@Setter
	private UUID voteId;

	/**
	 * Creates a legacy-compatible queued vote without an identifier.
	 *
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 */
	public VoteTimeQueue(String name, String service, long time) {
		this(null, name, service, time);
	}

	/**
	 * Creates a queued vote with its original identifier.
	 *
	 * @param voteId unique vote identifier
	 * @param name player name
	 * @param service service site
	 * @param time vote timestamp
	 */
	public VoteTimeQueue(UUID voteId, String name, String service, long time) {
		this.voteId = voteId;
		this.name = name;
		this.service = service;
		this.time = time;
	}
}
