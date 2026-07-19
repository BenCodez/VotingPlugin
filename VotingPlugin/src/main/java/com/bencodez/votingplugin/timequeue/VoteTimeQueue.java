package com.bencodez.votingplugin.timequeue;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

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

	public VoteTimeQueue(String name, String service, long time) {
		this(null, name, service, time);
	}

	public VoteTimeQueue(UUID voteId, String name, String service, long time) {
		this.voteId = voteId;
		this.name = name;
		this.service = service;
		this.time = time;
	}
}
