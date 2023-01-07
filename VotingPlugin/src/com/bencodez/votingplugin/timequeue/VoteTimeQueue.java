package com.bencodez.votingplugin.timequeue;

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

	public VoteTimeQueue(String name, String service, long time) {
		this.name = name;
		this.service = service;
		this.time = time;
	}
}
