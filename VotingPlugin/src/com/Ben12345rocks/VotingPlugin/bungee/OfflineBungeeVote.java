package com.Ben12345rocks.VotingPlugin.bungee;

import lombok.Getter;

public class OfflineBungeeVote {

	@Getter
	private String playerName;
	@Getter
	private String uuid;
	@Getter
	private String service;
	@Getter
	private long time;

	public OfflineBungeeVote(String playerName, String uuid, String service, long time) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
	}

}
