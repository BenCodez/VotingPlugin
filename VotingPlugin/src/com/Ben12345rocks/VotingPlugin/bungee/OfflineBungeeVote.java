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
	@Getter
	private boolean realVote;
	@Getter
	private String text;

	public OfflineBungeeVote(String playerName, String uuid, String service, long time, boolean realVote, String text) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
	}

}
