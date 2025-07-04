package com.bencodez.votingplugin.proxy;

import lombok.Getter;
import lombok.Setter;

public class OfflineBungeeVote {

	@Getter
	private String playerName;
	@Getter
	private boolean realVote;
	@Getter
	private String service;
	@Getter
	@Setter
	private String text;
	@Getter
	private long time;
	@Getter
	private String uuid;

	public OfflineBungeeVote(String playerName, String uuid, String service, long time, boolean realVote, String text) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
	}

	@Override
	public String toString() {
		return "VoteCache:" + playerName + "/" + uuid + "/" + service + "/" + time + "/" + realVote + "/" + text;
	}

}
