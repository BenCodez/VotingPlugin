package com.bencodez.votingplugin.proxy;

import java.util.UUID;

import lombok.Getter;
import lombok.Setter;

/**
 * Represents an offline bungee vote.
 */
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
	@Getter
	private UUID voteId;

	/**
	 * Constructor with UUID voteId.
	 * @param voteId the vote ID
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(UUID voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
		this.voteId = voteId;
	}
	
	/**
	 * Constructor with String voteId.
	 * @param voteId the vote ID as string
	 * @param playerName the player name
	 * @param uuid the player UUID
	 * @param service the vote service
	 * @param time the vote time
	 * @param realVote whether this is a real vote
	 * @param text additional text
	 */
	public OfflineBungeeVote(String voteId, String playerName, String uuid, String service, long time, boolean realVote,
			String text) {
		this.playerName = playerName;
		this.uuid = uuid;
		this.service = service;
		this.time = time;
		this.realVote = realVote;
		this.text = text;
		if (voteId != null && !voteId.isEmpty()) {
			this.voteId = UUID.fromString(voteId);
		} else {
			this.voteId = null;
		}
	}

	@Override
	public String toString() {
		return "VoteCache:" + playerName + "/" + uuid + "/" + service + "/" + time + "/" + realVote + "/" + text + "/"
				+ voteId;
	}

}
