package com.Ben12345rocks.VotingPlugin.Events;

import lombok.Getter;

public enum SpecialRewardType {
	ALLSITE, CUMMULATIVE, MILESTONE, VOTESTREAK, FIRSTVOTE;

	@Getter

	private int amount = -1;
	@Getter

	private String type = "";

	public SpecialRewardType setAmount(int num) {
		amount = num;
		return this;
	}

	public SpecialRewardType setType(String type) {
		this.type = type;
		return this;
	}

}
