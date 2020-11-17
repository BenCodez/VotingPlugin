package com.bencodez.votingplugin.events;

import lombok.Getter;

public enum SpecialRewardType {
	ALLSITE, CUMMULATIVE, FIRSTVOTE, MILESTONE, VOTESTREAK;

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
