package com.bencodez.votingplugin.events;

import lombok.Getter;

public enum SpecialRewardType {
	ALLSITE, CUMMULATIVE, FIRSTVOTE, FIRSTVOTETODAY, MILESTONE, VOTESTREAK, ALMOSTALLSITES, TOPVOTER, VOTESTREAKS;

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

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(name());

		if (amount >= 0) {
			sb.append("(amount=").append(amount);
		}

		if (type != null && !type.isEmpty()) {
			if (amount >= 0) {
				sb.append(", ");
			} else {
				sb.append("(");
			}
			sb.append("type=").append(type).append(")");
		} else if (amount >= 0) {
			sb.append(")");
		}

		return sb.toString();
	}

}
