package com.bencodez.votingplugin.events;

import lombok.Getter;

/**
 * Enumeration for different special reward types.
 */
public enum SpecialRewardType {
	/** @deprecated All site reward. */
	@Deprecated
	ALLSITE, 
	/** @deprecated Cumulative reward. */
	@Deprecated
	CUMMULATIVE, 
	/** @deprecated First vote reward. */
	@Deprecated
	FIRSTVOTE, 
	/** @deprecated First vote today reward. */
	@Deprecated
	FIRSTVOTETODAY, 
	/** @deprecated Milestone reward. */
	@Deprecated
	MILESTONE, 
	/** Vote streak reward. */
	VOTESTREAK, 
	/** @deprecated Almost all sites reward. */
	@Deprecated
	ALMOSTALLSITES, 
	/** Top voter reward. */
	TOPVOTER, 
	/** Vote streaks reward. */
	VOTESTREAKS;

	@Getter
	private int amount = -1;

	@Getter
	private String type = "";

	/**
	 * Sets the amount for this special reward.
	 *
	 * @param num the amount
	 * @return this special reward type
	 */
	public SpecialRewardType setAmount(int num) {
		amount = num;
		return this;
	}

	/**
	 * Sets the type for this special reward.
	 *
	 * @param type the type
	 * @return this special reward type
	 */
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
