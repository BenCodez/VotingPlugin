package com.Ben12345rocks.VotingPlugin.Events;

public enum SpecialRewardType {
	ALLSITE, CUMMULATIVE, MILESTONE, VOTESTREAK, FIRSTVOTE;

	private int amount = -1;
	private String type = "";

	/**
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public SpecialRewardType setType(String type) {
		this.type = type;
		return this;
	}

	public SpecialRewardType setAmount(int num) {
		amount = num;
		return this;
	}

	/**
	 * @return the amount
	 */
	public int getAmount() {
		return amount;
	}

}
