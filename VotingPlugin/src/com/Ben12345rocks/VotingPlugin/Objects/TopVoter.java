package com.Ben12345rocks.VotingPlugin.Objects;

import com.Ben12345rocks.VotingPlugin.Config.Config;

public enum TopVoter {
	AllTime,
	Monthly,
	Weekly,
	Daily;

	public static TopVoter getTopVoter(String str) {
		for (TopVoter value : values()) {
			if (value.toString().equalsIgnoreCase(str)) {
				return value;
			}
		}
		return null;
	}

	public static TopVoter getDefault() {
		TopVoter top = getTopVoter(Config.getInstance().getVoteTopDefault());
		if (top != null) {
			return top;
		}
		return AllTime;
	}

	private static TopVoter[] vals = values();

	public TopVoter next() {
		return vals[(this.ordinal() + 1) % vals.length];
	}
}
