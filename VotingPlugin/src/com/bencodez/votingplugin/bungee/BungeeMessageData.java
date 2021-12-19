package com.bencodez.votingplugin.bungee;

import java.util.regex.Pattern;

import lombok.Getter;

public class BungeeMessageData {

	// Message string
	// alltimetotal//monthtotal//weeklytotal//dailytotal//points//milestonecount//votepartycurrent//votepartyrequired

	@Getter
	private int allTimeTotal = 0;
	@Getter
	private int dailyTotal = 0;
	@Getter
	private int milestoneCount = 0;
	@Getter
	private int monthTotal = 0;
	@Getter
	private int points = 0;
	@Getter
	private int weeklyTotal = 0;
	@Getter
	private int votePartyCurrent = 0;
	@Getter
	private int votePartyRequired = 0;

	public BungeeMessageData(int allTimeTotal, int monthTotal, int weeklyTotal, int dailyTotal, int points,
			int milestoneCount, int votePartyCurrent, int votePartyRequired) {
		this.allTimeTotal = allTimeTotal;
		this.monthTotal = monthTotal;
		this.weeklyTotal = weeklyTotal;
		this.dailyTotal = dailyTotal;
		this.points = points;
		this.milestoneCount = milestoneCount;
		this.votePartyCurrent = votePartyCurrent;
		this.votePartyRequired = votePartyRequired;
	}

	public BungeeMessageData(String str) {
		String[] data = str.split(Pattern.quote("//"));
		if (data.length >= 6) {
			allTimeTotal = Integer.parseInt(data[0]);
			monthTotal = Integer.parseInt(data[1]);
			weeklyTotal = Integer.parseInt(data[2]);
			dailyTotal = Integer.parseInt(data[3]);
			points = Integer.parseInt(data[4]);
			milestoneCount = Integer.parseInt(data[5]);
		}
		if (data.length >= 8) {
			votePartyCurrent = Integer.parseInt(data[6]);
			votePartyRequired = Integer.parseInt(data[7]);
		}
	}

	@Override
	public String toString() {
		return allTimeTotal + "//" + monthTotal + "//" + weeklyTotal + "//" + dailyTotal + "//" + points + "//"
				+ milestoneCount + "//" + votePartyCurrent + "//" + votePartyRequired;
	}

}
