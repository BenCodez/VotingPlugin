package com.bencodez.votingplugin.proxy;

import java.util.UUID;
import java.util.regex.Pattern;

import lombok.Getter;
import lombok.Setter;

public class BungeeMessageData {

	// Message string
	// alltimetotal//monthtotal//weeklytotal//dailytotal//points//milestonecount-REMOVED//votepartycurrent//votepartyrequired//datemonthtotal

	@Getter
	@Setter
	private int allTimeTotal = 0;
	@Getter
	@Setter
	private int dailyTotal = 0;

	private int milestoneCount = 0;
	@Getter
	@Setter
	private int monthTotal = 0;
	@Getter
	@Setter
	private int dateMonthTotal = 0;
	@Getter
	@Setter
	private int points = 0;
	@Getter
	@Setter
	private int weeklyTotal = 0;
	@Getter
	@Setter
	private int votePartyCurrent = 0;
	@Getter
	@Setter
	private int votePartyRequired = 0;

	@Getter
	@Setter
	private UUID voteUUID;

	public BungeeMessageData(int allTimeTotal, int monthTotal, int weeklyTotal, int dailyTotal, int points,
			int votePartyCurrent, int votePartyRequired, int dateMonthTotal, UUID voteUUID) {
		this.allTimeTotal = allTimeTotal;
		this.monthTotal = monthTotal;
		this.weeklyTotal = weeklyTotal;
		this.dailyTotal = dailyTotal;
		this.points = points;
		this.votePartyCurrent = votePartyCurrent;
		this.votePartyRequired = votePartyRequired;
		this.dateMonthTotal = dateMonthTotal;
		this.voteUUID = voteUUID;
	}

	public BungeeMessageData(String str) {
		String[] data = str.split(Pattern.quote("//"));
		if (data.length >= 6) {
			allTimeTotal = Integer.parseInt(data[0]);
			monthTotal = Integer.parseInt(data[1]);
			weeklyTotal = Integer.parseInt(data[2]);
			dailyTotal = Integer.parseInt(data[3]);
			points = Integer.parseInt(data[4]);
		}
		if (data.length >= 8) {
			votePartyCurrent = Integer.parseInt(data[6]);
			votePartyRequired = Integer.parseInt(data[7]);
		}
		if (data.length >= 9) {
			dateMonthTotal = Integer.parseInt(data[8]);
		}
		if (data.length >= 10) {
			voteUUID = UUID.fromString(data[9]);
		}
	}

	@Override
	public String toString() {
		return allTimeTotal + "//" + monthTotal + "//" + weeklyTotal + "//" + dailyTotal + "//" + points + "//"
				+ milestoneCount + "//" + votePartyCurrent + "//" + votePartyRequired + "//" + dateMonthTotal + "//"
				+ voteUUID;
	}

}
