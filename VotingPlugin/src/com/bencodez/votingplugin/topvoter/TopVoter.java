package com.bencodez.votingplugin.topvoter;

import java.util.ArrayList;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public enum TopVoter {
	AllTime, Monthly, Weekly, Daily;

	public static TopVoter getDefault() {
		TopVoter top = getTopVoter(VotingPluginMain.plugin.getConfigFile().getVoteTopDefault());
		if (top != null) {
			return top;
		}
		return AllTime;
	}

	public static TopVoter getTopVoter(String str) {
		for (TopVoter value : values()) {
			if (value.toString().equalsIgnoreCase(str)) {
				return value;
			}
		}
		return AllTime;
	}

	public static TopVoter[] valuesMinusAllTime() {
		return new TopVoter[] { TopVoter.Daily, TopVoter.Weekly, TopVoter.Monthly };
	}

	public String getColumnName() {
		switch (this) {
		case AllTime:
			return "AllTimeTotal";
		case Daily:
			return "DailyTotal";
		case Monthly:
			return "MonthTotal";
		case Weekly:
			return "WeeklyTotal";
		default:
			return null;

		}
	}

	public String getLastColumnName() {
		switch (this) {
		case AllTime:
			return null;
		case Daily:
			return "LastDailyTotal";
		case Monthly:
			return "LastMonthTotal";
		case Weekly:
			return "LastWeeklyTotal";
		default:
			return null;

		}
	}

	@Getter
	private ArrayList<TopVoter> switchItems = new ArrayList<TopVoter>();

	public String getName() {
		if (this.equals(TopVoter.Monthly)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterMonthly();
		} else if (this.equals(TopVoter.Weekly)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterWeekly();
		} else if (this.equals(TopVoter.Daily)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterDaily();
		} else {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterAllTime();
		}
	}

	public TopVoter next() {
		ArrayList<TopVoter> list = new ArrayList<TopVoter>();
		if (switchItems != null && !switchItems.isEmpty()) {
			list.addAll(switchItems);
		} else {
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterAllTime()) {
				list.add(TopVoter.AllTime);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterMonthly()) {
				list.add(TopVoter.Monthly);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterWeekly()) {
				list.add(TopVoter.Weekly);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterDaily()) {
				list.add(TopVoter.Daily);
			}
		}

		for (int i = 0; i < list.size(); i++) {
			if (list.get(i).equals(this)) {
				int next = i + 1;
				if (next > list.size() - 1) {
					next = 0;
				}
				return list.get(next);
			}
		}
		return TopVoter.AllTime;
	}

	public TopVoter prev() {
		ArrayList<TopVoter> list = new ArrayList<TopVoter>();
		if (switchItems != null && !switchItems.isEmpty()) {
			list.addAll(switchItems);
		} else {
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterAllTime()) {
				list.add(TopVoter.AllTime);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterMonthly()) {
				list.add(TopVoter.Monthly);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterWeekly()) {
				list.add(TopVoter.Weekly);
			}
			if (VotingPluginMain.plugin.getConfigFile().isLoadTopVoterDaily()) {
				list.add(TopVoter.Daily);
			}
		}
		for (int i = list.size() - 1; i >= 0; i--) {
			if (list.get(i).equals(this)) {
				int prev = i - 1;
				if (prev < 0) {
					prev = list.size() - 1;
				}
				return list.get(prev);
			}
		}
		return TopVoter.AllTime;
	}

	public static TopVoter of(TimeType type) {
		switch (type) {
		case DAY:
			return Daily;
		case MONTH:
			return Monthly;
		case WEEK:
			return Weekly;
		default:
			return null;
		}
	}
}
