package com.bencodez.votingplugin.topvoter;

import java.util.ArrayList;

import com.bencodez.votingplugin.VotingPluginMain;

public enum TopVoter {
	AllTime, Daily, Monthly, Weekly;

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
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterAllTime()) {
			list.add(TopVoter.AllTime);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterMonthly()) {
			list.add(TopVoter.Monthly);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterWeekly()) {
			list.add(TopVoter.Weekly);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterDaily()) {
			list.add(TopVoter.Daily);
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
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterAllTime()) {
			list.add(TopVoter.AllTime);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterMonthly()) {
			list.add(TopVoter.Monthly);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterWeekly()) {
			list.add(TopVoter.Weekly);
		}
		if (VotingPluginMain.plugin.getConfigFile().getLoadTopVoterDaily()) {
			list.add(TopVoter.Daily);
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
}
