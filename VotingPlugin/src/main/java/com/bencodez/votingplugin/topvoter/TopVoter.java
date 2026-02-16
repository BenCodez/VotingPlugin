package com.bencodez.votingplugin.topvoter;

import java.util.ArrayList;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

/**
 * Enumeration for different top voter time periods.
 */
public enum TopVoter {
	/** All time top voters. */
	AllTime, 
	/** Monthly top voters. */
	Monthly, 
	/** Weekly top voters. */
	Weekly, 
	/** Daily top voters. */
	Daily;

	/**
	 * Gets the default top voter type from configuration.
	 *
	 * @return the default top voter type
	 */
	public static TopVoter getDefault() {
		TopVoter top = getTopVoter(VotingPluginMain.plugin.getConfigFile().getVoteTopDefault());
		if (top != null) {
			return top;
		}
		return AllTime;
	}

	/**
	 * Gets a top voter type by name.
	 *
	 * @param str the name string
	 * @return the top voter type
	 */
	public static TopVoter getTopVoter(String str) {
		for (TopVoter value : values()) {
			if (value.toString().equalsIgnoreCase(str)) {
				return value;
			}
		}
		return AllTime;
	}

	/**
	 * Converts a TimeType to a TopVoter.
	 *
	 * @param type the time type
	 * @return the corresponding top voter type
	 */
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

	/**
	 * Gets all top voter types except AllTime.
	 *
	 * @return array of top voter types
	 */
	public static TopVoter[] valuesMinusAllTime() {
		return new TopVoter[] { TopVoter.Daily, TopVoter.Weekly, TopVoter.Monthly };
	}

	@Getter
	private ArrayList<TopVoter> switchItems = new ArrayList<>();

	/**
	 * Gets the database column name for this top voter type.
	 *
	 * @return the column name
	 */
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

	/**
	 * Gets the last period database column name for this top voter type.
	 *
	 * @return the last column name
	 */
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

	/**
	 * Gets the formatted name for this top voter type.
	 *
	 * @return the formatted name
	 */
	public String getName() {
		if (this.equals(TopVoter.Monthly)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterMonthly();
		}
		if (this.equals(TopVoter.Weekly)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterWeekly();
		}
		if (this.equals(TopVoter.Daily)) {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterDaily();
		} else {
			return VotingPluginMain.plugin.getConfigFile().getFormatTopVoterAllTime();
		}
	}

	/**
	 * Gets the next top voter type in the cycle.
	 *
	 * @return the next top voter type
	 */
	public TopVoter next() {
		ArrayList<TopVoter> list = new ArrayList<>();
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

	/**
	 * Gets the previous top voter type in the cycle.
	 *
	 * @return the previous top voter type
	 */
	public TopVoter prev() {
		ArrayList<TopVoter> list = new ArrayList<>();
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
}
