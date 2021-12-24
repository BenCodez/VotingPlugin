package com.bencodez.votingplugin.bungee;

import lombok.Getter;
import lombok.Setter;

public abstract class TimeHandle {
	@Getter
	@Setter
	private String month;
	@Getter
	@Setter
	private int day;
	@Getter
	@Setter
	private int week;

	public TimeHandle(String month, int day, int week) {
		this.month = month;
		this.day = day;
		this.week = week;
	}

	public abstract void save();
}
