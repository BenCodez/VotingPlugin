package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.util.Locale;

import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Defines what numeric value a VoteMilestone evaluates against.
 */
public enum VoteMilestoneTotal {

	// ---- Vote totals (TopVoter-based) ----
	ALLTIME_VOTES {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			if (bungeeMessageData != null) {
				return bungeeMessageData.getAllTimeTotal();
			}
			return user.getTotal(TopVoter.AllTime);
		}
	},

	DAILY_VOTES {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			if (bungeeMessageData != null) {
				return bungeeMessageData.getDailyTotal();
			}
			return user.getTotal(TopVoter.Daily);
		}
	},

	WEEKLY_VOTES {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			if (bungeeMessageData != null) {
				return bungeeMessageData.getWeeklyTotal();
			}
			return user.getTotal(TopVoter.Weekly);
		}
	},

	MONTHLY_VOTES {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			if (bungeeMessageData != null) {
				return bungeeMessageData.getMonthTotal();
			}
			return user.getTotal(TopVoter.Monthly);
		}
	},

	// ---- Global points (single value only) ----
	POINTS {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			if (bungeeMessageData != null) {
				return bungeeMessageData.getPoints();
			}
			return user.getPoints();
		}
	},
	ALLSITES_TODAY {
		@Override
		public long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData) {
			return user.getUniqueVoteSitesToday();
		}
	};

	public abstract long getValue(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData);

	/**
	 * Parse config values flexibly.
	 *
	 * Supported examples: - ALLTIME_VOTES - AllTime - ALLTIME - Daily / Weekly /
	 * Monthly - Points / VotePoints - Legacy_Milestone_Count
	 */
	public static VoteMilestoneTotal parse(String input) {
		if (input == null || input.trim().isEmpty()) {
			return ALLTIME_VOTES;
		}

		String raw = input.trim();
		String key = raw.toUpperCase(Locale.ROOT);

		// 1) Exact enum match (fast path)
		try {
			return VoteMilestoneTotal.valueOf(key);
		} catch (IllegalArgumentException ignored) {
		}

		// 2) Friendly / legacy aliases
		switch (key) {
		// Votes (AllTime)
		case "ALLTIME":
		case "ALL_TIME":
		case "ALLTIMEVOTES":
		case "ALLTIMEVOTE":
		case "VOTES":
			return ALLTIME_VOTES;

		// Votes (Daily)
		case "DAILY":
		case "DAY":
		case "DAILYVOTES":
		case "DAILYVOTE":
			return DAILY_VOTES;

		// Votes (Weekly)
		case "WEEKLY":
		case "WEEK":
		case "WEEKLYVOTES":
		case "WEEKLYVOTE":
			return WEEKLY_VOTES;

		// Votes (Monthly)
		case "MONTHLY":
		case "MONTH":
		case "MONTHLYVOTES":
		case "MONTHLYVOTE":
			return MONTHLY_VOTES;

		// Points
		case "POINT":
		case "POINTS":
		case "VOTEPOINT":
		case "VOTEPOINTS":
			return POINTS;

		case "SITES_TODAY":
		case "ALLSITES_TODAY":
			return ALLSITES_TODAY;

		default:
			return ALLTIME_VOTES;
		}
	}
}
