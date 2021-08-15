package com.bencodez.votingplugin.placeholders;

import java.text.NumberFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.placeholder.PlaceHolder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlaceHolders.
 */
public class PlaceHolders {

	@Getter
	private ArrayList<PlaceHolder<VotingPluginUser>> nonPlayerPlaceholders = new ArrayList<PlaceHolder<VotingPluginUser>>();

	@Getter
	private ArrayList<PlaceHolder<VotingPluginUser>> placeholders = new ArrayList<PlaceHolder<VotingPluginUser>>();

	private VotingPluginMain plugin;

	public PlaceHolders(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public String getPlaceHolder(OfflinePlayer p, String identifier) {
		return getPlaceHolder(p, identifier, true);
	}

	public String getPlaceHolder(OfflinePlayer p, String identifier, boolean javascript) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders() && javascript) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}

		for (PlaceHolder<VotingPluginUser> placeholder : nonPlayerPlaceholders) {
			if (placeholder.matches(identifier)) {
				return placeholder.placeholderRequest(p, null, identifier);
			}
		}

		VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(p);
		if (Bukkit.isPrimaryThread() || plugin.getConfigFile().isAlwaysWaitForCachePlaceholders()) {
			user.setWaitForCache(false);
			if (!user.isCached()) {
				user.loadCache();
				return "..";
			}
		}
		if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
			user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
		}

		for (PlaceHolder<VotingPluginUser> placeholder : placeholders) {
			try {
				if (placeholder.matches(identifier)) {
					return placeholder.placeholderRequest(p, user, identifier);
				}
			} catch (Exception e) {
				plugin.debug(e);
				return "...";
			}
		}

		return "Not a valid placeholder";
	}

	public String getPlaceHolder(Player p, String identifier) {
		if (plugin.getConfigFile().isUseJavascriptPlaceholders()) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}
		return getPlaceHolder((OfflinePlayer) p, identifier, false);
	}

	public void load() {
		placeholders.clear();
		nonPlayerPlaceholders.clear();

		// older placeholders, might be removed in the future
		placeholders.add(new PlaceHolder<VotingPluginUser>("total") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.Monthly));
			}
		}.withDescription("Month total"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("alltimetotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.AllTime));
			}
		}.withDescription("Alltime total"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("lastmonthtotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getLastMonthTotal());
			}
		}.withDescription("Last month total"));

		// end of older placeholders

		placeholders.add(new PlaceHolder<VotingPluginUser>("DisableBroadcast") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return "" + user.getDisableBroadcast();
			}
		}.withDescription("Returns true/false if user has broadcast disabled"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("MilestoneCount") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return "" + user.getMilestoneCount();
			}
		}.withDescription("User milestonecount"));

		for (final String identifier : plugin.getGui().getChestShopIdentifiers()) {
			if (plugin.getGui().getChestShopIdentifierLimit(identifier) > 0) {
				placeholders.add(new PlaceHolder<VotingPluginUser>("VoteShopLimit_" + identifier) {

					@Override
					public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String ident) {
						return "" + user.getVoteShopIdentifierLimit(identifier);
					}
				}.withDescription("User voteshop limit for " + identifier));
			}
		}

		for (final TopVoter top : TopVoter.values()) {
			placeholders.add(new PlaceHolder<VotingPluginUser>("Total_" + top.toString()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
					return Integer.toString(user.getTotal(top));
				}
			}.withDescription("User total for " + top.getName()));
		}

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestDailyTotal());
			}
		}.withDescription("Best daily total"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestWeeklyTotal());
			}
		}.withDescription("Best weekly total"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestMonthlyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getHighestMonthlyTotal());
			}
		}.withDescription("Best monthly total"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("DailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getDayVoteStreak());
			}
		}.withDescription("Current daily votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("WeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getWeekVoteStreak());
			}
		}.withDescription("Current weekly votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("MonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getMonthVoteStreak());
			}
		}.withDescription("Current month votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestDailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestDayVoteStreak());
			}
		}.withDescription("Best daily votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestWeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestWeekVoteStreak());
			}
		}.withDescription("Best weekly votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("BestMonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getBestMonthVoteStreak());
			}
		}.withDescription("Best month votestreak"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Points") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(user.getPoints());
			}
		}.withDescription("User points"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Points_Format") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				NumberFormat numberFormat = NumberFormat.getNumberInstance(Locale.US);
				return numberFormat.format(user.getPoints());
			}
		}.withDescription("User points"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("CanVote") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Boolean.toString(user.canVoteAll());
			}
		}.withDescription("Return true/false if player can vote on all sites"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("CanVoteSites") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return "" + user.getSitesNotVotedOn();
			}
		}.withDescription("Return number of votesites available"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Next_AnySite") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				if (user.canVoteAny()) {
					return plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				}
				long smallest = -1;
				HashMap<Long, VoteSite> times = new HashMap<Long, VoteSite>();
				for (VoteSite site : plugin.getVoteSites()) {
					long t = user.voteNextDurationTime(site);
					if (smallest == -1) {
						smallest = t;
					}
					if (t < smallest) {
						smallest = t;
					}
					times.put(t, site);
				}
				for (Entry<Long, VoteSite> entry : times.entrySet()) {
					if (entry.getKey().longValue() == smallest) {
						return user.voteCommandNextInfo(entry.getValue());
					}
				}

				return "Error";
			}
		}.withDescription("How long until user can vote on anysite"));

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder<VotingPluginUser>("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
					return user.voteCommandNextInfo(voteSite);
				}
			}.withDescription("How long until user can vote on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder<VotingPluginUser>("Last_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
					return user.voteCommandLastDuration(voteSite);
				}
			}.withDescription("How long ago user voted on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder<VotingPluginUser>("CanVote_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
					return "" + user.canVoteSite(voteSite);
				}
			}.withDescription("Whether or not player can vote on " + voteSite.getKey()));
		}

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_All_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (entry.getKey().getUuid().equals(p.getUniqueId())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_AllVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_All_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Month_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (entry.getKey().getUuid().equals(p.getUniqueId())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Month_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_MonthVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Week_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (entry.getKey().getUuid().equals(p.getUniqueId())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Week_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_WeekVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Daily_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (entry.getKey().getUuid().equals(p.getUniqueId())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_Daily_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("VotePartyContributedVotes") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return "" + user.getVotePartyVotes();
			}
		}.useStartsWith().withDescription("See vote party placeholders contributed"));

		placeholders.add(new PlaceHolder<VotingPluginUser>("Top_DailyVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return "" + entry.getValue().intValue();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		// non players

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("VotePartyVotesCurrent") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(plugin.getVoteParty().getTotalVotes());
			}
		}.withDescription("Current amount of voteparty votes"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("VotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(plugin.getVoteParty().getNeededVotes());
			}
		}.withDescription("Voteparty votes needed"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("VotePartyVotesRequired") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				return Integer.toString(plugin.getVoteParty().getVotesRequired());
			}
		}.withDescription("Amount of votes needed for voteparty"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("GlobalMonthTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Monthly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global month total"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("GlobalAllTimeTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.AllTime).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global alltime total"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("GlobalWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Weekly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global weekly total"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("GlobalDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				int total = 0;
				for (int num : VotingPluginMain.plugin.getTopVoter(TopVoter.Daily).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global daily total"));

		nonPlayerPlaceholders.add(new PlaceHolder<VotingPluginUser>("TimeUntilDayReset") {

			@Override
			public String placeholderRequest(OfflinePlayer p, VotingPluginUser user, String identifier) {
				LocalDateTime now = LocalDateTime.now();
				LocalDateTime offsetoclocktoday = plugin.getTimeChecker().getTime().withHour(0).withMinute(0);
				LocalDateTime offsetoclocktomorrow = plugin.getTimeChecker().getTime().plusDays(1).withHour(0)
						.withMinute(0);

				String timeMsg = "%hours% Hours %minutes% Minutes";
				Duration dur = null;
				if (!now.isBefore(offsetoclocktoday)) {
					dur = Duration.between(now, offsetoclocktomorrow);

				} else {
					dur = Duration.between(now, offsetoclocktoday);
				}
				int diffHours = (int) (dur.getSeconds() / (60 * 60));
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

				if (diffHours < 0) {
					diffHours = diffHours * -1;
				}
				if (diffHours >= 24) {
					diffHours = diffHours - 24;
				}
				if (diffMinutes < 0) {
					diffMinutes = diffMinutes * -1;
				}

				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
						Long.toString(diffMinutes));
				return timeMsg;

			}
		}.withDescription("Time until plugin time day changes"));
	}
}
