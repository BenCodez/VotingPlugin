package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import java.util.ArrayList;
import java.util.Map.Entry;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class PlaceHolders.
 */
public class PlaceHolders {

	/** The instance. */
	static PlaceHolders instance = new PlaceHolders();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of PlaceHolders.
	 *
	 * @return single instance of PlaceHolders
	 */
	public static PlaceHolders getInstance() {
		return instance;
	}

	@Getter
	private ArrayList<PlaceHolder> placeholders = new ArrayList<PlaceHolder>();

	@Getter
	private ArrayList<PlaceHolder> nonPlayerPlaceholders = new ArrayList<PlaceHolder>();

	/**
	 * Instantiates a new place holders.
	 */
	private PlaceHolders() {
	}

	/**
	 * Instantiates a new place holders.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public PlaceHolders(Main plugin) {
		PlaceHolders.plugin = plugin;
	}

	public String getPlaceHolder(OfflinePlayer p, String identifier) {
		identifier = StringUtils.getInstance().replaceJavascript(p, identifier);

		for (PlaceHolder placeholder : nonPlayerPlaceholders) {
			if (placeholder.matches(identifier)) {
				return placeholder.placeholderRequest(p, null, identifier);
			}
		}

		User user = UserManager.getInstance().getVotingPluginUser(p);

		for (PlaceHolder placeholder : placeholders) {
			if (placeholder.matches(identifier)) {
				return placeholder.placeholderRequest(p, user, identifier);
			}
		}

		return identifier;
	}

	public String getPlaceHolder(Player p, String identifier) {
		identifier = StringUtils.getInstance().replaceJavascript(p, identifier);
		return getPlaceHolder((OfflinePlayer) p, identifier);
	}

	public void load() {
		placeholders.clear();
		nonPlayerPlaceholders.clear();

		// older placeholders, might be removed in the future
		placeholders.add(new PlaceHolder("total") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.Monthly));
			}
		}.withDescription("Month total"));

		placeholders.add(new PlaceHolder("alltimetotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.AllTime));
			}
		}.withDescription("Alltime total"));

		placeholders.add(new PlaceHolder("lastmonthtotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getLastMonthTotal());
			}
		}.withDescription("Last month total"));

		// end of older placeholders

		placeholders.add(new PlaceHolder("DisableBroadcast") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return "" + user.getDisableBroadcast();
			}
		}.withDescription("Returns true/false if user has broadcast disabled"));

		placeholders.add(new PlaceHolder("MilestoneCount") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return "" + user.getMilestoneCount();
			}
		}.withDescription("User milestonecount"));

		for (final String identifier : Config.getInstance().getIdentifiers()) {
			if (Config.getInstance().getIdentifierLimit(identifier) > 0) {
				placeholders.add(new PlaceHolder("VoteShopLimit_" + identifier) {

					@Override
					public String placeholderRequest(OfflinePlayer p, User user, String ident) {
						return "" + user.getVoteShopIdentifierLimit(identifier);
					}
				}.withDescription("User voteshop limit for " + identifier));
			}
		}

		for (final TopVoter top : TopVoter.values()) {
			placeholders.add(new PlaceHolder("Total_" + top.toString()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Integer.toString(user.getTotal(top));
				}
			}.withDescription("User total for " + top.getName()));
		}

		placeholders.add(new PlaceHolder("BestDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestDailyTotal());
			}
		}.withDescription("Best daily total"));

		placeholders.add(new PlaceHolder("BestWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestWeeklyTotal());
			}
		}.withDescription("Best weekly total"));

		placeholders.add(new PlaceHolder("BestMonthlyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestMonthlyTotal());
			}
		}.withDescription("Best monthly total"));

		placeholders.add(new PlaceHolder("DailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getDayVoteStreak());
			}
		}.withDescription("Current daily votestreak"));

		placeholders.add(new PlaceHolder("WeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getWeekVoteStreak());
			}
		}.withDescription("Current weekly votestreak"));

		placeholders.add(new PlaceHolder("MonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getMonthVoteStreak());
			}
		}.withDescription("Current month votestreak"));

		placeholders.add(new PlaceHolder("BestDailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestDayVoteStreak());
			}
		}.withDescription("Best daily votestreak"));

		placeholders.add(new PlaceHolder("BestWeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestWeekVoteStreak());
			}
		}.withDescription("Best weekly votestreak"));

		placeholders.add(new PlaceHolder("BestMonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestMonthVoteStreak());
			}
		}.withDescription("Best month votestreak"));

		placeholders.add(new PlaceHolder("Points") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getPoints());
			}
		}.withDescription("User points"));

		placeholders.add(new PlaceHolder("CanVote") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Boolean.toString(user.canVoteAll());
			}
		}.withDescription("Return true/false if player can vote on all sites"));

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandNextInfo(user, voteSite);
				}
			}.withDescription("How long until user can vote on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder("Last_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandLastDuration(user, voteSite);
				}
			}.withDescription("How long ago user voted on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder("CanVote_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return "" + user.canVoteSite(voteSite);
				}
			}.withDescription("Whether or not player can vote on " + voteSite.getKey()));
		}

		placeholders.add(new PlaceHolder("Top_All_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder("Top_AllVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return "" + entry.getKey().getTotal(TopVoter.AllTime);
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder("Top_All_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder("Top_Month_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder("Top_Month_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder("Top_MonthVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return "" + entry.getKey().getTotal(TopVoter.Monthly);
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder("Top_Week_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder("Top_Week_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder("Top_WeekVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return "" + entry.getKey().getTotal(TopVoter.Weekly);
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		placeholders.add(new PlaceHolder("Top_Daily_Position") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
						return "" + num;
					}
					num++;
				}
				return "";
			}
		}.withDescription("Get user top voter position"));

		placeholders.add(new PlaceHolder("Top_Daily_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user at postion in top voter"));

		placeholders.add(new PlaceHolder("Top_DailyVotes_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[2]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return "" + entry.getKey().getTotal(TopVoter.Daily);
					}
					num++;
				}
				return "";
			}
		}.useStartsWith().withDescription("Get user votes at position in top voter"));

		// non players

		nonPlayerPlaceholders.add(new PlaceHolder("VotePartyVotesCurrent") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getTotalVotes());
			}
		}.withDescription("Current amount of voteparty votes"));

		nonPlayerPlaceholders.add(new PlaceHolder("VotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getNeededVotes());
			}
		}.withDescription("Voteparty votes needed"));

		nonPlayerPlaceholders.add(new PlaceHolder("VotePartyVotesRequired") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getVotesRequired());
			}
		}.withDescription("Amount of votes needed for voteparty"));
	}
}
