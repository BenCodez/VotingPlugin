package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Map.Entry;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Placeholder.PlaceHolder;
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
	private ArrayList<PlaceHolder<User>> placeholders = new ArrayList<PlaceHolder<User>>();

	@Getter
	private ArrayList<PlaceHolder<User>> nonPlayerPlaceholders = new ArrayList<PlaceHolder<User>>();

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
		if (Config.getInstance().isUseJavascriptPlaceholders()) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}

		for (PlaceHolder<User> placeholder : nonPlayerPlaceholders) {
			if (placeholder.matches(identifier)) {
				return placeholder.placeholderRequest(p, null, identifier);
			}
		}

		User user = UserManager.getInstance().getVotingPluginUser(p);

		for (PlaceHolder<User> placeholder : placeholders) {
			if (placeholder.matches(identifier)) {
				return placeholder.placeholderRequest(p, user, identifier);
			}
		}

		return "Not a valid placeholder";
	}

	public String getPlaceHolder(Player p, String identifier) {
		if (Config.getInstance().isUseJavascriptPlaceholders()) {
			identifier = StringParser.getInstance().replaceJavascript(p, identifier);
		}
		return getPlaceHolder((OfflinePlayer) p, identifier);
	}

	public void load() {
		placeholders.clear();
		nonPlayerPlaceholders.clear();

		// older placeholders, might be removed in the future
		placeholders.add(new PlaceHolder<User>("total") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.Monthly));
			}
		}.withDescription("Month total"));

		placeholders.add(new PlaceHolder<User>("alltimetotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.AllTime));
			}
		}.withDescription("Alltime total"));

		placeholders.add(new PlaceHolder<User>("lastmonthtotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getLastMonthTotal());
			}
		}.withDescription("Last month total"));

		// end of older placeholders

		placeholders.add(new PlaceHolder<User>("DisableBroadcast") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return "" + user.getDisableBroadcast();
			}
		}.withDescription("Returns true/false if user has broadcast disabled"));

		placeholders.add(new PlaceHolder<User>("MilestoneCount") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return "" + user.getMilestoneCount();
			}
		}.withDescription("User milestonecount"));

		for (final String identifier : Config.getInstance().getIdentifiers()) {
			if (Config.getInstance().getIdentifierLimit(identifier) > 0) {
				placeholders.add(new PlaceHolder<User>("VoteShopLimit_" + identifier) {

					@Override
					public String placeholderRequest(OfflinePlayer p, User user, String ident) {
						return "" + user.getVoteShopIdentifierLimit(identifier);
					}
				}.withDescription("User voteshop limit for " + identifier));
			}
		}

		for (final TopVoter top : TopVoter.values()) {
			placeholders.add(new PlaceHolder<User>("Total_" + top.toString()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Integer.toString(user.getTotal(top));
				}
			}.withDescription("User total for " + top.getName()));
		}

		placeholders.add(new PlaceHolder<User>("BestDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestDailyTotal());
			}
		}.withDescription("Best daily total"));

		placeholders.add(new PlaceHolder<User>("BestWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestWeeklyTotal());
			}
		}.withDescription("Best weekly total"));

		placeholders.add(new PlaceHolder<User>("BestMonthlyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestMonthlyTotal());
			}
		}.withDescription("Best monthly total"));

		placeholders.add(new PlaceHolder<User>("DailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getDayVoteStreak());
			}
		}.withDescription("Current daily votestreak"));

		placeholders.add(new PlaceHolder<User>("WeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getWeekVoteStreak());
			}
		}.withDescription("Current weekly votestreak"));

		placeholders.add(new PlaceHolder<User>("MonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getMonthVoteStreak());
			}
		}.withDescription("Current month votestreak"));

		placeholders.add(new PlaceHolder<User>("BestDailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestDayVoteStreak());
			}
		}.withDescription("Best daily votestreak"));

		placeholders.add(new PlaceHolder<User>("BestWeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestWeekVoteStreak());
			}
		}.withDescription("Best weekly votestreak"));

		placeholders.add(new PlaceHolder<User>("BestMonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestMonthVoteStreak());
			}
		}.withDescription("Best month votestreak"));

		placeholders.add(new PlaceHolder<User>("Points") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getPoints());
			}
		}.withDescription("User points"));

		placeholders.add(new PlaceHolder<User>("Points_Format") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				NumberFormat numberFormat = NumberFormat.getNumberInstance(Locale.US);
				return numberFormat.format(user.getPoints());
			}
		}.withDescription("User points"));

		placeholders.add(new PlaceHolder<User>("CanVote") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Boolean.toString(user.canVoteAll());
			}
		}.withDescription("Return true/false if player can vote on all sites"));

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder<User>("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandNextInfo(user, voteSite);
				}
			}.withDescription("How long until user can vote on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder<User>("Last_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandLastDuration(user, voteSite);
				}
			}.withDescription("How long ago user voted on " + voteSite.getKey()));
			placeholders.add(new PlaceHolder<User>("CanVote_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return "" + user.canVoteSite(voteSite);
				}
			}.withDescription("Whether or not player can vote on " + voteSite.getKey()));
		}

		placeholders.add(new PlaceHolder<User>("Top_All_Position") {

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

		placeholders.add(new PlaceHolder<User>("Top_AllVotes_") {

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

		placeholders.add(new PlaceHolder<User>("Top_All_") {

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

		placeholders.add(new PlaceHolder<User>("Top_Month_Position") {

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

		placeholders.add(new PlaceHolder<User>("Top_Month_") {

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
		}.useStartsWith().withDescription("Get user at position in top voter"));

		placeholders.add(new PlaceHolder<User>("Top_MonthVotes_") {

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

		placeholders.add(new PlaceHolder<User>("Top_Week_Position") {

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

		placeholders.add(new PlaceHolder<User>("Top_Week_") {

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

		placeholders.add(new PlaceHolder<User>("Top_WeekVotes_") {

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

		placeholders.add(new PlaceHolder<User>("Top_Daily_Position") {

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

		placeholders.add(new PlaceHolder<User>("Top_Daily_") {

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

		placeholders.add(new PlaceHolder<User>("Top_DailyVotes_") {

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

		nonPlayerPlaceholders.add(new PlaceHolder<User>("VotePartyVotesCurrent") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getTotalVotes());
			}
		}.withDescription("Current amount of voteparty votes"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("VotePartyVotesNeeded") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getNeededVotes());
			}
		}.withDescription("Voteparty votes needed"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("VotePartyVotesRequired") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(VoteParty.getInstance().getVotesRequired());
			}
		}.withDescription("Amount of votes needed for voteparty"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("GlobalMonthTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int total = 0;
				for (int num : Main.plugin.getTopVoter(TopVoter.Monthly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global month total"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("GlobalAllTimeTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int total = 0;
				for (int num : Main.plugin.getTopVoter(TopVoter.AllTime).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global alltime total"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("GlobalWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int total = 0;
				for (int num : Main.plugin.getTopVoter(TopVoter.Weekly).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global weekly total"));

		nonPlayerPlaceholders.add(new PlaceHolder<User>("GlobalDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int total = 0;
				for (int num : Main.plugin.getTopVoter(TopVoter.Daily).values()) {
					total += num;
				}
				return Integer.toString(total);
			}
		}.withDescription("Global daily total"));
	}
}
