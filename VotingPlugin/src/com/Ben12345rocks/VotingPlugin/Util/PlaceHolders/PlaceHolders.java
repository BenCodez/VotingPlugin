package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import java.util.ArrayList;
import java.util.Map.Entry;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;

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

	private ArrayList<PlaceHolder> placeholders = new ArrayList<PlaceHolder>();

	/**
	 * @return the placeholders
	 */
	public ArrayList<PlaceHolder> getPlaceholders() {
		return placeholders;
	}

	public void load() {
		placeholders.clear();

		// older placeholders, might be removed in the future
		placeholders.add(new PlaceHolder("total") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.Monthly));
			}
		});

		placeholders.add(new PlaceHolder("alltimetotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getTotal(TopVoter.AllTime));
			}
		});

		placeholders.add(new PlaceHolder("lastmonthtotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getLastMonthTotal());
			}
		});

		// end of older placeholders

		for (final TopVoter top : TopVoter.values()) {
			placeholders.add(new PlaceHolder("total_" + top.toString()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Integer.toString(user.getTotal(top));
				}
			});
		}

		placeholders.add(new PlaceHolder("BestDailyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestDailyTotal());
			}
		});

		placeholders.add(new PlaceHolder("BestWeeklyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestWeeklyTotal());
			}
		});

		placeholders.add(new PlaceHolder("BestMonthlyTotal") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getHighestMonthlyTotal());
			}
		});

		placeholders.add(new PlaceHolder("DailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getDayVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("WeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getWeekVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("MonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getMonthVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("BestDailyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestDayVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("BestWeeklyVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestWeekVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("BestMonthVoteStreak") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getBestMonthVoteStreak());
			}
		});

		placeholders.add(new PlaceHolder("Points") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Integer.toString(user.getPoints());
			}
		});

		placeholders.add(new PlaceHolder("CanVote") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				return Boolean.toString(user.canVoteAll());
			}
		});

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder("Next_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandNextInfo(user, voteSite);
				}
			});
		}

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			placeholders.add(new PlaceHolder("Last_" + voteSite.getKey()) {

				@Override
				public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
					return Commands.getInstance().voteCommandLastDuration(user, voteSite);
				}
			});
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
		});

		placeholders.add(new PlaceHolder("Top_All_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[3]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith());

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
		});

		placeholders.add(new PlaceHolder("Top_Month_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[3]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith());

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
		});

		placeholders.add(new PlaceHolder("Top_Week_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[3]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith());

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
		});

		placeholders.add(new PlaceHolder("Top_Daily_") {

			@Override
			public String placeholderRequest(OfflinePlayer p, User user, String identifier) {
				int num = 1;
				int number = Integer.parseInt(identifier.split("_")[3]);
				for (Entry<User, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
					if (num == number) {
						return entry.getKey().getPlayerName();
					}
					num++;
				}
				return "";
			}
		}.useStartsWith());

	}

	public String getPlaceHolder(OfflinePlayer p, String identifier) {
		identifier = StringUtils.getInstance().replaceJavascript(p, identifier);

		// vote party placeholders
		// non player based
		if (identifier.equalsIgnoreCase("VotePartyVotesNeeded")) {
			return Integer.toString(VoteParty.getInstance().getNeededVotes());
		} else if (identifier.equalsIgnoreCase("VotePartyVotesCurrent")) {
			return Integer.toString(VoteParty.getInstance().getTotalVotes());
		} else if (identifier.equalsIgnoreCase("VotePartyVotesRequired")) {
			return Integer.toString(VoteParty.getInstance().getVotesRequired());
		}

		User user = UserManager.getInstance().getVotingPluginUser(p);

		for (PlaceHolder placeholder : placeholders) {
			if (placeholder.isUseStartsWith()) {
				if (StringUtils.getInstance().startsWithIgnoreCase(placeholder.getIdentifier(), identifier)) {
					return placeholder.placeholderRequest(p, user, identifier);
				}
			} else {
				if (placeholder.getIdentifier().equalsIgnoreCase(identifier)) {
					return placeholder.placeholderRequest(p, user, identifier);
				}
			}
		}

		return identifier;
	}

	public String getPlaceHolder(Player p, String identifier) {
		identifier = StringUtils.getInstance().replaceJavascript(p, identifier);
		return getPlaceHolder((OfflinePlayer) p, identifier);
	}
}
