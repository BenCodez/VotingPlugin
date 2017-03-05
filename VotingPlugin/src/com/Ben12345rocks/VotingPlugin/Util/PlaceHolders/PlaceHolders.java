package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import java.util.Map.Entry;

import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
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

	public synchronized String getPlaceHolder(Player p, String identifier) {
		// %VotingPlugin_total% - Total votes of all vote sites
		if (identifier.equalsIgnoreCase("total")) {
			return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getMonthTotal());
		} else if (identifier.equalsIgnoreCase("alltimetotal")) {
			return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getAllTimeTotal());
		}

		String[] args = identifier.split("_");
		if (args.length > 1 && args[0].equalsIgnoreCase("total")) {
			if (args[1].equalsIgnoreCase("all")) {
				return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getAllTimeTotal());
			} else if (args[1].equalsIgnoreCase("month")) {
				return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getMonthTotal());
			} else if (args[1].equalsIgnoreCase("week")) {
				return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getWeeklyTotal());
			} else if (args[1].equalsIgnoreCase("daily")) {
				return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getDailyTotal());
			}
		}

		// %VotingPlugin_points% - Total votes of all vote sites
		if (identifier.equalsIgnoreCase("points")) {
			return Integer.toString(UserManager.getInstance().getVotingPluginUser(p).getPoints());
		}

		// %VotingPlugin_VotePartyVotesNeeded - Number of votes needed until
		// vote party rewards
		if (identifier.equalsIgnoreCase("VotePartyVotesNeeded")) {
			return Integer.toString(VoteParty.getInstance().getNeededVotes());
		}

		// %VotingPlugin_canvote% - Whether or not a player can vote on all
		// sites
		if (identifier.equalsIgnoreCase("canvote")) {
			return Boolean.toString(UserManager.getInstance().getVotingPluginUser(p).canVoteAll());
		}

		// %VotingPlugin_next_SITENAME% - Next time you can vote for voteSite
		if (startsWithIgnoreCase(identifier, "next")) {
			if (identifier.split("_").length > 1) {
				return playerNextVote(p, identifier.split("_")[1]);
			} else {
				return "";
			}
		}

		// %VotingPlugin_last_SITENAME% - Next time you can vote for voteSite
		if (startsWithIgnoreCase(identifier, "last")) {
			if (identifier.split("_").length > 1) {
				return playerLastVote(p, identifier.split("_")[1]);
			}
		}
		if (args.length > 2) {
			if (args[0].equalsIgnoreCase("top")) {
				if (StringUtils.getInstance().isInt(args[2])) {
					int number = Integer.parseInt(args[2]);
					int num = 1;
					if (args[1].equalsIgnoreCase("all")) {
						for (Entry<User, Integer> entry : plugin.topVoterAllTime.entrySet()) {
							if (num == number) {
								return entry.getKey().getPlayerName();
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("month")) {
						for (Entry<User, Integer> entry : plugin.topVoterMonthly.entrySet()) {
							if (num == number) {
								return entry.getKey().getPlayerName();
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("week")) {
						for (Entry<User, Integer> entry : plugin.topVoterWeekly.entrySet()) {
							if (num == number) {
								return entry.getKey().getPlayerName();
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("daily")) {
						for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
							if (num == number) {
								return entry.getKey().getPlayerName();
							}
							num++;
						}
					}
				} else if (args[2].equalsIgnoreCase("Position")) {
					int num = 1;
					if (args[1].equalsIgnoreCase("all")) {
						for (Entry<User, Integer> entry : plugin.topVoterAllTime.entrySet()) {
							if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
								return "" + num;
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("month")) {
						for (Entry<User, Integer> entry : plugin.topVoterMonthly.entrySet()) {
							if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
								return "" + num;
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("week")) {
						for (Entry<User, Integer> entry : plugin.topVoterWeekly.entrySet()) {
							if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
								return "" + num;
							}
							num++;
						}
					} else if (args[1].equalsIgnoreCase("daily")) {
						for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
							if (entry.getKey().getUUID().equals(p.getUniqueId().toString())) {
								return "" + num;
							}
							num++;
						}
					}
				}
			}
		}
		return "";
	}

	/**
	 * Player last vote.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 * @return the string
	 */
	public String playerLastVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames().contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = UserManager.getInstance().getVotingPluginUser(player);
		return Commands.getInstance().voteCommandLastDate(user, voteSite);
	}

	/**
	 * Player next vote.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 * @return the string
	 */
	public String playerNextVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames().contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = UserManager.getInstance().getVotingPluginUser(player);
		return Commands.getInstance().voteCommandNextInfo(user, voteSite);
	}

	public boolean startsWithIgnoreCase(String str1, String str2) {
		return str1.toLowerCase().startsWith(str2.toLowerCase());
	}

}
