package com.Ben12345rocks.VotingPlugin.Util.PlaceHolders;

import org.bukkit.entity.Player;

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

	/**
	 * Player can vote.
	 *
	 * @param player
	 *            the player
	 * @return the string
	 */
	public synchronized String playerCanVote(Player player) {
		return Boolean.toString(UserManager.getInstance()
				.getVotingPluginUser(player).canVoteAll());

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
	public synchronized String playerLastVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
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
	public synchronized String playerNextVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = UserManager.getInstance().getVotingPluginUser(player);
		return Commands.getInstance().voteCommandNextInfo(user, voteSite);
	}

	/**
	 * Player points.
	 *
	 * @param player
	 *            the player
	 * @return the string
	 */
	public synchronized String playerPoints(Player player) {
		return Integer.toString(UserManager.getInstance()
				.getVotingPluginUser(player).getPoints());
	}

	/**
	 * Player total votes.
	 *
	 * @param player
	 *            the player
	 * @return the string
	 */
	public synchronized String playerTotalVotes(Player player) {
		return Integer.toString(UserManager.getInstance()
				.getVotingPluginUser(player).getTotalVotes());
	}

	/**
	 * Player total votes site.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 * @return the string
	 */
	public synchronized String playerTotalVotesSite(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = UserManager.getInstance().getVotingPluginUser(player);
		return Integer.toString(user.getTotalVotesSite(voteSite));
	}

	/**
	 * Vote party votes needed.
	 *
	 * @return the string
	 */
	public synchronized String votePartyVotesNeeded() {
		return Integer.toString(VoteParty.getInstance().getNeededVotes());
	}

	public synchronized String getPlaceHolder(Player p, String identifier) {
		// %VotingPlugin_total% - Total votes of all vote sites
		if (identifier.equalsIgnoreCase("total")) {
			return playerTotalVotes(p);
		}

		// %VotingPlugin_points% - Total votes of all vote sites
		if (identifier.equalsIgnoreCase("points")) {
			return playerPoints(p);
		}

		// %VotingPlugin_VotePartyVotesNeeded - Number of votes needed until
		// vote party rewards
		if (identifier.equalsIgnoreCase("VotePartyVotesNeeded")) {
			return votePartyVotesNeeded();
		}

		// %VotingPlugin_canvote% - Whether or not a player can vote on all
		// sites
		if (identifier.equalsIgnoreCase("canvote")) {
			return playerCanVote(p);
		}

		// %VotingPlugin_total_SITENAME% - Total votes for site
		if (startsWithIgnoreCase(identifier, "total")) {
			if (identifier.split("_").length > 1) {
				return playerTotalVotesSite(p, identifier.split("_")[1]);
			} else {
				return "";
			}
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
			} else {
				return "";
			}
		}
		return "Invalid Placeholder";
	}

	public boolean startsWithIgnoreCase(String str1, String str2) {
		return str1.toLowerCase().startsWith(str2.toLowerCase());
	}

}
