package com.Ben12345rocks.VotingPlugin.PlaceHolderExpansion;

import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

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
	public String playerCanVote(Player player) {
		return Boolean.toString(new User(player).canVoteAll());

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
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
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
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
		return Commands.getInstance().voteCommandNextInfo(user, voteSite);
	}

	/**
	 * Player points.
	 *
	 * @param player
	 *            the player
	 * @return the string
	 */
	public String playerPoints(Player player) {
		return Integer.toString(new User(player).getPoints());
	}

	/**
	 * Player total votes.
	 *
	 * @param player
	 *            the player
	 * @return the string
	 */
	public String playerTotalVotes(Player player) {
		return Integer.toString(new User(player).getTotalVotes());
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
	public String playerTotalVotesSite(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
		return Integer.toString(user.getTotalVotesSite(voteSite));
	}

}
