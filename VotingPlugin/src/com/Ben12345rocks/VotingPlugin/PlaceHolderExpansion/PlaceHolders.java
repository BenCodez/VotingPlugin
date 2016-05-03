package com.Ben12345rocks.VotingPlugin.PlaceHolderExpansion;

import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class PlaceHolders {

	static PlaceHolders instance = new PlaceHolders();

	static Main plugin = Main.plugin;

	public static PlaceHolders getInstance() {
		return instance;
	}

	private PlaceHolders() {
	}

	public PlaceHolders(Main plugin) {
		PlaceHolders.plugin = plugin;
	}

	public String playerCanVote(Player player) {
		return Boolean.toString(new User(player).canVoteAll());
	}

	public String playerTotalVotes(Player player) {
		return Integer.toString(new User(player).getTotalVotes());
	}

	public String playerTotalVotesSite(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
		return Integer.toString(user.getTotalVotesSite(voteSite));
	}

	public String playerNextVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
		return Commands.getInstance().voteCommandNextInfo(user, voteSite);
	}

	public String playerLastVote(Player player, String siteName) {
		if (!ConfigVoteSites.getInstance().getVoteSitesNames()
				.contains(siteName)) {
			return "";
		}

		VoteSite voteSite = plugin.getVoteSite(siteName);
		User user = new User(player);
		return Commands.getInstance().voteCommandLastDate(user, voteSite);
	}

}
