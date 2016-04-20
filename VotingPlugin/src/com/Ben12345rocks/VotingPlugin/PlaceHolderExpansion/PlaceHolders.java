package com.Ben12345rocks.VotingPlugin.PlaceHolderExpansion;

import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Objects.User;

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

	public String playerTotalVotes(Player player) {
		return Integer.toString(new User(player).getTotalVotes());
	}

	public String playerCanVote(Player player) {
		return Boolean.toString(new User(player).canVoteAll());
	}

}
