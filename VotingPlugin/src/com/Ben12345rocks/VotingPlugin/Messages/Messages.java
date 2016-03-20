package com.Ben12345rocks.VotingPlugin.Messages;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

public class Messages {

	private Messages() {
	}

	static Messages instance = new Messages();

	public static Messages getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public Messages(Main plugin) {
		Messages.plugin = plugin;
	}

	public String noPerms() {
		return Utils.getInstance().colorize(
				"&cYou do not have enough permission for that command!");
	}

	public String mustBePlayer() {
		return Utils.getInstance().colorize(
				"You must be a player to use this command!");
	}

}
