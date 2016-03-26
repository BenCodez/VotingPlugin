package com.Ben12345rocks.VotingPlugin.Messages;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;

public class Messages {

	static Messages instance = new Messages();

	static Main plugin = Main.plugin;

	public static Messages getInstance() {
		return instance;
	}

	private Messages() {
	}

	public Messages(Main plugin) {
		Messages.plugin = plugin;
	}

	public String invalidCommand() {
		return Utils.getInstance().colorize("&cInvalid command, see /v help");
	}

	public String mustBePlayer() {
		return Utils.getInstance().colorize(
				"You must be a player to use this command!");
	}

	public String noPerms() {
		return Utils.getInstance().colorize(
				"&cYou do not have enough permission for that command!");
	}

}
