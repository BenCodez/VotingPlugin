package com.Ben12345rocks.VotingPlugin.Objects;

import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;

public abstract class CommandHandler {
	static Main plugin = Main.plugin;
	private String[] args;

	public CommandHandler(String[] args) {
		this.args = args;
	}

	public boolean runCommand(CommandSender sender, String[] args) {
		if (args.length == this.args.length) {
			for (int i = 0; i < args.length; i++) {
				if (!argsMatch(args[i], i)) {
					return false;
				}
			}

			execute(sender, args);
			return true;
		}
		return true;
	}

	public abstract void execute(CommandSender sender, String[] args);

	private boolean argsMatch(String arg, int i) {
		if (arg.equalsIgnoreCase("PLAYER") || arg.equalsIgnoreCase("SITENAME")
				|| arg.equalsIgnoreCase("number")
				|| arg.equalsIgnoreCase(args[i])) {
			return true;
		}
		return false;
	}

}
