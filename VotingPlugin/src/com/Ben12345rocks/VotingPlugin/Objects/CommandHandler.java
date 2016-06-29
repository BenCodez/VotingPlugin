package com.Ben12345rocks.VotingPlugin.Objects;

import org.bukkit.command.CommandSender;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;

public abstract class CommandHandler {
	static Main plugin = Main.plugin;
	private String[] args;
	private String perm;
	private String helpMessage;

	public CommandHandler(String[] args, String perm) {
		this.args = args;
		this.perm = perm;
		helpMessage = "Unknown Help Message";
	}

	public CommandHandler(String[] args, String perm, String helpMessage) {
		this.args = args;
		this.perm = perm;
		this.helpMessage = helpMessage;
	}

	public boolean argsMatch(String arg, int i) {
		if (i < args.length) {
			if (args[i].split("|").length <= 1) {
				if (args[i].equalsIgnoreCase("player")
						|| args[i].equalsIgnoreCase("SITENAME")
						|| args[i].equalsIgnoreCase("number")
						|| args[i].equalsIgnoreCase("string")
						|| args[i].equalsIgnoreCase("boolean")
						|| args[i].equalsIgnoreCase("list")
						|| arg.equalsIgnoreCase(args[i])) {
					return true;
				}
			} else {
				for (int j = 0; j < args[j].split("|").length; j++) {
					if (args[i].split("|")[j].equalsIgnoreCase("player")
							|| args[i].split("|")[j]
									.equalsIgnoreCase("SITENAME")
							|| args[i].split("|")[j].equalsIgnoreCase("number")
							|| args[i].split("|")[j].equalsIgnoreCase("string")
							|| args[i].split("|")[j]
									.equalsIgnoreCase("boolean")
							|| args[i].split("|")[j].equalsIgnoreCase("list")
							|| arg.equalsIgnoreCase(args[i].split("|")[j])) {
						return true;
					}
				}
			}
			return false;
		}
		return false;
	}

	public abstract void execute(CommandSender sender, String[] args);

	public String[] getArgs() {
		return args;
	}

	public String getHelpMessage() {
		return helpMessage;
	}

	public String getHelpLine(String command) {
		String line = command;
		for (String arg : args) {
			if (arg.equalsIgnoreCase("player")) {
				line += " (Player)";
			} else if (arg.equalsIgnoreCase("sitename")) {
				line += " (SiteName)";
			} else if (arg.equalsIgnoreCase("boolean")) {
				line += " (True/False)";
			} else if (arg.equalsIgnoreCase("number")) {
				line += " (Number)";
			} else {
				line += " " + arg;
			}
		}
		if (getHelpMessage() != "") {
			line += " - " + getHelpMessage();
		}
		return line;

	}

	public String getPerm() {
		return perm;
	}

	public boolean runCommand(CommandSender sender, String[] args) {
		if (args.length >= this.args.length) {
			for (int i = 0; i < args.length; i++) {
				if (!argsMatch(args[i], i)) {
					return false;
				}
				if (this.args[i].equalsIgnoreCase("number")) {
					if (!Utils.getInstance().isInt(args[i])) {
						sender.sendMessage(Utils.getInstance().colorize(
								ConfigFormat.getInstance().getNotNumber()
										.replace("%arg%", args[i])));
						return true;
					}
				}
			}

			if (perm != "") {
				if (!sender.hasPermission(perm)) {
					sender.sendMessage(Utils.getInstance().colorize(
							ConfigFormat.getInstance().getNoPerms()));
					return true;
				}
			}

			execute(sender, args);
			return true;
		}
		return false;
	}

	public void setHelpMessage(String helpMessage) {
		this.helpMessage = helpMessage;
	}

	public void setPerm(String perm) {
		this.perm = perm;
	}

}
