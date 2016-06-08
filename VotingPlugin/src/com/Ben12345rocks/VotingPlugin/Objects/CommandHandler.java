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
		helpMessage = "";
	}

	public CommandHandler(String[] args, String perm, String helpMessage) {
		this.args = args;
		this.perm = perm;
		this.helpMessage = helpMessage;
	}

	public boolean argsMatch(String arg, int i) {
		if (i < args.length) {
			if (args[i].equalsIgnoreCase("player")
					|| args[i].equalsIgnoreCase("SITENAME")
					|| args[i].equalsIgnoreCase("number")
					|| args[i].equalsIgnoreCase("string")
					|| args[i].equalsIgnoreCase("boolean")
					|| args[i].equalsIgnoreCase("list")
					|| arg.equalsIgnoreCase(args[i])) {
				return true;
			}
			return false;
		}
		return false;
	}

	public String getHelpLine(String command) {
		String msg = command;
		for (int i = 0; i < args.length; i++) {
			String arg = args[i];
			arg = Utils.getInstance().replaceIgnoreCase(arg, "player",
					"{Player}");
			arg = Utils.getInstance().replaceIgnoreCase(arg, "sitename",
					"{SiteName}");
			arg = Utils.getInstance().replaceIgnoreCase(arg, "number",
					"{Number}");
			arg = Utils.getInstance()
					.replaceIgnoreCase(arg, "string", "{Text}");
			arg = Utils.getInstance().replaceIgnoreCase(arg, "boolean",
					"{True/False}");
			arg = Utils.getInstance().replaceIgnoreCase(arg, "list", "{Text}");
			msg += " " + arg;
		}

		msg += " - " + helpMessage;

		return msg;
	}

	public abstract void execute(CommandSender sender, String[] args);

	public String[] getArgs() {
		return args;
	}

	public String getHelpMessage() {
		return helpMessage;
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
