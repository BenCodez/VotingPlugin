package com.Ben12345rocks.VotingPlugin.Objects;

import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.ComponentBuilder;
import net.md_5.bungee.api.chat.HoverEvent;
import net.md_5.bungee.api.chat.TextComponent;

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
			String[] cmdArgs = args[i].split("&");
			for (String cmdArg : cmdArgs) {
				if (cmdArg.equalsIgnoreCase("player")
						|| cmdArg.equalsIgnoreCase("SITENAME")
						|| cmdArg.equalsIgnoreCase("number")
						|| cmdArg.equalsIgnoreCase("string")
						|| cmdArg.equalsIgnoreCase("boolean")
						|| cmdArg.equalsIgnoreCase("list")
						|| arg.equalsIgnoreCase(cmdArg)) {
					return true;
				}
			}
			/*
			 * if (args[i].split("|").length <= 1) { if
			 * (args[i].equalsIgnoreCase("player") ||
			 * args[i].equalsIgnoreCase("SITENAME") ||
			 * args[i].equalsIgnoreCase("number") ||
			 * args[i].equalsIgnoreCase("string") ||
			 * args[i].equalsIgnoreCase("boolean") ||
			 * args[i].equalsIgnoreCase("list") ||
			 * arg.equalsIgnoreCase(args[i])) { return true; } } else {
			 *
			 * }
			 */
			return false;
		}
		return false;
	}

	public abstract void execute(CommandSender sender, String[] args);

	public String[] getArgs() {
		return args;
	}

	public TextComponent getHelpLine(String command) {
		String line = ConfigFormat.getInstance().getCommandsVoteHelpLine();

		String commandText = getHelpLineCommand(command);
		line = line.replace("%Command%", commandText);
		if (getHelpMessage() != "") {
			line = line.replace("%HelpMessage%", getHelpMessage());
		}
		TextComponent txt = Utils.getInstance().stringToComp(line);
		txt.setClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND,
				commandText));
		txt.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
				new ComponentBuilder(getHelpMessage()).color(ChatColor.AQUA)
				.create()));
		return txt;

	}

	public String getHelpLineCommand(String command) {
		String commandText = command;
		for (String arg1 : args) {
			int count = 1;
			for (String arg : arg1.split("&")) {
				if (count == 1) {
					if (arg.equalsIgnoreCase("player")) {
						commandText += " (Player)";
					} else if (arg.equalsIgnoreCase("sitename")) {
						commandText += " (SiteName)";
					} else if (arg.equalsIgnoreCase("boolean")) {
						commandText += " (True/False)";
					} else if (arg.equalsIgnoreCase("number")) {
						commandText += " (Number)";
					} else if (arg.equalsIgnoreCase("string")) {
						commandText += " (Text)";
					} else {
						commandText += " " + arg;
					}
				} else {
					if (arg.equalsIgnoreCase("player")) {
						commandText += "/(Player)";
					} else if (arg.equalsIgnoreCase("sitename")) {
						commandText += "/(SiteName)";
					} else if (arg.equalsIgnoreCase("boolean")) {
						commandText += "/(True/False)";
					} else if (arg.equalsIgnoreCase("number")) {
						commandText += "/(Number)";
					} else if (arg.equalsIgnoreCase("string")) {
						commandText += "/(Text)";
					} else {
						commandText += "/" + arg;
					}
				}
				count++;
			}
		}
		return commandText;
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
