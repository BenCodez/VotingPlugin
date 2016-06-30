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

	public TextComponent getHelpLine(String command) {
		String line = ConfigFormat.getInstance().getCommandsVoteHelpLine();
		String commandText = command;
		for (String arg : args) {
			if (arg.equalsIgnoreCase("player")) {
				command += " (Player)";
			} else if (arg.equalsIgnoreCase("sitename")) {
				command += " (SiteName)";
			} else if (arg.equalsIgnoreCase("boolean")) {
				command += " (True/False)";
			} else if (arg.equalsIgnoreCase("number")) {
				command += " (Number)";
			} else {
				command += " " + arg;
			}
		}
		line.replace("%Command%", commandText);
		if (getHelpMessage() != "") {
			line.replace("%HelpMessage%", getHelpMessage());
		}
		TextComponent txt = Utils.getInstance().stringToComp(line);
		txt.setClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND,
				commandText));
		txt.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT,
				new ComponentBuilder(getHelpMessage()).color(ChatColor.AQUA)
						.create()));
		return txt;

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
