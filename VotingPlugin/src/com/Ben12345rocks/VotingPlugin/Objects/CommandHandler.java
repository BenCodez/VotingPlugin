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

// TODO: Auto-generated Javadoc
/**
 * The Class CommandHandler.
 */
public abstract class CommandHandler {
	
	/** The plugin. */
	static Main plugin = Main.plugin;
	
	/** The args. */
	private String[] args;
	
	/** The perm. */
	private String perm;
	
	/** The help message. */
	private String helpMessage;

	/**
	 * Instantiates a new command handler.
	 *
	 * @param args the args
	 * @param perm the perm
	 */
	public CommandHandler(String[] args, String perm) {
		this.args = args;
		this.perm = perm;
		helpMessage = "Unknown Help Message";
	}

	/**
	 * Instantiates a new command handler.
	 *
	 * @param args the args
	 * @param perm the perm
	 * @param helpMessage the help message
	 */
	public CommandHandler(String[] args, String perm, String helpMessage) {
		this.args = args;
		this.perm = perm;
		this.helpMessage = helpMessage;
	}

	/**
	 * Args match.
	 *
	 * @param arg the arg
	 * @param i the i
	 * @return true, if successful
	 */
	public boolean argsMatch(String arg, int i) {
		if (i < args.length) {
			String[] cmdArgs = args[i].split("&");
			for (String cmdArg : cmdArgs) {
				if (cmdArg.equalsIgnoreCase("(player)")
						|| cmdArg.equalsIgnoreCase("(SITENAME)")
						|| cmdArg.equalsIgnoreCase("(reward)")
						|| cmdArg.equalsIgnoreCase("(number)")
						|| cmdArg.equalsIgnoreCase("(string)")
						|| cmdArg.equalsIgnoreCase("(boolean)")
						|| cmdArg.equalsIgnoreCase("(list)")
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

	/**
	 * Execute.
	 *
	 * @param sender the sender
	 * @param args the args
	 */
	public abstract void execute(CommandSender sender, String[] args);

	/**
	 * Gets the args.
	 *
	 * @return the args
	 */
	public String[] getArgs() {
		return args;
	}

	/**
	 * Gets the help line.
	 *
	 * @param command the command
	 * @return the help line
	 */
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

	/**
	 * Gets the help line command.
	 *
	 * @param command the command
	 * @return the help line command
	 */
	public String getHelpLineCommand(String command) {
		String commandText = command;
		for (String arg1 : args) {
			int count = 1;
			for (String arg : arg1.split("&")) {
				if (count == 1) {
					if (arg.equalsIgnoreCase("(player)")) {
						commandText += " (Player)";
					} else if (arg.equalsIgnoreCase("(sitename)")) {
						commandText += " (SiteName)";
					} else if (arg.equalsIgnoreCase("(reward)")) {
						commandText += " (Reward)";
					} else if (arg.equalsIgnoreCase("(boolean)")) {
						commandText += " (True/False)";
					} else if (arg.equalsIgnoreCase("(number)")) {
						commandText += " (Number)";
					} else if (arg.equalsIgnoreCase("(string)")) {
						commandText += " (Text)";
					} else {
						commandText += " " + arg;
					}
				} else {
					if (arg.equalsIgnoreCase("(player)")) {
						commandText += "/(Player)";
					} else if (arg.equalsIgnoreCase("(sitename)")) {
						commandText += "/(SiteName)";
					} else if (arg.equalsIgnoreCase("(reward)")) {
						commandText += "/(Reward)";
					} else if (arg.equalsIgnoreCase("(boolean)")) {
						commandText += "/(True/False)";
					} else if (arg.equalsIgnoreCase("(number)")) {
						commandText += "/(Number)";
					} else if (arg.equalsIgnoreCase("(string)")) {
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

	/**
	 * Gets the help message.
	 *
	 * @return the help message
	 */
	public String getHelpMessage() {
		return helpMessage;
	}

	/**
	 * Gets the perm.
	 *
	 * @return the perm
	 */
	public String getPerm() {
		return perm;
	}

	/**
	 * Run command.
	 *
	 * @param sender the sender
	 * @param args the args
	 * @return true, if successful
	 */
	public boolean runCommand(CommandSender sender, String[] args) {
		if (args.length >= this.args.length) {
			for (int i = 0; i < args.length; i++) {
				if (!argsMatch(args[i], i)) {
					return false;
				}
				if (this.args[i].equalsIgnoreCase("(number)")) {
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
					plugin.debug(sender.getName()
							+ " was denied access to command");
					return true;
				}
			}

			execute(sender, args);
			return true;
		}
		return false;
	}

	/**
	 * Sets the help message.
	 *
	 * @param helpMessage the new help message
	 */
	public void setHelpMessage(String helpMessage) {
		this.helpMessage = helpMessage;
	}

	/**
	 * Sets the perm.
	 *
	 * @param perm the new perm
	 */
	public void setPerm(String perm) {
		this.perm = perm;
	}

}
