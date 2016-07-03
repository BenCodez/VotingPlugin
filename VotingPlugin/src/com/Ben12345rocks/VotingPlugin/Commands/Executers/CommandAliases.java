package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.command.defaults.BukkitCommand;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;

public class CommandAliases extends BukkitCommand {

	private Main plugin;

	private CommandHandler cmdHandle;

	public CommandAliases(String commandPrefix, CommandHandler cmdHandle) {
		super(commandPrefix + cmdHandle.getArgs()[0]);
		setPermission(cmdHandle.getPerm());
		setDescription("");
		setUsage("");
		setAliases(new ArrayList<String>());
		this.cmdHandle = cmdHandle;
	}

	@Override
	public boolean execute(CommandSender sender, String alias, String[] args) {
		ArrayList<String> argsNew = new ArrayList<String>();
		argsNew.add(cmdHandle.getArgs()[0]);
		for (String arg : args) {
			argsNew.add(arg);
		}
		if (Config.getInstance().getDebugEnabled()) {
			plugin.getLogger().info("Attempting cmd...");
			plugin.getLogger()
					.info(Utils.getInstance().makeStringList(argsNew));
		}
		if (cmdHandle.runCommand(sender,
				Utils.getInstance().convertArray(argsNew))) {
			if (Config.getInstance().getDebugEnabled()) {
				plugin.getLogger().info("cmd found, ran cmd");
			}
			return true;
		}
		return false;
	}
}
