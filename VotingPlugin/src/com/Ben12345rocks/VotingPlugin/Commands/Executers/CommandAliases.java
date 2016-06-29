package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.command.TabCompleter;
import org.bukkit.command.defaults.BukkitCommand;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Commands.TabCompleter.AliasesTabCompleter;
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
		TabCompleter tab = new AliasesTabCompleter();
		((AliasesTabCompleter) tab).setTabCompleter(cmdHandle);
		plugin.getCommand(commandPrefix + cmdHandle.getArgs()[0])
		.setTabCompleter(tab);
	}

	@Override
	public boolean execute(CommandSender sender, String alias, String[] args) {
		ArrayList<String> argsNew = new ArrayList<String>();
		argsNew.add(cmdHandle.getArgs()[0]);
		for (String arg : args) {
			argsNew.add(arg);
		}
		if (cmdHandle.runCommand(sender,
				Utils.getInstance().convertArray(argsNew))) {
			return true;
		}
		return false;
	}
}
