package com.bencodez.votingplugin.proxy;

import com.bencodez.advancedcore.api.time.TimeType;

public class VotingPluginProxyCommand {
	private VotingPluginProxy plugin;

	public VotingPluginProxyCommand(VotingPluginProxy plugin) {
		this.plugin = plugin;
	}

	/**
	 * Execute the command and return a message as a String.
	 *
	 * @param args Command arguments
	 * @return The resulting message to send
	 */
	public String execute(String[] args) {
		if (args.length == 0)
			return "&cPlease provide a valid command. Use 'help' to view all commands.";

		switch (args[0].toLowerCase()) {
		case "reload":
			plugin.reloadCore(false);
			return "&aReloading VotingPlugin";

		case "reloadmysql":
			plugin.reloadCore(true);
			return "&aReloading VotingPlugin including MySQL";

		case "vote":
			if (args.length >= 3) {
				String user = args[1];
				String site = args[2];
				plugin.vote(user, site, false, true, 0, null, null);
				return "&aVote sent for " + user + " on " + site;
			}
			return "&cUsage: vote <player> <site>";

		case "forcetimechange":
			if (args.length >= 2) {
				plugin.getBungeeTimeChecker().forceChanged(TimeType.getTimeType(args[1]));
				return "&aTriggered time change to " + args[1];
			}
			return "&cUsage: forcetimechange <TimeType>";

		case "status":
			return handleStatusCommand();

		case "multiproxystatus":
			plugin.getMultiProxyHandler().sendStatus();
			return "&aSent status message across multi-proxy";

		case "help":
			return getHelpMessage();

		case "voteparty":
			return handleVotePartyCommand(args);

		default:
			return "&cUnknown command. Use 'help' to view all commands.";
		}
	}

	private String handleStatusCommand() {
		BungeeMethod method = plugin.getMethod();
		plugin.status();
		plugin.sendServerNameMessage();
		return "&aChecking status with method: " + method.toString().toLowerCase();
	}

	private String handleVotePartyCommand(String[] args) {
		if (args.length > 1) {
			if ("force".equalsIgnoreCase(args[1])) {
				plugin.setCurrentVotePartyVotes(plugin.getCurrentVotePartyVotesRequired());
				plugin.checkVoteParty();
				return "&aVote party forced";
			} else if ("setvotecount".equalsIgnoreCase(args[1]) && args.length > 2 && isInt(args[2])) {
				plugin.setCurrentVotePartyVotes(Integer.parseInt(args[2]));
				return "&aSet current vote party votes to " + args[2];
			} else {
				return "&cInvalid usage. Use 'help' for correct syntax.";
			}
		}
		return "&cUsage: voteparty <force/setvotecount>";
	}

	private String getHelpMessage() {
		StringBuilder helpBuilder = new StringBuilder("&aVotingPlugin Commands:\n");
		helpBuilder.append("/votingplugin reload - Reload the plugin\n");
		helpBuilder.append("/votingplugin reloadmysql - Reload the plugin including MySQL\n");
		helpBuilder.append("/votingplugin vote <player> <site> - Send a vote\n");
		helpBuilder.append("/votingplugin forcetimechange <TimeType> - Force a time change\n");
		helpBuilder.append("/votingplugin status - Check connection status\n");
		helpBuilder.append("/votingplugin multiproxystatus - Send status message across proxies\n");
		helpBuilder.append("/votingplugin voteparty <force/setvotecount> - Trigger or modify vote party\n");
		return helpBuilder.toString();
	}

	private boolean isInt(String str) {
		try {
			Integer.parseInt(str);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

}
