package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Objects.CommandHandler;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.Updater.Updater;
import com.vexsoftware.votifier.model.Vote;

public class CommandAdminVote implements CommandExecutor {

	private static CommandAdminVote instance = new CommandAdminVote();

	public static CommandAdminVote getInstance() {
		return instance;
	}

	ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	Config config = Config.getInstance();

	ConfigFormat format = ConfigFormat.getInstance();

	private Main plugin = Main.plugin;

	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	private CommandAdminVote() {
	}

	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
	}

	public void checkUpdate(CommandSender sender) {
		plugin.updater = new Updater(plugin, 15358, false);
		final Updater.UpdateResult result = plugin.updater.getResult();
		switch (result) {
		case FAIL_SPIGOT: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cFailed to check for update for &c&l" + plugin.getName()
							+ "&c!"));
			break;
		}
		case NO_UPDATE: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&c&l" + plugin.getName()
							+ " &cis up to date! Version: &c&l"
							+ plugin.updater.getVersion()));
			break;
		}
		case UPDATE_AVAILABLE: {
			sender.sendMessage(Utils.getInstance().colorize(
					"&c&l" + plugin.getName()
							+ " &chas an update available! Your Version: &c&l"
							+ plugin.getDescription().getVersion()
							+ " New Version: &c&l"
							+ plugin.updater.getVersion()));
			break;
		}
		default: {
			break;
		}
		}
	}

	public void createVoteSite(CommandSender sender, String voteSite) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cCreating VoteSite..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				ConfigVoteSites.getInstance().generateVoteSite(voteSite);
				sender.sendMessage(Utils.getInstance().colorize(
						"&cCreated VoteSite: &c&l" + voteSite));
			}
		});

	}

	public void checkVoteSite(CommandSender sender, String siteName) {
		if (!ConfigVoteSites.getInstance().isServiceSiteGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cServiceSite is invalid, votes may not work properly"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cServiceSite is properly setup"));
		}
		if (!ConfigVoteSites.getInstance().isVoteURLGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cVoteURL is invalid"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cVoteURL is properly setup"));
		}
	}

	public void help(CommandSender sender, int page) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendJson(Commands.getInstance().adminHelp(page - 1));
		} else {
			sender.sendMessage(Utils.getInstance().convertArray(
					Utils.getInstance().comptoString(
							Commands.getInstance().adminHelp(page - 1))));
		}

	}

	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label,
			String[] args) {

		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED
				+ "No valid arguments, see /adminvote help!");

		return true;
	}

	public void permList(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().listPerms());

	}

	public void reload(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				sender.sendMessage(ChatColor.RED + "Reloading "
						+ plugin.getName() + "...");
				plugin.reload();
				sender.sendMessage(ChatColor.RED + plugin.getName() + " "
						+ plugin.getDescription().getVersion() + " reloaded!");
			}
		});

	}

	public void resetPlayerTotals(CommandSender sender, String playerName) {
		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting totals for player &c&l" + playerName));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTotalsPlayer(new User(playerName));
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting totals for &c&l" + playerName));
				plugin.update();
			}
		});
	}

	public void resetTotals(CommandSender sender) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting totals for all players..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTotals();
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting totals"));
				plugin.update();
			}
		});

	}

	public void setConfigAllowUnjoined(CommandSender sender, boolean value) {

		Config.getInstance().setAllowUnJoined(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet AllowUnjoined to &c&l" + value));

	}

	public void setConfigBroadcastVote(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet BroadcastVote to &c&l" + value));

	}

	public void setConfigDebug(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet Debug to &c&l" + value));

	}

	public void setConfigEnableTopVoterAwards(CommandSender sender,
			boolean value) {

		Config.getInstance().setTopVoterAwardsEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet DisableTopVoterAwards to &c&l" + value));

	}

	public void setServerDataPrevMonth(CommandSender sender, int month) {

		ServerData.getInstance().setPrevMonth(month);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet PreMonth to &c&l" + month));

	}

	public void setTotal(CommandSender sender, String playerName,
			String voteSite, int amount) {

		Data.getInstance().setTotal(new User(playerName), voteSite, amount);
		sender.sendMessage(ChatColor.GREEN + playerName + " total votes for "
				+ voteSite + " has been set to " + amount);
		plugin.update();

	}

	public void setVoteSiteEnabled(CommandSender sender, String voteSite,
			boolean value) {
		ConfigVoteSites.getInstance().setEnabled(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet votesite " + voteSite + " enabled to " + value));
	}

	public void setVoteSitePriority(CommandSender sender, String voteSite,
			int value) {

		ConfigVoteSites.getInstance().setPriority(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

	}

	public void setVoteSiteServiceSite(CommandSender sender, String voteSite,
			String serviceSite) {

		ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l"
						+ voteSite));

	}

	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite,
			int delay) {

		ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

	}

	public void setVoteSiteVoteURL(CommandSender sender, String voteSite,
			String url) {

		ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));

	}

	public void site(CommandSender sender, String site) {

		sender.sendMessage(Commands.getInstance().voteCommandSiteInfo(site));

	}

	public void sites(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandSites());

	}

	public void reward(CommandSender sender, String reward) {

		sender.sendMessage(Commands.getInstance().voteCommandRewardInfo(reward));

	}

	public void rewards(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandRewards());

	}

	public void uuid(CommandSender sender, String playerName) {

		sender.sendMessage(ChatColor.GREEN + "UUID of player "
				+ ChatColor.DARK_GREEN + playerName + ChatColor.GREEN + " is: "
				+ Utils.getInstance().getUUID(playerName));

	}

	public void version(CommandSender sender) {
		if (sender instanceof Player) {

			Player player = (Player) sender;
			player.performCommand("bukkit:version " + plugin.getName());

		} else {
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					"bukkit:version " + plugin.getName());
		}
	}

	public void Vote(CommandSender sender, String playerName, String voteSite) {

		VotiferEvent.playerVote(playerName, voteSite);

		Vote vote = new com.vexsoftware.votifier.model.Vote();
		vote.setServiceName(new VoteSite(voteSite).getServiceSite());
		vote.setUsername(playerName);
		try {
			BungeeVote.getInstance().sendVote(vote);
		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			e.printStackTrace();
		}
	}

}
