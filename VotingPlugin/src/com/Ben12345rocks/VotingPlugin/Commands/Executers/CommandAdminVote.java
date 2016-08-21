package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Bungee.BungeeVote;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.vexsoftware.votifier.model.Vote;

// TODO: Auto-generated Javadoc
/**
 * The Class CommandAdminVote.
 */
public class CommandAdminVote implements CommandExecutor {

	/** The instance. */
	private static CommandAdminVote instance = new CommandAdminVote();

	/**
	 * Gets the single instance of CommandAdminVote.
	 *
	 * @return single instance of CommandAdminVote
	 */
	public static CommandAdminVote getInstance() {
		return instance;
	}

	/** The bonus reward. */
	ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	/** The config. */
	Config config = Config.getInstance();

	/** The format. */
	ConfigFormat format = ConfigFormat.getInstance();

	/** The plugin. */
	private Main plugin = Main.plugin;

	/** The vote sites. */
	ConfigVoteSites voteSites = ConfigVoteSites.getInstance();

	/**
	 * Instantiates a new command admin vote.
	 */
	private CommandAdminVote() {
	}

	/**
	 * Instantiates a new command admin vote.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CommandAdminVote(Main plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check update.
	 *
	 * @param sender
	 *            the sender
	 */
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
							+ " &cNew Version: &c&l"
							+ plugin.updater.getVersion()));
			break;
		}
		default: {
			break;
		}
		}
	}

	/**
	 * Check vote site.
	 *
	 * @param sender
	 *            the sender
	 * @param siteName
	 *            the site name
	 */
	public void checkVoteSite(CommandSender sender, String siteName) {
		if (!ConfigVoteSites.getInstance().isServiceSiteGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cServiceSite is invalid, votes may not work properly"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&aServiceSite is properly setup"));
		}
		if (!ConfigVoteSites.getInstance().isVoteURLGood(siteName)) {
			sender.sendMessage(Utils.getInstance().colorize(
					"&cVoteURL is invalid"));
		} else {
			sender.sendMessage(Utils.getInstance().colorize(
					"&aVoteURL is properly setup"));
		}
	}

	/**
	 * Creates the vote site.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 */
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

	/**
	 * Help.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 */
	public void help(CommandSender sender, int page) {
		if (sender instanceof Player) {
			User user = new User((Player) sender);
			user.sendJson(Commands.getInstance().adminHelp(sender, page - 1));
		} else {
			sender.sendMessage(Utils.getInstance()
					.convertArray(
							Utils.getInstance().comptoString(
									Commands.getInstance().adminHelp(sender,
											page - 1))));
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.bukkit.command.CommandExecutor#onCommand(org.bukkit.command.CommandSender
	 * , org.bukkit.command.Command, java.lang.String, java.lang.String[])
	 */
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

	/**
	 * Perm list.
	 *
	 * @param sender
	 *            the sender
	 */
	public void permList(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().listPerms());

	}

	/**
	 * Reload.
	 *
	 * @param sender
	 *            the sender
	 */
	public void reload(CommandSender sender) {

		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				sender.sendMessage(ChatColor.RED + "Reloading "
						+ plugin.getName() + "...");
				plugin.reload();
				sender.sendMessage(ChatColor.RED + plugin.getName() + " v"
						+ plugin.getDescription().getVersion() + " reloaded!");
			}
		});

	}

	/**
	 * Reset player totals.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
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

	/**
	 * Reset totals.
	 *
	 * @param sender
	 *            the sender
	 */
	public void resetTotals(CommandSender sender) {

		sender.sendMessage(Utils.getInstance().colorize(
				"&cResseting totals for all players..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoter.getInstance().resetTotalsMonthly();
				sender.sendMessage(Utils.getInstance().colorize(
						"&cDone resseting totals"));
				plugin.update();
			}
		});

	}

	/**
	 * Reward.
	 *
	 * @param sender
	 *            the sender
	 * @param reward
	 *            the reward
	 */
	public void reward(CommandSender sender, String reward) {

		sender.sendMessage(Commands.getInstance().voteCommandRewardInfo(reward));

	}

	/**
	 * Rewards.
	 *
	 * @param sender
	 *            the sender
	 */
	public void rewards(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandRewards());

	}

	/**
	 * Sets the config allow unjoined.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigAllowUnjoined(CommandSender sender, boolean value) {

		Config.getInstance().setAllowUnJoined(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet AllowUnjoined to &c&l" + value));

	}

	/**
	 * Sets the config broadcast vote.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigBroadcastVote(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet BroadcastVote to &c&l" + value));

	}

	/**
	 * Sets the config debug.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigDebug(CommandSender sender, boolean value) {

		Config.getInstance().setDebugEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet Debug to &c&l" + value));

	}

	/**
	 * Sets the config enable top voter awards.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigEnableTopVoterAwards(CommandSender sender,
			boolean value) {

		Config.getInstance().setTopVoterAwardsEnabled(value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet DisableTopVoterAwards to &c&l" + value));

	}

	public void setRewardMaxMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMaxMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet maxmoney to &c&l" + money + "&c on &c&l" + reward));
	}

	public void setRewardMessage(CommandSender sender, String reward, String msg) {
		ConfigRewards.getInstance().setMessagesReward(reward, msg);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet reward message to &c&l" + msg + "&c on &c&l" + reward));
	}

	public void setRewardMinMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMinMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet minmoney to &c&l" + money + "&c on &c&l" + reward));
	}

	public void setRewardMoney(CommandSender sender, String reward, int money) {
		ConfigRewards.getInstance().setMoney(reward, money);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet money to &c&l" + money + "&c on &c&l" + reward));
	}

	public void setRewardRequirePermission(CommandSender sender, String reward,
			boolean value) {
		ConfigRewards.getInstance().setRequirePermission(reward, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet require permission to &c&l" + value + "&c on &c&l"
						+ reward));
	}

	/**
	 * Sets the server data prev month.
	 *
	 * @param sender
	 *            the sender
	 * @param month
	 *            the month
	 */
	public void setServerDataPrevMonth(CommandSender sender, int month) {

		ServerData.getInstance().setPrevMonth(month);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet PreMonth to &c&l" + month));

	}

	/**
	 * Sets the total.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public void setTotal(CommandSender sender, String playerName,
			String voteSite, int amount) {

		Data.getInstance().setTotal(new User(playerName), voteSite, amount);
		sender.sendMessage(ChatColor.GREEN + playerName + " total votes for "
				+ voteSite + " has been set to " + amount);
		plugin.update();

	}

	/**
	 * Sets the vote site enabled.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setVoteSiteEnabled(CommandSender sender, String voteSite,
			boolean value) {
		ConfigVoteSites.getInstance().setEnabled(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet votesite " + voteSite + " enabled to " + value));
	}

	/**
	 * Sets the vote site priority.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param value
	 *            the value
	 */
	public void setVoteSitePriority(CommandSender sender, String voteSite,
			int value) {

		ConfigVoteSites.getInstance().setPriority(voteSite, value);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

	}

	/**
	 * Sets the vote site service site.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param serviceSite
	 *            the service site
	 */
	public void setVoteSiteServiceSite(CommandSender sender, String voteSite,
			String serviceSite) {

		ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l"
						+ voteSite));

	}

	/**
	 * Sets the vote site vote delay.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param delay
	 *            the delay
	 */
	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite,
			int delay) {

		ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

	}

	/**
	 * Sets the vote site vote URL.
	 *
	 * @param sender
	 *            the sender
	 * @param voteSite
	 *            the vote site
	 * @param url
	 *            the url
	 */
	public void setVoteSiteVoteURL(CommandSender sender, String voteSite,
			String url) {

		ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
		sender.sendMessage(Utils.getInstance().colorize(
				"&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));

	}

	/**
	 * Site.
	 *
	 * @param sender
	 *            the sender
	 * @param site
	 *            the site
	 */
	public void site(CommandSender sender, String site) {

		sender.sendMessage(Commands.getInstance().voteCommandSiteInfo(site));

	}

	/**
	 * Sites.
	 *
	 * @param sender
	 *            the sender
	 */
	public void sites(CommandSender sender) {

		sender.sendMessage(Commands.getInstance().voteCommandSites());

	}

	/**
	 * Uuid.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 */
	public void uuid(CommandSender sender, String playerName) {

		sender.sendMessage(ChatColor.GREEN + "UUID of player "
				+ ChatColor.DARK_GREEN + playerName + ChatColor.GREEN + " is: "
				+ Utils.getInstance().getUUID(playerName));

	}

	/**
	 * Version.
	 *
	 * @param sender
	 *            the sender
	 */
	public void version(CommandSender sender) {
		if (sender instanceof Player) {

			Player player = (Player) sender;
			player.performCommand("bukkit:version " + plugin.getName());

		} else {
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
					"bukkit:version " + plugin.getName());
		}
	}

	/**
	 * Vote.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 * @param voteSite
	 *            the vote site
	 */
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
