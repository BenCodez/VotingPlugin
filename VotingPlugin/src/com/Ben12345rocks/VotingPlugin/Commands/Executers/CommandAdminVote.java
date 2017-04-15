package com.Ben12345rocks.VotingPlugin.Commands.Executers;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.Util.Updater.Updater;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.Commands;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Events.VotiferEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoterHandler;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

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

	/** The config. */
	Config config = Config.getInstance();

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
			sender.sendMessage(StringUtils.getInstance()
					.colorize("&cFailed to check for update for &c&l" + plugin.getName() + "&c!"));
			break;
		}
		case NO_UPDATE: {
			sender.sendMessage(StringUtils.getInstance().colorize(
					"&c&l" + plugin.getName() + " &cis up to date! Version: &c&l" + plugin.updater.getVersion()));
			break;
		}
		case UPDATE_AVAILABLE: {
			sender.sendMessage(StringUtils.getInstance()
					.colorize("&c&l" + plugin.getName() + " &chas an update available! Your Version: &c&l"
							+ plugin.getDescription().getVersion() + " &cNew Version: &c&l"
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
			sender.sendMessage(
					StringUtils.getInstance().colorize("&cServiceSite is invalid, votes may not work properly"));
		} else {
			sender.sendMessage(StringUtils.getInstance().colorize("&aServiceSite is properly setup"));
		}
		if (!ConfigVoteSites.getInstance().isVoteURLGood(siteName)) {
			sender.sendMessage(StringUtils.getInstance().colorize("&cVoteURL is invalid"));
		} else {
			sender.sendMessage(StringUtils.getInstance().colorize("&aVoteURL is properly setup"));
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

		sender.sendMessage(StringUtils.getInstance().colorize("&cCreating VoteSite..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				ConfigVoteSites.getInstance().generateVoteSite(voteSite);
				sender.sendMessage(StringUtils.getInstance().colorize("&cCreated VoteSite: &c&l" + voteSite));
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
			User user = UserManager.getInstance().getVotingPluginUser((Player) sender);
			user.sendJson(Commands.getInstance().adminHelp(sender, page - 1));
		} else {
			sender.sendMessage(ArrayUtils.getInstance().convert(
					ArrayUtils.getInstance().comptoString(Commands.getInstance().adminHelp(sender, page - 1))));
		}

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.bukkit.command.CommandExecutor#onCommand(org.bukkit.command.
	 * CommandSender , org.bukkit.command.Command, java.lang.String,
	 * java.lang.String[])
	 */
	@Override
	public boolean onCommand(CommandSender sender, Command cmd, String label, String[] args) {
		for (CommandHandler commandHandler : plugin.adminVoteCommand) {
			if (commandHandler.runCommand(sender, args)) {
				return true;
			}
		}

		// invalid command
		sender.sendMessage(ChatColor.RED + "No valid arguments, see /adminvote help!");

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
				sender.sendMessage(ChatColor.RED + "Reloading " + plugin.getName() + "...");
				plugin.reload();
				sender.sendMessage(
						ChatColor.RED + plugin.getName() + " v" + plugin.getDescription().getVersion() + " reloaded!");
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
		sender.sendMessage(StringUtils.getInstance().colorize("&cResseting totals for player &c&l" + playerName));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				User user = UserManager.getInstance().getVotingPluginUser(playerName);
				user.resetDailyTotalVotes();
				user.resetMonthlyTotalVotes();
				user.resetWeeklyTotalVotes();
				sender.sendMessage(StringUtils.getInstance().colorize("&cDone resseting totals for &c&l" + playerName));
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

		sender.sendMessage(StringUtils.getInstance().colorize("&cResseting totals for all players..."));
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				TopVoterHandler.getInstance().resetDailyTotals();
				TopVoterHandler.getInstance().resetMonthlyTotals();
				TopVoterHandler.getInstance().resetWeeklyTotals();
				sender.sendMessage(StringUtils.getInstance().colorize("&cDone resseting totals"));
				plugin.update();
			}
		});

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
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet AllowUnjoined to &c&l" + value));

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
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet BroadcastVote to &c&l" + value));

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
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet Debug to &c&l" + value));

	}

	/**
	 * Sets the config enable top voter awards.
	 *
	 * @param sender
	 *            the sender
	 * @param value
	 *            the value
	 */
	public void setConfigEnableTopVoterAwards(CommandSender sender, boolean value) {

		Config.getInstance().setTopVoterAwardsEnabled(value);
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet DisableTopVoterAwards to &c&l" + value));

	}

	/**
	 * Sets the total.
	 *
	 * @param sender
	 *            the sender
	 * @param playerName
	 *            the player name
	 * @param amount
	 *            the amount
	 */
	public void setTotal(CommandSender sender, String playerName, int amount) {

		UserManager.getInstance().getVotingPluginUser(playerName).setMonthTotal(amount);
		sender.sendMessage(ChatColor.GREEN + playerName + " total votes has been set to " + amount);
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
	public void setVoteSiteEnabled(CommandSender sender, String voteSite, boolean value) {
		ConfigVoteSites.getInstance().setEnabled(voteSite, value);
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet votesite " + voteSite + " enabled to " + value));
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
	public void setVoteSitePriority(CommandSender sender, String voteSite, int value) {

		ConfigVoteSites.getInstance().setPriority(voteSite, value);
		sender.sendMessage(
				StringUtils.getInstance().colorize("&cSet priortiy to &c&l" + value + "&c on &c&l" + voteSite));

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
	public void setVoteSiteServiceSite(CommandSender sender, String voteSite, String serviceSite) {

		ConfigVoteSites.getInstance().setServiceSite(voteSite, serviceSite);
		sender.sendMessage(StringUtils.getInstance()
				.colorize("&cSet ServiceSite to &c&l" + serviceSite + "&c on &c&l" + voteSite));

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
	public void setVoteSiteVoteDelay(CommandSender sender, String voteSite, int delay) {

		ConfigVoteSites.getInstance().setVoteDelay(voteSite, delay);
		sender.sendMessage(
				StringUtils.getInstance().colorize("&cSet VoteDelay to &c&l" + delay + "&c on &c&l" + voteSite));

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
	public void setVoteSiteVoteURL(CommandSender sender, String voteSite, String url) {

		ConfigVoteSites.getInstance().setVoteURL(voteSite, url);
		sender.sendMessage(StringUtils.getInstance().colorize("&cSet VoteURL to &c&l" + url + "&c on &c&l" + voteSite));

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

		sender.sendMessage(ChatColor.GREEN + "UUID of player " + ChatColor.DARK_GREEN + playerName + ChatColor.GREEN
				+ " is: " + PlayerUtils.getInstance().getUUID(playerName));

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
			Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), "bukkit:version " + plugin.getName());
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
	}

}
