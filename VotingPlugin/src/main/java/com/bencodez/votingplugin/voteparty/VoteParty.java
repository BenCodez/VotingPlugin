
package com.bencodez.votingplugin.voteparty;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VotePartyEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteParty.
 */
public class VoteParty implements Listener {

	private VotingPluginMain plugin;

	public VoteParty(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void addTotal(VotingPluginUser user) {
		setTotalVotes(getTotalVotes() + 1);
		user.setVotePartyVotes(user.getVotePartyVotes() + 1);
		plugin.getPlaceholders().onVotePartyUpdate();
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
	public void addVotePlayer(VotingPluginUser user) {
		String uuid = user.getUUID();
		List<String> voted = getVotedUsers();
		if (voted == null) {
			voted = new ArrayList<>();
		}
		if (!voted.contains(uuid)) {
			voted.add(uuid);
			setVotedUsers(voted);
		}
	}

	public void check(VotingPluginUser user, boolean forceBungee) {
		if (getTotalVotes() < getVotesRequired()) {
			plugin.extraDebug("Not enough votes for vote party: " + getTotalVotes() + " / " + getVotesRequired());
			return;
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
				&& plugin.getServerData().isLastVotePartySameDay()) {
			plugin.extraDebug("Already had vote party today");
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
				&& plugin.getServerData().isLastVotePartySameWeek()) {
			plugin.extraDebug("Already had vote party this week");
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyResetCount()) {
			setTotalVotes(getTotalVotes() - getVotesRequired());
		}

		VotePartyEvent event = new VotePartyEvent();
		Bukkit.getPluginManager().callEvent(event);
		if (event.isCancelled()) {
			return;
		}

		giveRewards(user, forceBungee);

		if (plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired() > 0) {
			plugin.getServerData().setVotePartyExtraRequired(plugin.getServerData().getVotePartyExtraRequired()
					+ plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired());
		}

		plugin.getServerData().updateLastVoteParty();

		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()) {
			plugin.getServerData().updateLastVotePartyWeek();
		}

	}

	public void checkVoteReminder(VotingPluginUser user) {
		if (!user.isVanished()) {
			int neededVotes = getNeededVotes();

			for (Integer num1 : plugin.getSpecialRewardsConfig().getVotePartyVoteReminderAtVotes()) {
				int num = num1.intValue();
				if (neededVotes == num) {
					String broadcastMessage = plugin.getSpecialRewardsConfig().getVotePartyVoteReminderBroadcast();
					HashMap<String, String> placeholders = new HashMap<>();
					placeholders.put("player", user.getPlayerName());
					placeholders.put("votesrequired", "" + neededVotes);
					MiscUtils.getInstance()
							.broadcast(PlaceholderUtils.replacePlaceHolder(broadcastMessage, placeholders));
				}
			}
		}

	}

	/**
	 * Command vote party.
	 *
	 * @param sender the sender
	 */
	public void commandVoteParty(CommandSender sender) {
		if (plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
			ArrayList<String> msg = plugin.getConfigFile().getFormatCommandsVoteParty();
			int votesRequired = getVotesRequired();
			int votes = getTotalVotes();
			int neededVotes = votesRequired - votes;
			HashMap<String, String> placeholders = new HashMap<>();
			placeholders.put("votesrequired", "" + votesRequired);
			placeholders.put("neededvotes", "" + neededVotes);
			placeholders.put("votes", "" + votes);
			msg = ArrayUtils.colorize(PlaceholderUtils.replacePlaceHolder(msg, placeholders));
			if (sender instanceof Player) {
				Player p = (Player) sender;
				sender.sendMessage(ArrayUtils.convert(PlaceholderUtils.replacePlaceHolders(msg, p)));
			} else {
				sender.sendMessage(ArrayUtils.convert(msg));
			}
		} else {
			sender.sendMessage(MessageAPI.colorize("&cVoteParty not enabled"));
		}
	}

	/**
	 * Gets the needed votes.
	 *
	 * @return the needed votes
	 */
	public int getNeededVotes() {
		int votesRequired = getVotesRequired();
		int votes = getTotalVotes();
		int neededVotes = votesRequired - votes;
		return neededVotes;
	}

	public String getRandomPlayerName() {
		ArrayList<String> allPlayers = new ArrayList<>();
		for (Player players : Bukkit.getOnlinePlayers()) {
			allPlayers.add(players.getName());
		}
		if (allPlayers.size() == 0) {
			return "No Player";
		}
		int random = new Random().nextInt(allPlayers.size());
		return allPlayers.get(random);
	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		return plugin.getServerData().getData().getInt("VoteParty.Total");
	}

	/**
	 * Gets the voted users.
	 *
	 * @return the voted users
	 */
	public List<String> getVotedUsers() {
		List<String> list =  plugin.getServerData().getData().getStringList("VoteParty.Voted");
		if (list != null) {
			return list;
		}
		return new ArrayList<>();
	}

	public int getVotesRequired() {
		int required = plugin.getSpecialRewardsConfig().getVotePartyVotesRequired();
		int extra = plugin.getServerData().getVotePartyExtraRequired();
		if (extra > 0) {
			return required + extra;
		}
		return required;
	}

	public void giveReward(VotingPluginUser user, boolean useBungee) {
		if (plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired() > 0) {
			if (user.getVotePartyVotes() < plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired()) {
				return;
			}
		}
		giveReward(user, user.isOnline(), useBungee);
	}

	public void giveReward(VotingPluginUser user, boolean online, boolean useBungee) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVotePartyRewardsPath()).setOnline(online)
				.withPlaceHolder("VotesRequired", "" + plugin.getSpecialRewardsConfig().getVotePartyVotesRequired())
				.withPlaceHolder("FirstVotePartyToday", "" + !plugin.getServerData().isLastVotePartySameDay())
				.setServer(useBungee).send(user);
	}

	public void giveRewards(VotingPluginUser orgUser, boolean forceBungee) {
		MiscUtils.getInstance().broadcast(plugin.getSpecialRewardsConfig().getVotePartyBroadcast());

		String player = getRandomPlayerName();
		for (final String cmd : plugin.getSpecialRewardsConfig().getVotePartyGlobalCommands()) {
			plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(),
							PlaceholderUtils.replacePlaceHolder(cmd, "randomonlineplayer", player));
				}

			});
		}

		final ArrayList<String> list = plugin.getSpecialRewardsConfig().getVotePartyGlobalRandomCommand();
		if (list.size() > 0) {
			plugin.getBukkitScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), PlaceholderUtils.replacePlaceHolder(
							list.get(ThreadLocalRandom.current().nextInt(list.size())), "randomonlineplayer", player));
				}

			});
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyGiveAllPlayers()) {
			plugin.debug("Trying to give all players vote party");

			ArrayList<String> alreadyGotten = new ArrayList<>();
			for (Player p : Bukkit.getOnlinePlayers()) {
				VotingPluginUser user;
				if (orgUser != null && orgUser.getJavaUUID().equals(p.getUniqueId())) {
					user = orgUser;
				} else {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
					user.dontCache();
				}

				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveReward(user, forceBungee);
				}
				alreadyGotten.add(p.getUniqueId().toString());
			}
			for (String uuid : plugin.getVotingPluginUserManager().getAllUUIDs()) {
				if (!alreadyGotten.contains(uuid)) {
					VotingPluginUser user;
					if (orgUser != null && orgUser.getJavaUUID().toString().equals(uuid)) {
						user = orgUser;
					} else {
						user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid));
						user.dontCache();
					}

					if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
						giveReward(user, forceBungee);
					}
				}
			}
		} else {
			plugin.debug("Trying to give all voted players vote party");
			plugin.debug(ArrayUtils.makeStringList(getVotedUsers()));
			for (String uuid :

			getVotedUsers()) {
				VotingPluginUser user;
				if (orgUser != null && orgUser.getJavaUUID().toString().equals(uuid)) {
					user = orgUser;
				} else {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid));
					user.dontCache();
				}
				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveReward(user, forceBungee);
				}

			}
		}

		reset(false);
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetEachDay()) {
			reset(true);
		}
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetMonthly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetWeekly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesWeekly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void reset(boolean override) {
		if (override) {
			setTotalVotes(0);
		}
		setVotedUsers(new ArrayList<>());
		resetVotePartyCount();

	}

	public void resetVotePartyCount() {
		plugin.getUserManager().removeAllKeyValues("VotePartyVotes", DataType.INTEGER);
	}

	/**
	 * Sets the total votes.
	 *
	 * @param value the new total votes
	 */
	public void setTotalVotes(int value) {
		plugin.getServerData().getData().set("VoteParty.Total", value);
		plugin.getServerData().saveData();
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value the new voted users
	 */
	public void setVotedUsers(List<String> value) {
		plugin.getServerData().getData().set("VoteParty.Voted", value);
		plugin.getServerData().saveData();
	}

	public synchronized void vote(VotingPluginUser user, boolean realVote, boolean forceBungee) {
		if (plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
			if (plugin.getSpecialRewardsConfig().isVotePartyCountFakeVotes() || realVote) {
				if (plugin.getSpecialRewardsConfig().isVotePartyCountOfflineVotes() || user.isOnline()) {
					addTotal(user);
					addVotePlayer(user);
					check(user, forceBungee);
					checkVoteReminder(user);
				}
			}
		}
	}

}
