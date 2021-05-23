
package com.bencodez.votingplugin.voteparty;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VotePartyEvent;
import com.bencodez.votingplugin.user.UserManager;
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
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
	public void addVotePlayer(VotingPluginUser user) {
		String uuid = user.getUUID();
		ArrayList<String> voted = getVotedUsers();
		if (voted == null) {
			voted = new ArrayList<String>();
		}
		if (!voted.contains(uuid)) {
			voted.add(uuid);
			setVotedUsers(voted);
		}
	}

	public void check(boolean forceBungee) {
		if (getTotalVotes() >= getVotesRequired() && ((plugin.getSpecialRewardsConfig().getVotePartyOnlyOncePerDay()
				&& plugin.getServerData().isLastVotePartySameDay())
				|| !plugin.getSpecialRewardsConfig().getVotePartyOnlyOncePerDay())) {
			if (plugin.getSpecialRewardsConfig().isVotePartyResetCount()) {
				setTotalVotes(getTotalVotes() - getVotesRequired());
			}

			VotePartyEvent event = new VotePartyEvent();
			Bukkit.getPluginManager().callEvent(event);
			if (event.isCancelled()) {
				return;
			}

			giveRewards(forceBungee);

			if (plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRquired() > 0) {
				plugin.getServerData().setVotePartyExtraRequired(plugin.getServerData().getVotePartyExtraRequired()
						+ plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRquired());
			}

			if (plugin.getSpecialRewardsConfig().getVotePartyOnlyOncePerDay()) {
				plugin.getServerData().updateLastVoteParty();
			}
		}

	}

	/**
	 * Command vote party.
	 *
	 * @param sender the sender
	 */
	public void commandVoteParty(CommandSender sender) {
		if (plugin.getSpecialRewardsConfig().getVotePartyEnabled()) {
			ArrayList<String> msg = plugin.getConfigFile().getFormatCommandsVoteParty();
			int votesRequired = getVotesRequired();
			int votes = getTotalVotes();
			int neededVotes = votesRequired - votes;
			HashMap<String, String> placeholders = new HashMap<String, String>();
			placeholders.put("votesrequired", "" + votesRequired);
			placeholders.put("neededvotes", "" + neededVotes);
			placeholders.put("votes", "" + votes);
			msg = ArrayUtils.getInstance().colorize(ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders));
			sender.sendMessage(ArrayUtils.getInstance().convert(msg));
		} else {
			sender.sendMessage(StringParser.getInstance().colorize("&cVoteParty not enabled"));
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
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotedUsers() {
		ArrayList<String> list = (ArrayList<String>) plugin.getServerData().getData().getList("VoteParty.Voted");
		if (list != null) {
			return list;
		}
		return new ArrayList<String>();
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
				plugin.getSpecialRewardsConfig().getVotePartyRewardsPath())
						.setOnline(online)
						.withPlaceHolder("VotesRequired",
								"" + plugin.getSpecialRewardsConfig().getVotePartyVotesRequired())
						.setServer(useBungee).send(user);
	}

	public void giveRewards(boolean forceBungee) {
		MiscUtils.getInstance().broadcast(plugin.getSpecialRewardsConfig().getVotePartyBroadcast());

		for (final String cmd : plugin.getSpecialRewardsConfig().getVotePartyCommands()) {
			Bukkit.getScheduler().runTask(plugin, new Runnable() {

				@Override
				public void run() {
					Bukkit.getServer().dispatchCommand(Bukkit.getConsoleSender(), cmd);
				}

			});
		}

		if (plugin.getSpecialRewardsConfig().getVotePartyGiveAllPlayers()) {
			plugin.debug("Trying to give all players vote party");
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveReward(user, forceBungee);
				}

			}
		} else {
			plugin.debug("Trying to give all voted players vote party");
			plugin.debug(ArrayUtils.getInstance().makeStringList(getVotedUsers()));
			for (String uuid : getVotedUsers()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				if (!plugin.getSpecialRewardsConfig().isVotePartyGiveOnlinePlayersOnly() || user.isOnline()) {
					giveReward(user, forceBungee);
				}

			}
		}
		reset(false);
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().getVotePartyResetEachDay()) {
			reset(true);
		}
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().getVotePartyResetMontly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().getVotePartyResetWeekly()) {
			reset(true);
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void reset(boolean override) {
		if (override) {
			setTotalVotes(0);
		}
		setVotedUsers(new ArrayList<String>());
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getVotePartyVotes() != 0) {
				user.setVotePartyVotes(0);
			}
		}

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
	public void setVotedUsers(ArrayList<String> value) {
		plugin.getServerData().getData().set("VoteParty.Voted", value);
		plugin.getServerData().saveData();
	}

	public synchronized void vote(VotingPluginUser user, boolean realVote, boolean forceBungee) {
		if (plugin.getSpecialRewardsConfig().getVotePartyEnabled()) {
			if (plugin.getSpecialRewardsConfig().getVotePartyCountFakeVotes() || realVote) {
				if (plugin.getSpecialRewardsConfig().getVotePartyCountOfflineVotes() || user.isOnline()) {
					addTotal(user);
					addVotePlayer(user);
					check(forceBungee);
				}
			}
		}
	}

}
