
package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VotePartyEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Coordinates VoteParty voting, reminders, commands, and reset events. */
public class VoteParty implements Listener {

	private VotingPluginMain plugin;
	private final VotePartyState state;
	private final VotePartyRewardHandler rewardHandler;

	/**
	 * Constructs a new VoteParty.
	 *
	 * @param plugin the main plugin instance
	 */
	public VoteParty(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.state = new VotePartyState(plugin);
		this.rewardHandler = new VotePartyRewardHandler(plugin, state, this);
	}

	/**
	 * Adds a vote total for the user.
	 *
	 * @param user the voting plugin user
	 */
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
		if (!voted.contains(uuid)) {
			voted.add(uuid);
			setVotedUsers(voted);
		}
	}

	/**
	 * Checks if vote party requirements are met and triggers if appropriate.
	 *
	 * @param user        the voting plugin user
	 * @param forceBungee whether to force Bungee processing
	 */
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

	/**
	 * Checks and sends vote reminders if requirements are met.
	 *
	 * @param user the voting plugin user
	 */
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

					MiscUtils.getInstance().executeConsoleCommands(
							plugin.getSpecialRewardsConfig().getVotePartyVoteReminderCommands(), placeholders, false);
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
		return getVotesRequired() - getTotalVotes();
	}

	/**
	 * Gets a random player name from voted users.
	 *
	 * @return random player name or "No Player" if none available
	 */
	public String getRandomPlayerName() {
		return rewardHandler.randomOnlinePlayerName();
	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		return state.getTotalVotes();
	}

	/**
	 * Gets the voted users.
	 *
	 * @return the voted users
	 */
	public List<String> getVotedUsers() {
		return state.getVotedUsers();
	}

	/**
	 * Gets the number of votes required for the vote party.
	 *
	 * @return votes required including extra requirements
	 */
	public int getVotesRequired() {
		return state.getVotesRequired();
	}

	/**
	 * Gives vote party reward to a user.
	 *
	 * @param user      the voting plugin user
	 * @param useBungee whether to use Bungee processing
	 */
	public void giveReward(VotingPluginUser user, boolean useBungee) {
		if (plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired() > 0
				&& user.getVotePartyVotes() < plugin.getSpecialRewardsConfig().getVotePartyUserVotesRequired()) {
			return;
		}
		giveReward(user, user.isOnline(), useBungee);
	}

	/**
	 * Gives vote party reward to a user with online status.
	 *
	 * @param user      the voting plugin user
	 * @param online    whether the user is online
	 * @param useBungee whether to use Bungee processing
	 */
	public void giveReward(VotingPluginUser user, boolean online, boolean useBungee) {
		rewardHandler.giveReward(user, online, useBungee);
	}

	/**
	 * Gives rewards to all eligible players for the vote party.
	 *
	 * @param orgUser     the original voting user who triggered the party
	 * @param forceBungee whether to force Bungee processing
	 */
	public void giveRewards(VotingPluginUser orgUser, boolean forceBungee) {
		rewardHandler.giveRewards(orgUser, forceBungee);
		reset(false);
	}

	/**
	 * Handles day change events to reset vote party if configured.
	 *
	 * @param event the day change event
	 */
	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetEachDay()) {
			runRecoverableVotePartyReset(event.getTransition(), "VotePartyDayReset");
		}
	}

	/**
	 * Handles month change events to reset vote party if configured.
	 *
	 * @param event the month change event
	 */
	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetMonthly()) {
			runRecoverableVotePartyReset(event.getTransition(), "VotePartyMonthReset");
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {
			runRecoverableExtraReset(event.getTransition(), "VotePartyMonthExtraVotes");
		}
	}

	/**
	 * Handles week change events to reset vote party if configured.
	 *
	 * @param event the week change event
	 */
	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		if (plugin.getSpecialRewardsConfig().isVotePartyResetWeekly()) {
			runRecoverableVotePartyReset(event.getTransition(), "VotePartyWeekReset");
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesWeekly()) {
			runRecoverableExtraReset(event.getTransition(), "VotePartyWeekExtraVotes");
		}
	}

	private void runRecoverableVotePartyReset(TimeChangeTransition transition, String effect) {
		if (transition == null) {
			reset(true);
			return;
		}
		runRecoverableReset(transition, effect, () -> {
			String generation = transition.getId() + ':' + effect;
			if (!copyRecoverableUserCountBoundary("vote-party-copy:" + generation)) {
				throw new IllegalStateException("Unable to durably capture VoteParty user counts");
			}
			plugin.getServerData().prepareTimeChangeVotePartyReset(transition, effect);
			if (!resetRecoverableUserCounts("vote-party-reset:" + generation)) {
				throw new IllegalStateException("Unable to durably reset VoteParty user counts");
			}
			plugin.getServerData().completeTimeChangeVotePartyReset(transition, effect);
		});
	}

	/** Storage boundary kept separate so lifecycle tests do not require a live database. */
	public boolean copyRecoverableUserCountBoundary(String generation) {
		return state.copyUserCountBoundary(generation);
	}

	/** Storage boundary kept separate so lifecycle tests do not require a live database. */
	public boolean resetRecoverableUserCounts(String generation) {
		return state.resetUserCounts(generation);
	}

	private void runRecoverableExtraReset(TimeChangeTransition transition, String effect) {
		if (transition == null) {
			plugin.getServerData().setVotePartyExtraRequired(0);
			return;
		}
		runRecoverableReset(transition, effect,
				() -> plugin.getServerData().completeTimeChangeVotePartyExtraReset(transition, effect));
	}

	private synchronized void runRecoverableReset(TimeChangeTransition transition, String effect, Runnable reset) {
		if (transition == null) {
			reset.run();
			return;
		}
		TimeChangeTransition.Lease lease = transition.retain();
		try {
			if (transition.isCancellationRequested()) {
				throw new java.util.concurrent.CancellationException("Time transition was cancelled");
			}
			plugin.getServerData().beginTimeChangeRecovery(transition);
			if (!plugin.getServerData().hasTimeChangeEffect(transition, effect)) {
				reset.run();
			}
			if (transition.isCancellationRequested()) {
				throw new java.util.concurrent.CancellationException("Time transition was cancelled");
			}
			lease.complete();
		} catch (Throwable failure) {
			lease.fail(failure);
			plugin.getLogger().warning("VoteParty time-change effect remains pending: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	/**
	 * Registers this class as an event listener.
	 */
	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	/**
	 * Resets the vote party state.
	 *
	 * @param override whether to override total votes to zero
	 */
	public void reset(boolean override) {
		if (override) {
			setTotalVotes(0);
		}
		setVotedUsers(new ArrayList<>());
		resetVotePartyCount();
	}

	/**
	 * Resets the vote party count for all users.
	 */
	public void resetVotePartyCount() {
		state.resetUserCounts();
	}

	/**
	 * Sets the total votes.
	 *
	 * @param value the new total votes
	 */
	public void setTotalVotes(int value) {
		state.setTotalVotes(value);
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value the new voted users
	 */
	public void setVotedUsers(List<String> value) {
		state.setVotedUsers(value);
	}

	/**
	 * Processes a vote for the vote party system.
	 *
	 * @param user        the voting plugin user
	 * @param realVote    whether this is a real vote
	 * @param forceBungee whether to force Bungee processing
	 */
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
