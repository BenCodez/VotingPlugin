
package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiConsumer;
import java.util.function.BooleanSupplier;
import java.util.function.Supplier;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.time.TimeType;
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
	private final AtomicLong rewardGeneration = new AtomicLong();
	private record NewPartyVotes(VotingPluginUser user, int count) { }
	private final HashMap<String, NewPartyVotes> votesAfterCommit = new HashMap<>();
	private Object counterResetAttempt;
	private record ScalarRollback(Object attempt, List<String> voters, int total, int extra, int day, int week) { }
	private final AtomicReference<ScalarRollback> pendingScalarRollback = new AtomicReference<>();
	private final AtomicReference<Runnable> pendingCounterReset = new AtomicReference<>();

	long rewardGeneration() { return rewardGeneration.get(); }

	<T> T withPreparedVoteFence(Supplier<T> work) {
		synchronized (this) {
			stateCommit.lock();
			try { return work.get(); }
			finally { stateCommit.unlock(); }
		}
	}

	int preparedVotes(VotingPluginUser user) {
		NewPartyVotes delta = counterResetAttempt == storedPreparation.get()
				? votesAfterCommit.get(user.getUUID()) : null;
		return Math.max(0, user.getVotePartyVotes() - (delta == null ? 0 : delta.count()));
	}


	private final AtomicReference<Object> storedPreparation = new AtomicReference<>();
	private record PendingCheck(Object attempt, long generation, Runnable action) { }
	private final AtomicReference<PendingCheck> pendingStoredCheck = new AtomicReference<>();
	// Workers may hold this permit across storage; the platform only tries it.
	// Order: worker monitor -> stateCommit -> scalarCommit; platform only tries the last two.
	// Scalar commands take scalarCommit alone and release it before user storage.
	private final ReentrantLock stateCommit = new ReentrantLock();
	private final ReentrantLock scalarCommit = new ReentrantLock();

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
	public synchronized void addTotal(VotingPluginUser user) {
		stateCommit.lock();
		try {
			if (!recoverScalarState()) throw new IllegalStateException("VoteParty state recovery is still unavailable");
			scalarCommit.lock();
			try { state.setTotalVotes(getTotalVotes() + 1); }
			finally { scalarCommit.unlock(); }
			user.setVotePartyVotes(user.getVotePartyVotes() + 1);
			if (counterResetAttempt != null && counterResetAttempt == storedPreparation.get()) {
				NewPartyVotes previous = votesAfterCommit.get(user.getUUID());
				votesAfterCommit.put(user.getUUID(), new NewPartyVotes(user, previous == null ? 1 : previous.count() + 1));
			}
			plugin.getPlaceholders().onVotePartyUpdate();
		} finally { stateCommit.unlock(); }
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
	public void addVotePlayer(VotingPluginUser user) {
		scalarCommit.lock();
		try {
			String uuid = user.getUUID();
			List<String> voted = getVotedUsers();
			if (!voted.contains(uuid)) {
				voted.add(uuid);
				setVotedUsers(voted);
			}
		} finally { scalarCommit.unlock(); }
	}

	/**
	 * Checks if vote party requirements are met and triggers if appropriate.
	 *
	 * @param user        the voting plugin user
	 * @param forceBungee whether to force Bungee processing
	 */
	public synchronized void check(VotingPluginUser user, boolean forceBungee) {
		stateCommit.lock();
		try {
			if (!recoverScalarState()) return;
			Object active = storedPreparation.get();
			if (active != null) {
				retryPendingCounterReset();
				registerPendingCheck(active, rewardGeneration.get(), () -> check(user, forceBungee));
				return;
			}
			// A period-limited party cannot use stored recipients. Avoid occupying
			// the persistence worker with an enumeration that cannot start a party.
			if ((plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
					&& plugin.getServerData().isLastVotePartySameDay())
					|| (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
							&& plugin.getServerData().isLastVotePartySameWeek())) return;
			if (getTotalVotes() >= getVotesRequired()
					&& prepareStoredParty((ids, finished) -> checkPrepared(user, forceBungee, ids, finished))) return;
			checkPrepared(user, forceBungee, null, null);
		} finally { stateCommit.unlock(); }
	}

	private void checkPrepared(VotingPluginUser user, boolean forceBungee, VotePartyRewardHandler.StoredRecipients storedIds,
			Object attempt) {
		if (attempt != null && !plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
			finishStoredParty(attempt);
			return;
		}
		if (getTotalVotes() < getVotesRequired()) {
			plugin.extraDebug("Not enough votes for vote party: " + getTotalVotes() + " / " + getVotesRequired());
			finishStoredParty(attempt);
			return;
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
				&& plugin.getServerData().isLastVotePartySameDay()) {
			plugin.extraDebug("Already had vote party today");
			finishStoredParty(attempt);
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
				&& plugin.getServerData().isLastVotePartySameWeek()) {
			plugin.extraDebug("Already had vote party this week");
			finishStoredParty(attempt);
			return;
		}
		if (storedIds == null && plugin.getSpecialRewardsConfig().isVotePartyResetCount()) {
			setTotalVotes(getTotalVotes() - getVotesRequired());
		}

		VotePartyEvent event = new VotePartyEvent();
		Bukkit.getPluginManager().callEvent(event);
		if (event.isCancelled()) {
			finishStoredParty(attempt);
			return;
		}

		if (storedIds != null) {
			dispatchPreparedRewards(user, forceBungee, storedIds,
					() -> plugin.getSpecialRewardsConfig().isVotePartyEnabled() && getTotalVotes() >= getVotesRequired()
					&& (!plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
							|| !plugin.getServerData().isLastVotePartySameDay())
					&& (!plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
							|| !plugin.getServerData().isLastVotePartySameWeek()), () -> {
				if (plugin.getSpecialRewardsConfig().isVotePartyResetCount())
					state.setTotalVotes(getTotalVotes() - getVotesRequired());
				completePartyMetadata();
			}, attempt);
		} else {
			giveRewards(user, forceBungee);
			completePartyMetadata();
			finishStoredParty(attempt);
		}
	}

	private void completePartyMetadata() {
		scalarCommit.lock();
		try {
			if (plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired() > 0) {
				plugin.getServerData().setVotePartyExtraRequired(plugin.getServerData().getVotePartyExtraRequired()
						+ plugin.getSpecialRewardsConfig().getVotePartyIncreaseVotesRequired());
			}

			plugin.getServerData().updateLastVoteParty();

			if (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()) {
				plugin.getServerData().updateLastVotePartyWeek();
			}
		} finally { scalarCommit.unlock(); }
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
			var display = plugin.getServerData().getVotePartyDisplay();
			int votesRequired = display.required(plugin.getSpecialRewardsConfig().getVotePartyVotesRequired());
			int votes = display.total();
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
		giveRewards(orgUser, forceBungee,
				() -> plugin.getLogger().warning("VoteParty force request not started: another party is pending"));
	}

	/**
	 * Requests forced delivery. The busy callback reports rejected admission; returning
	 * from this method does not confirm reward completion.
	 */
	public void giveRewards(VotingPluginUser orgUser, boolean forceBungee, Runnable busy) {
		if (prepareStoredParty((ids, finished) ->
				dispatchPreparedRewards(orgUser, forceBungee, ids, () -> true, () -> { }, finished), busy)) return;
		rewardHandler.giveRewards(orgUser, forceBungee);
		setVotedUsers(new ArrayList<>());
		com.bencodez.advancedcore.api.user.usercache.UserDataManager manager =
				plugin.getUserManager().getDataManager();
		if (manager != null && manager.deferSharedStorageWork(this::resetVotePartyCount)) return;
		resetVotePartyCount();
	}

	/** One pending preparation/reset at a time; its native query is always queued. */
	private boolean prepareStoredParty(BiConsumer<VotePartyRewardHandler.StoredRecipients, Object> action) {
		return prepareStoredParty(action, () -> { });
	}

	private boolean prepareStoredParty(BiConsumer<VotePartyRewardHandler.StoredRecipients, Object> action,
			Runnable busy) {
		if (storedPreparation.get() != null) { busy.run(); return true; }
		if (!rewardHandler.needsStoredPreparation()) return false;
		Object attempt = new Object();
		if (!storedPreparation.compareAndSet(null, attempt)) { busy.run(); return true; }
		try {
			if (rewardHandler.prepareStoredRecipients(ids -> {
				try {
					plugin.getVoteTimer().execute(() -> {
						synchronized (VoteParty.this) {
							stateCommit.lock();
							try {
								if (storedPreparation.get() != attempt) return;
								if (!ids.isCurrent()) { finishStoredParty(attempt); return; }
								try { action.accept(ids, attempt); }
								catch (RuntimeException | Error error) { finishStoredParty(attempt); throw error; }
							} finally { stateCommit.unlock(); }
						}
					});
				} catch (RuntimeException | Error error) {
					finishStoredParty(attempt);
					rewardHandler.logPreparationFailure(error);
				}
			}, error -> {
				if (storedPreparation.get() == attempt) {
					rewardHandler.logPreparationFailure(error);
					finishStoredParty(attempt);
				}
			})) return true;
		} catch (RuntimeException | Error error) {
			finishStoredParty(attempt);
			throw error;
		}
		finishStoredParty(attempt);
		return false;
	}


	private void registerPendingCheck(Object active, long generation, Runnable action) {
		PendingCheck pending = new PendingCheck(active, generation, action);
		pendingStoredCheck.set(pending);
		if (storedPreparation.get() != active && pendingStoredCheck.compareAndSet(pending, null))
			schedulePendingCheck(pending);
	}

	private void finishStoredParty(Object attempt) {
		if (attempt != null && storedPreparation.compareAndSet(attempt, null)) {
			PendingCheck pending = pendingStoredCheck.get();
			if (pending != null && pending.attempt() == attempt
					&& pendingStoredCheck.compareAndSet(pending, null)) schedulePendingCheck(pending);
		}
	}


	private void schedulePendingCheck(PendingCheck pending) {
		try {
			plugin.getVoteTimer().execute(() -> {
				if (pending.generation() == rewardGeneration.get()) pending.action().run();
			});
		} catch (RuntimeException error) {
			pendingStoredCheck.compareAndSet(null, pending);
			rewardHandler.logPreparationFailure(error);
			Runnable fallback = () -> {
				if (pendingStoredCheck.compareAndSet(pending, null)
						&& pending.generation() == rewardGeneration.get()) pending.action().run();
			};
			try {
				var manager = plugin.getUserManager().getDataManager();
				if (!manager.deferSharedStorageWork(fallback) && !manager.isPlatformOwnedThread()) fallback.run();
			} catch (RuntimeException fallbackFailure) {
				rewardHandler.logPreparationFailure(fallbackFailure);
			}
		}
	}

	/** Wait for a contended vote on the worker, then retry platform capture without blocking it. */
	private void retryPreparedDispatch(VotingPluginUser user, boolean forceBungee,
			VotePartyRewardHandler.StoredRecipients ids, BooleanSupplier canCommit,
			Runnable commitState, Object attempt) {
		try {
			plugin.getVoteTimer().execute(() -> {
				synchronized (VoteParty.this) {
					stateCommit.lock();
					try {
						scalarCommit.lock();
						try {
							if (storedPreparation.get() == attempt)
								dispatchPreparedRewards(user, forceBungee, ids, canCommit, commitState, attempt);
						} finally { scalarCommit.unlock(); }
					} finally { stateCommit.unlock(); }
				}
			});
		} catch (RuntimeException error) {
			finishStoredParty(attempt);
			rewardHandler.logPreparationFailure(error);
		}
	}

	/** Commit scalar party state with vote(), then capture platform input on its owner. */
	private void dispatchPreparedRewards(VotingPluginUser user, boolean forceBungee,
			VotePartyRewardHandler.StoredRecipients storedIds, BooleanSupplier canCommit,
			Runnable commitState, Object attempt) {
		try {
			plugin.getBukkitScheduler().runTask(plugin, () -> {
				VotePartyRewardHandler.StoredRecipients dispatch;
				if (!stateCommit.tryLock()) {
					retryPreparedDispatch(user, forceBungee, storedIds, canCommit, commitState, attempt);
					return;
				}
				try {
					if (!scalarCommit.tryLock()) {
						retryPreparedDispatch(user, forceBungee, storedIds, canCommit, commitState, attempt);
						return;
					}
					try {
						if (storedPreparation.get() != attempt || !storedIds.isCurrent()
								|| !rewardHandler.needsStoredPreparation() || !canCommit.getAsBoolean()) {
							finishStoredParty(attempt);
							return;
						}
						dispatch = storedIds.forDispatch();
						List<String> voters = new ArrayList<>(state.getVotedUsers());
						int total = getTotalVotes();
						int extra = plugin.getServerData().getVotePartyExtraRequired();
						int day = plugin.getServerData().getData().getInt("LastVoteParty", 0);
						int week = plugin.getServerData().getData().getInt("LastVotePartyWeek", -1);
						try {
							state.setVotedUsers(new ArrayList<>());
							commitState.run();
							rewardHandler.broadcastParty();
							counterResetAttempt = attempt;
							votesAfterCommit.clear();
						} catch (RuntimeException | Error error) {
							try {
								state.setVotedUsers(voters);
								state.setTotalVotes(total);
								plugin.getServerData().setVotePartyExtraRequired(extra);
								plugin.getServerData().setInt("LastVoteParty", day);
								plugin.getServerData().setInt("LastVotePartyWeek", week);
								finishStoredParty(attempt);
							} catch (RuntimeException | Error rollbackFailure) {
								pendingScalarRollback.set(new ScalarRollback(attempt, voters, total, extra, day, week));
								plugin.getLogger().warning("VoteParty state restoration retained for a later check");
								error.addSuppressed(rollbackFailure);
							}
							rewardHandler.logPreparationFailure(error);
							return;
						}
					} finally { scalarCommit.unlock(); }
				} catch (RuntimeException | Error error) {
					finishStoredParty(attempt);
					rewardHandler.logPreparationFailure(error);
					return;
				} finally { stateCommit.unlock(); }
				try {
					rewardHandler.dispatchRewards(user, forceBungee, dispatch);
					Runnable reset = () -> resetPreparedCounters(dispatch, attempt);
					if (!plugin.getUserManager().getDataManager().deferSharedStorageWork(reset)) {
						pendingCounterReset.set(reset);
						plugin.getLogger().warning("VoteParty counter reset is pending storage admission");
					}
				} catch (RuntimeException | Error error) {
					pendingCounterReset.set(() -> resetPreparedCounters(dispatch, attempt));
					rewardHandler.logPreparationFailure(error);
				}
			});
		} catch (RuntimeException | Error error) {
			finishStoredParty(attempt);
			rewardHandler.logPreparationFailure(error);
		}
	}


	/** Restore a failed pre-dispatch commit before another vote mutates its state. */
	private boolean recoverScalarState() {
		scalarCommit.lock();
		try {
			ScalarRollback rollback = pendingScalarRollback.get();
			if (rollback == null) return true;
			if (storedPreparation.get() != rollback.attempt()) {
				pendingScalarRollback.compareAndSet(rollback, null);
				return true;
			}
			try {
				state.setVotedUsers(rollback.voters());
				state.setTotalVotes(rollback.total());
				plugin.getServerData().setVotePartyExtraRequired(rollback.extra());
				plugin.getServerData().setInt("LastVoteParty", rollback.day());
				plugin.getServerData().setInt("LastVotePartyWeek", rollback.week());
				pendingScalarRollback.compareAndSet(rollback, null);
				finishStoredParty(rollback.attempt());
				return true;
			} catch (RuntimeException error) {
				plugin.getLogger().warning("VoteParty state restoration remains pending");
				return false;
			}
		} finally { scalarCommit.unlock(); }
	}

	private void retryPendingCounterReset() {
		Runnable reset = pendingCounterReset.getAndSet(null);
		if (reset == null) return;
		try {
			plugin.getVoteTimer().execute(() -> {
				try {
					if (!plugin.getUserManager().getDataManager().deferSharedStorageWork(reset)) reset.run();
				} catch (RuntimeException error) {
					pendingCounterReset.compareAndSet(null, reset);
					rewardHandler.logPreparationFailure(error);
				}
			});
		} catch (RuntimeException error) {
			pendingCounterReset.compareAndSet(null, reset);
			rewardHandler.logPreparationFailure(error);
		}
	}

	private void resetPreparedCounters(VotePartyRewardHandler.StoredRecipients dispatch, Object attempt) {
		try {
			synchronized (this) {
				stateCommit.lock();
				try {
					if (!dispatch.isCurrentChecked()) {
						finishStoredParty(attempt);
						return;
					}
					dispatch.withCurrentOwner(() -> {
						if (storedPreparation.get() == attempt && counterResetAttempt == attempt) {
							resetVotePartyCount();
							for (NewPartyVotes delta : votesAfterCommit.values())
								delta.user().setVotePartyVotes(delta.count());
							votesAfterCommit.clear();
							counterResetAttempt = null;
						}
						return null;
					});
				} finally { stateCommit.unlock(); }
			}
			finishStoredParty(attempt);
		} catch (RuntimeException error) {
			if (storedPreparation.get() == attempt)
				pendingCounterReset.set(() -> resetPreparedCounters(dispatch, attempt));
			plugin.getLogger().warning("VoteParty counter reset failed; retained for a later check");
		}
	}

	/**
	 * Handles day change events to reset vote party if configured.
	 *
	 * @param event the day change event
	 */
	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		if (event.getTransition() != null
				&& !plugin.getConfigFile().isAutomaticTimeChangeEnabled(TimeType.DAY)) {
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyResetEachDay()) {
			reset(true);
		}
	}

	/**
	 * Handles month change events to reset vote party if configured.
	 *
	 * @param event the month change event
	 */
	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		if (event.getTransition() != null
				&& !plugin.getConfigFile().isAutomaticTimeChangeEnabled(TimeType.MONTH)) {
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyResetMonthly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesMonthly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
		}
	}

	/**
	 * Handles week change events to reset vote party if configured.
	 *
	 * @param event the week change event
	 */
	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		if (event.getTransition() != null
				&& !plugin.getConfigFile().isAutomaticTimeChangeEnabled(TimeType.WEEK)) {
			return;
		}
		if (plugin.getSpecialRewardsConfig().isVotePartyResetWeekly()) {
			reset(true);
		}

		if (plugin.getSpecialRewardsConfig().isVotePartyResetExtraVotesWeekly()) {
			plugin.getServerData().setVotePartyExtraRequired(0);
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
		scalarCommit.lock();
		try {
			rewardGeneration.incrementAndGet();
			pendingScalarRollback.set(null);
			pendingCounterReset.set(null);
			storedPreparation.set(null);
			pendingStoredCheck.set(null);
			if (override) state.setTotalVotes(0);
			state.setVotedUsers(new ArrayList<>());
		} finally { scalarCommit.unlock(); }
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
		scalarCommit.lock();
		try {
			state.setTotalVotes(value);
		} finally { scalarCommit.unlock(); }
	}

	/**
	 * Sets the voted users.
	 *
	 * @param value the new voted users
	 */
	public void setVotedUsers(List<String> value) {
		scalarCommit.lock();
		try {
			state.setVotedUsers(value);
		} finally { scalarCommit.unlock(); }
	}

	/**
	 * Processes a vote for the vote party system.
	 *
	 * @param user        the voting plugin user
	 * @param realVote    whether this is a real vote
	 * @param forceBungee whether to force Bungee processing
	 */
	public synchronized void vote(VotingPluginUser user, boolean realVote, boolean forceBungee) {
		stateCommit.lock();
		try {
			vote(user, realVote, forceBungee, user.isOnline());
		} finally { stateCommit.unlock(); }
	}

	/**
	 * Processes a vote for the vote party system using the player's online state
	 * when the vote occurred. Proxy deliveries can be replayed after the player
	 * joins, so current online state must not decide whether an originally offline
	 * vote counts.
	 *
	 * @param user             the voting plugin user
	 * @param realVote         whether this is a real vote
	 * @param forceBungee      whether to force Bungee processing
	 * @param onlineAtVoteTime whether the player was online when the vote occurred
	 */
	public synchronized void vote(VotingPluginUser user, boolean realVote, boolean forceBungee,
			boolean onlineAtVoteTime) {
		stateCommit.lock();
		try {
			if (plugin.getSpecialRewardsConfig().isVotePartyEnabled()) {
				if (plugin.getSpecialRewardsConfig().isVotePartyCountFakeVotes() || realVote) {
					if (plugin.getSpecialRewardsConfig().isVotePartyCountOfflineVotes() || onlineAtVoteTime) {
						addTotal(user);
						addVotePlayer(user);
						check(user, forceBungee);
						checkVoteReminder(user);
					}
				}
			}
		} finally { stateCommit.unlock(); }
	}
}
