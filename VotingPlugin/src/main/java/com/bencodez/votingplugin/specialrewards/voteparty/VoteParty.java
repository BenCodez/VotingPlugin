
package com.bencodez.votingplugin.specialrewards.voteparty;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
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
import com.bencodez.simpleapi.folialib.impl.PlatformScheduler;
import com.bencodez.simpleapi.folialib.wrapper.task.WrappedTask;
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
	private static final class StoredAttempt {
		final long generation;
		StoredAttempt(long generation) { this.generation = generation; }
		boolean eventApproved;
		boolean committed;
		Runnable refresh;
		volatile PendingCheck continuation;
		volatile PendingCheck finishing;
		PlatformCommit platformCommit;
		boolean retryCommit;
		final HashMap<String, NewPartyVotes> nextPartyVotes = new HashMap<>();
		Runnable followup;
		int includedContributions;
		boolean completed;
	}
	private static final class PlatformCommit {
		final PlatformScheduler owner;
		volatile WrappedTask task;
		PlatformCommit(PlatformScheduler owner) { this.owner = owner; }
	}

	// Guarded by scalarCommit; entries belong to thresholds waiting behind an active party.
	private record Contribution(VotingPluginUser user, String uuid, AdmissionSlot slot, long sequence) { }
	private final AtomicLong contributionSequence = new AtomicLong();
	private final ArrayDeque<Contribution> waitingContributions = new ArrayDeque<>();
	private volatile boolean retainedPreparationBoundary;
	boolean hasRetainedPreparationBoundary() { return retainedPreparationBoundary; }
	// Positive values are admitted ingress scopes; -1 is a platform scalar commit.
	// Both sides try once and defer rather than wait on a platform owner.
	private final AtomicInteger ingressCommitFence = new AtomicInteger();
	private final AtomicReference<PendingCheck> scalarRecovery = new AtomicReference<>();
	// Two slots per admitted vote: one contribution and one explicit check.
	private final Semaphore admissions = new Semaphore(512);
	// Cache-only ownership fence. Never call storage, actions or schedulers under it.
	private final Object admissionOwnership = new Object();
	private final Set<AdmissionSlot> liveAdmissions = new HashSet<>();
	private record AdmissionContext(ArrayDeque<AdmissionSlot> slots, long generation, boolean enabled, boolean automatic, AtomicBoolean dispatched) { }
	private final ThreadLocal<AdmissionContext> admissionScope = new ThreadLocal<>();
	private final class AdmissionSlot implements AutoCloseable {
		private final AtomicBoolean released = new AtomicBoolean();
		AdmissionSlot() { liveAdmissions.add(this); }
		@Override public void close() {
			synchronized (admissionOwnership) {
				if (released.compareAndSet(false, true)) {
					liveAdmissions.remove(this);
					admissions.release();
				}
			}
		}
	}

	/**
	 * Internal ingress adapter. False means the action was not invoked. Callers
	 * must retain their original operation and must not report it completed.
	 * Slots transferred to pending party records outlive this dispatch scope.
	 * Admission only uses tryAcquire; it performs no storage or platform wait.
	 */
	public boolean tryWithVoteAdmission(Runnable action) {
		boolean enabled = plugin.getSpecialRewardsConfig().isVotePartyEnabled();
		if (enabled) {
			int ingress = ingressCommitFence.get();
			if (ingress < 0 || !ingressCommitFence.compareAndSet(ingress, ingress + 1)) return false;
		}
		try {
			if (enabled && pendingScalarRollback.get() != null) {
				requestScalarRecovery();
				return false;
			}
			ArrayDeque<AdmissionSlot> slots = new ArrayDeque<>();
			long generation;
			synchronized (admissionOwnership) {
				if (enabled && !admissions.tryAcquire(2)) {
					generation = -1;
				} else generation = rewardGeneration.get();
				if (enabled && generation >= 0) {
					slots.add(new AdmissionSlot());
					slots.add(new AdmissionSlot());
				}
			}
			if (generation < 0) {
				wakeRetainedCheck();
				return false;
			}
			runWithAdmission(slots, generation, enabled, false, action);
			return true;
		} finally {
			if (enabled) ingressCommitFence.decrementAndGet();
		}
	}

	/** Explicit refusal before event dispatch, distinct from failures after effects. */
	public static final class AdmissionRejectedException extends RejectedExecutionException {
		private static final long serialVersionUID = 1L;
		public AdmissionRejectedException() { super("Vote was not admitted before event dispatch"); }
	}

	/**
	 * Claims one event dispatch from an ingress reservation. A nested event needs
	 * its own reservation; a layered adapter for the same event reuses the still
	 * unclaimed pair. False means the event action was never invoked.
	 */
	public boolean tryDispatchVoteEvent(Runnable action) {
		AdmissionContext scope = admissionScope.get();
		if (scope != null) {
			if (staleAdmission()) return false;
			if ((!scope.enabled() || scope.slots().size() >= 2)
					&& scope.dispatched().compareAndSet(false, true)) {
				action.run();
				return true;
			}
		}
		return tryWithVoteAdmission(() -> {
			admissionScope.get().dispatched().set(true);
			action.run();
		});
	}

	private void requestScalarRecovery() {
		ScalarRollback rollback = pendingScalarRollback.get();
		if (rollback == null) return;
		PendingCheck recovery = scalarRecovery.get();
		if (recovery == null || recovery.attempt() != rollback.attempt()) {
			PendingCheck replacement = new PendingCheck(rollback.attempt(), rewardGeneration.get(), () -> {
				if (storedPreparation.get() == rollback.attempt()) recoverScalarState();
			});
			if (!scalarRecovery.compareAndSet(recovery, replacement)) return;
			recovery = replacement;
		}
		schedulePendingCheck(recovery);
	}

	private void runWithAdmission(ArrayDeque<AdmissionSlot> slots, long generation, boolean enabled, boolean automatic, Runnable action) {
		AdmissionContext previous = admissionScope.get();
		admissionScope.set(new AdmissionContext(slots, generation, enabled, automatic, new AtomicBoolean()));
		try { action.run(); }
		finally {
			admissionScope.remove();
			if (previous != null) admissionScope.set(previous);
			for (AdmissionSlot slot : slots) slot.close();
		}
	}

	private boolean staleAdmission() {
		AdmissionContext scope = admissionScope.get();
		return scope != null && scope.generation() != rewardGeneration.get();
	}

	private AdmissionSlot takeAdmission() {
		synchronized (admissionOwnership) {
			AdmissionContext scope = admissionScope.get();
			if (scope != null) {
				while (!scope.slots().isEmpty()) {
					AdmissionSlot slot = scope.slots().removeFirst();
					if (!slot.released.get()) return slot;
				}
			}
			if (!admissions.tryAcquire())
				throw new RejectedExecutionException("VoteParty work was not admitted; retry before applying vote effects");
			return new AdmissionSlot();
		}
	}

	private Object counterResetAttempt;
	private record ScalarRollback(Object attempt, List<String> voters, int total, int extra, int day, int week) { }
	private final AtomicReference<ScalarRollback> pendingScalarRollback = new AtomicReference<>();
	private final AtomicReference<Runnable> pendingCounterReset = new AtomicReference<>();
	private volatile PendingCheck resetWakeup;

	long rewardGeneration() { return rewardGeneration.get(); }

	<T> T withPreparedVoteFence(Supplier<T> work) {
		synchronized (this) {
			stateCommit.lock();
			try { return work.get(); }
			finally { stateCommit.unlock(); }
		}
	}

	int preparedVotes(VotingPluginUser user) {
		Object active = storedPreparation.get();
		NewPartyVotes delta = active instanceof StoredAttempt attempt
				? attempt.nextPartyVotes.get(user.getUUID()) : null;
		return Math.max(0, user.getVotePartyVotes() - (delta == null ? 0 : delta.count()));
	}


	private final AtomicReference<Object> storedPreparation = new AtomicReference<>();
	private record PendingCheck(Object attempt, long generation, Runnable action, AdmissionSlot slot, boolean storage) {
		PendingCheck(Object attempt, long generation, Runnable action) { this(attempt, generation, action, null, false); }
		PendingCheck(Object attempt, long generation, Runnable action, AdmissionSlot slot) {
			this(attempt, generation, action, slot, false);
		}
	}
	private final AtomicReference<PendingCheck> pendingStoredCheck = new AtomicReference<>();
	private final ConcurrentLinkedQueue<PendingCheck> pendingNonResetChecks = new ConcurrentLinkedQueue<>();
	private final Set<PendingCheck> scheduledChecks = new HashSet<>();
	private record CheckWakeup(ScheduledExecutorService owner, ScheduledExecutorService executor, Future<?> future) { }
	private final HashMap<PendingCheck, CheckWakeup> checkWakeups = new HashMap<>();
	private final ThreadLocal<ArrayDeque<PendingCheck>> drainingChecks = new ThreadLocal<>();
	private PendingCheck automaticWakeup;
	private volatile PendingCheck retainedCheck;
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
		if (staleAdmission()) return;
		AdmissionSlot admission = takeAdmission();
		boolean transferred = false;
		stateCommit.lock();
		try {
			if (!recoverScalarState()) throw new IllegalStateException("VoteParty state recovery is still unavailable");
			String uuid = user.getUUID();
			scalarCommit.lock();
			try { state.setTotalVotes(getTotalVotes() + 1); }
			finally { scalarCommit.unlock(); }
			user.setVotePartyVotes(user.getVotePartyVotes() + 1);
			scalarCommit.lock();
			try {
				if (!admission.released.get() && retainedPreparationBoundary) {
					waitingContributions.addLast(new Contribution(user, uuid, admission, contributionSequence.incrementAndGet()));
					transferred = true;
					if (storedPreparation.get() instanceof StoredAttempt attempt) addContribution(attempt, user, uuid);
				}
			} finally { scalarCommit.unlock(); }
			plugin.getPlaceholders().onVotePartyUpdate();
		} finally {
			stateCommit.unlock();
			if (!transferred) admission.close();
		}
	}

	/**
	 * Adds the vote player.
	 *
	 * @param user the user
	 */
	public void addVotePlayer(VotingPluginUser user) {
		String uuid = user.getUUID();
		scalarCommit.lock();
		try {
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
		check(user, forceBungee, contributionSequence.get());
	}

	private synchronized void check(VotingPluginUser user, boolean forceBungee, long contributionCutoff) {
		if (staleAdmission()) return;
		long generation = rewardGeneration.get();
		stateCommit.lock();
		try {
			if (generation != rewardGeneration.get()) return;
			if (!recoverScalarState()) return;
			Object active = storedPreparation.get();
			if (active != null) {
				AdmissionContext scope = admissionScope.get();
				if (scope != null && scope.automatic()) return;
				retryPendingCounterReset();
				if (plugin.getSpecialRewardsConfig().isVotePartyResetCount() && getVotesRequired() > 0)
					registerPendingCheck(active, generation, () -> check(user, forceBungee));
				else {
					PendingCheck dispatch = null;
					synchronized (admissionOwnership) {
						if (generation != rewardGeneration.get()) return;
						PendingCheck pending = new PendingCheck(active, generation,
								() -> check(user, forceBungee, contributionCutoff), takeAdmission());
						pendingNonResetChecks.add(pending);
						if (storedPreparation.get() != active && pendingNonResetChecks.remove(pending)) dispatch = pending;
					}
					if (dispatch != null) schedulePendingCheck(dispatch);
				}
				wakeAttemptContinuation(active);
				return;
			}
			// A period-limited party cannot use stored recipients. Avoid occupying
			// the persistence worker with an enumeration that cannot start a party.
			if ((plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
					&& plugin.getServerData().isLastVotePartySameDay())
					|| (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
							&& plugin.getServerData().isLastVotePartySameWeek())) return;
			if (getTotalVotes() >= getVotesRequired()
					&& prepareStoredParty((ids, finished) -> checkPrepared(user, forceBungee, ids, finished),
							() -> { }, () -> check(user, forceBungee), contributionCutoff)) return;
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

		if (!(attempt instanceof StoredAttempt current) || !current.eventApproved) {
			VotePartyEvent event = new VotePartyEvent();
			Bukkit.getPluginManager().callEvent(event);
			if (event.isCancelled()) {
				finishStoredParty(attempt);
				return;
			}
			if (attempt instanceof StoredAttempt approved) approved.eventApproved = true;
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
		retryPendingCounterReset();
		Object continuation = storedPreparation.get();
		wakeAttemptContinuation(continuation);
		if (pendingScalarRollback.get() != null) requestScalarRecovery();
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
		return prepareStoredParty(action, busy, null);
	}

	private static void addContribution(StoredAttempt attempt, VotingPluginUser user, String uuid) {
		NewPartyVotes previous = attempt.nextPartyVotes.get(uuid);
		attempt.nextPartyVotes.put(uuid, new NewPartyVotes(user, previous == null ? 1 : previous.count() + 1));
	}

	private boolean prepareStoredParty(BiConsumer<VotePartyRewardHandler.StoredRecipients, Object> action,
			Runnable busy, Runnable followup) {
		return prepareStoredParty(action, busy, followup, contributionSequence.get());
	}

	private boolean prepareStoredParty(BiConsumer<VotePartyRewardHandler.StoredRecipients, Object> action,
			Runnable busy, Runnable followup, long contributionCutoff) {
		if (storedPreparation.get() != null) { busy.run(); return true; }
		if (!rewardHandler.needsStoredPreparation()) return false;
		StoredAttempt attempt = new StoredAttempt(rewardGeneration.get());
		attempt.followup = followup;
		if (followup != null) retainedCheck = new PendingCheck(attempt, attempt.generation, followup);
		scalarCommit.lock();
		try {
			if (staleAdmission() || attempt.generation != rewardGeneration.get()) return true;
			if (!storedPreparation.compareAndSet(null, attempt)) { busy.run(); return true; }
			retainedPreparationBoundary = true;
			int baselineVotes = Math.max(0, getTotalVotes() - waitingContributions.size());
			if (followup == null || !plugin.getSpecialRewardsConfig().isVotePartyResetCount() || getVotesRequired() <= 0) {
				for (Contribution contribution : waitingContributions)
					if (contribution.sequence() <= contributionCutoff) attempt.includedContributions++;
			} else attempt.includedContributions = Math.min(waitingContributions.size(),
					Math.max(0, getVotesRequired() - baselineVotes));
			int index = 0;
			for (Contribution contribution : waitingContributions) {
				if (index++ >= attempt.includedContributions) addContribution(attempt, contribution.user(), contribution.uuid());
			}
		} finally { scalarCommit.unlock(); }
		attempt.refresh = () -> submitStoredPreparation(attempt, action);
		return submitStoredPreparation(attempt, action);
	}

	private boolean submitStoredPreparation(StoredAttempt attempt,
			BiConsumer<VotePartyRewardHandler.StoredRecipients, Object> action) {
		if (staleAdmission() || storedPreparation.get() != attempt) return true;
		try {
			if (rewardHandler.prepareStoredRecipients(ids -> {
				PendingCheck ready = new PendingCheck(attempt, attempt.generation, () -> {
					if (storedPreparation.get() != attempt) return;
					if (!ids.isCurrent()) { finishStoredParty(attempt); return; }
					attempt.continuation = null;
					try { action.accept(ids, attempt); }
					catch (RuntimeException | Error error) { finishStoredParty(attempt); throw error; }
				});
				attempt.continuation = ready;
				schedulePendingCheck(ready);
			}, error -> {
				if (storedPreparation.get() == attempt) {
					rewardHandler.logPreparationFailure(error);
					finishStoredParty(attempt);
				}
			}, attempt.generation)) return true;
		} catch (RuntimeException | Error error) {
			finishStoredParty(attempt);
			throw error;
		}
		finishStoredParty(attempt);
		return false;
	}


	private void registerPendingCheck(Object active, long generation, Runnable action) {
		PendingCheck dispatch = null;
		synchronized (admissionOwnership) {
			if (generation != rewardGeneration.get()) return;
			PendingCheck pending = new PendingCheck(active, generation, action, takeAdmission());
			PendingCheck previous = pendingStoredCheck.getAndSet(pending);
			if (previous != null) releaseCheck(previous);
			if (storedPreparation.get() != active && pendingStoredCheck.compareAndSet(pending, null)) dispatch = pending;
		}
		if (dispatch != null) schedulePendingCheck(dispatch);
	}

	/** Unspent credits become the next preparation's baseline; counters stay intact. */
	private void retireBaselineContributions() {
		scalarCommit.lock();
		try {
			if (counterResetAttempt != null) return;
			boolean limited = (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerDay()
					&& plugin.getServerData().isLastVotePartySameDay())
					|| (plugin.getSpecialRewardsConfig().isVotePartyOnlyOncePerWeek()
							&& plugin.getServerData().isLastVotePartySameWeek());
			if (getTotalVotes() >= getVotesRequired() && !limited) return;
			for (Contribution contribution : waitingContributions) contribution.slot().close();
			waitingContributions.clear();
			retainedPreparationBoundary = false;
			retainedCheck = null;
		} finally { scalarCommit.unlock(); }
	}

	private void finishStoredParty(Object attempt) {
		if (attempt == null) return;
		PendingCheck next = null;
		var manager = plugin.getUserManager().getDataManager();
		if (manager != null && manager.isPlatformOwnedThread()) {
			if (!scalarCommit.tryLock()) {
				if (attempt instanceof StoredAttempt current && storedPreparation.get() == current) {
					synchronized (admissionOwnership) {
						if (current.finishing == null) current.finishing = new PendingCheck(current,
								current.generation, () -> finishStoredParty(current));
					}
					wakeAttemptContinuation(current);
				}
				return;
			}
		} else scalarCommit.lock();
		try {
			if (storedPreparation.get() != attempt) return;
			retireBaselineContributions();
			PendingCheck pending = pendingStoredCheck.get();
			if (pending != null && pending.attempt() == attempt
					&& pendingStoredCheck.compareAndSet(pending, null)) next = pending;
			else next = pendingNonResetChecks.poll();
			if (next == null && attempt instanceof StoredAttempt completed && completed.completed
					&& completed.followup != null && plugin.getSpecialRewardsConfig().isVotePartyResetCount()
					&& getVotesRequired() > 0 && getTotalVotes() >= getVotesRequired())
				next = new PendingCheck(attempt, completed.generation, completed.followup);
			// Reserve the handover before releasing the old attempt. New checks
			// must queue behind the accepted continuation rather than overtake it.
			if (!storedPreparation.compareAndSet(attempt, next)) return;
		} finally { scalarCommit.unlock(); }
		if (next != null) schedulePendingCheck(next);
	}


	private void wakeAttemptContinuation(Object active) {
		PendingCheck pending = active instanceof PendingCheck check ? check
				: active instanceof StoredAttempt attempt
						? (attempt.finishing != null ? attempt.finishing : attempt.continuation) : null;
		if (pending != null && storedPreparation.get() == active) schedulePendingCheck(pending);
	}

	private void wakeRetainedCheck() {
		PendingCheck pending = retainedCheck;
		if (pending == null || pending.generation() != rewardGeneration.get()
				|| !retainedPreparationBoundary || !plugin.getSpecialRewardsConfig().isVotePartyEnabled()) return;
		if (storedPreparation.compareAndSet(null, pending)) schedulePendingCheck(pending);
	}

	private void releaseCheck(PendingCheck pending) {
		if (pending.slot() != null) pending.slot().close();
	}

	private void runPendingCheck(PendingCheck pending) {
		ArrayDeque<PendingCheck> drain = drainingChecks.get();
		if (drain != null) { drain.addLast(pending); return; }
		drain = new ArrayDeque<>();
		drainingChecks.set(drain);
		try {
			drain.addLast(pending);
			while (!drain.isEmpty()) runClaimedPendingCheck(drain.removeFirst());
		} finally { drainingChecks.remove(); }
	}

	private void runClaimedPendingCheck(PendingCheck pending) {
		synchronized (admissionOwnership) {
			if (!scheduledChecks.remove(pending)) return;
			checkWakeups.remove(pending);
			if (automaticWakeup == pending) automaticWakeup = null;
			if (pending.generation() != rewardGeneration.get()) { releaseCheck(pending); return; }
		}
		synchronized (this) {
			stateCommit.lock();
			try {
				if (pending.generation() != rewardGeneration.get()) { releaseCheck(pending); return; }
				storedPreparation.compareAndSet(pending, null);
				ArrayDeque<AdmissionSlot> slots = new ArrayDeque<>();
				if (pending.slot() != null) slots.add(pending.slot());
				runWithAdmission(slots, pending.generation(), true, pending.slot() == null, pending.action());
				PendingCheck coalesced = pendingStoredCheck.get();
				Object active = storedPreparation.get();
				if (coalesced != null && coalesced.attempt() == pending) {
					PendingCheck transferred = new PendingCheck(active, coalesced.generation(),
							coalesced.action(), coalesced.slot());
					if (pendingStoredCheck.compareAndSet(coalesced, transferred) && active == null) {
						pendingStoredCheck.compareAndSet(transferred, null);
						schedulePendingCheck(transferred);
					}
				} else if (active == null) {
					PendingCheck next = pendingNonResetChecks.poll();
					if (next != null && storedPreparation.compareAndSet(null, next)) schedulePendingCheck(next);
				}
			} finally { stateCommit.unlock(); }
		}
	}

	private void schedulePendingCheck(PendingCheck pending) {
		ScheduledExecutorService persistence = plugin.getUserManager().getDataManager().getTimer();
		ScheduledExecutorService owner = pending.storage() ? persistence : plugin.getVoteTimer();
		CheckWakeup reservation = new CheckWakeup(owner, owner, null);
		synchronized (admissionOwnership) {
			if (pending.generation() != rewardGeneration.get()) { releaseCheck(pending); return; }
			if (pending.slot() == null) {
				// Automatic wakeups coalesce. Explicit leased checks keep their own records.
				if (automaticWakeup != null && automaticWakeup != pending) {
					scheduledChecks.remove(automaticWakeup);
					checkWakeups.remove(automaticWakeup);
				}
				automaticWakeup = pending;
			}
			scheduledChecks.add(pending);
			CheckWakeup previous = checkWakeups.get(pending);
			if (previous != null && previous.owner() == owner && previous.executor() != null
					&& (previous.executor() == owner || previous.executor() == persistence)
					&& !previous.executor().isShutdown()
					&& (previous.future() == null || !previous.future().isDone())) return;
			checkWakeups.put(pending, reservation);
		}
		Runnable dispatch = () -> runPendingCheck(pending);
		try {
			Future<?> future = owner.schedule(dispatch, 0, TimeUnit.MILLISECONDS);
			synchronized (admissionOwnership) {
				checkWakeups.replace(pending, reservation, new CheckWakeup(owner, owner, future));
			}
		} catch (RuntimeException error) {
			rewardHandler.logPreparationFailure(error);
			try {
				if (pending.storage()) throw error; // Preserve order behind queued eligibility reads.
				Future<?> future = persistence.schedule(dispatch, 0, TimeUnit.MILLISECONDS);
				synchronized (admissionOwnership) {
					checkWakeups.replace(pending, reservation, new CheckWakeup(owner, persistence, future));
				}
				return;
			} catch (RuntimeException fallbackFailure) {
				rewardHandler.logPreparationFailure(fallbackFailure);
			}
			// Retain the record, but allow a later request to retry its wakeup.
			synchronized (admissionOwnership) { checkWakeups.remove(pending, reservation); }
		}
	}

	/** Wait for a contended vote on the worker, then retry platform capture without blocking it. */
	private void retryPreparedDispatch(VotingPluginUser user, boolean forceBungee,
			VotePartyRewardHandler.StoredRecipients ids, BooleanSupplier canCommit,
			Runnable commitState, Object attempt) {
		if (!(attempt instanceof StoredAttempt current) || storedPreparation.get() != current) return;
		synchronized (admissionOwnership) {
			if (current.continuation == null) current.continuation = new PendingCheck(current, current.generation,
					() -> {
						if (storedPreparation.get() == current)
							dispatchPreparedRewards(user, forceBungee, ids, canCommit, commitState, current);
					});
		}
		boolean wake;
		synchronized (admissionOwnership) {
			current.retryCommit = true;
			wake = current.platformCommit == null;
		}
		if (wake) wakeAttemptContinuation(current);
	}

	private void schedulePlatformCommit(StoredAttempt attempt, Runnable action) {
		PlatformScheduler owner = plugin.getBukkitScheduler().getFoliaLib().getScheduler();
		PlatformCommit ticket = new PlatformCommit(owner);
		synchronized (admissionOwnership) {
			if (storedPreparation.get() != attempt || attempt.generation != rewardGeneration.get() || attempt.committed) return;
			PlatformCommit previous = attempt.platformCommit;
			if (previous != null && previous.owner == owner && (previous.task == null || !previous.task.isCancelled())) return;
			attempt.platformCommit = ticket;
		}
		try {
			// One tick retains the same global-owner/next-tick semantics as runTask,
			// while retaining the cancellation handle that its void wrapper discards.
			ticket.task = owner.runLater(() -> {
				synchronized (admissionOwnership) {
					if (attempt.platformCommit != ticket || storedPreparation.get() != attempt
							|| attempt.generation != rewardGeneration.get()) return;
				}
				try { action.run(); }
				finally {
					boolean retry = false;
					synchronized (admissionOwnership) {
						if (attempt.platformCommit == ticket) {
							attempt.platformCommit = null;
							retry = attempt.retryCommit;
							attempt.retryCommit = false;
						}
					}
					if (retry) wakeAttemptContinuation(attempt);
				}
			}, 1L);
			if (ticket.task == null) throw new RejectedExecutionException("VoteParty platform commit was not admitted");
		} catch (RuntimeException | Error error) {
			synchronized (admissionOwnership) {
				if (attempt.platformCommit == ticket) attempt.platformCommit = null;
			}
			throw error;
		}
	}

	/** Commit scalar party state with vote(), then capture platform input on its owner. */
	private void dispatchPreparedRewards(VotingPluginUser user, boolean forceBungee,
			VotePartyRewardHandler.StoredRecipients storedIds, BooleanSupplier canCommit,
			Runnable commitState, Object attempt) {
		if (!(attempt instanceof StoredAttempt currentAttempt)) return;
		synchronized (admissionOwnership) {
			if (currentAttempt.continuation == null) currentAttempt.continuation = new PendingCheck(attempt,
					currentAttempt.generation, () -> dispatchPreparedRewards(user, forceBungee, storedIds,
							canCommit, commitState, attempt));
		}
		try {
			schedulePlatformCommit(currentAttempt, () -> {
				VotePartyRewardHandler.StoredRecipients dispatch;
				if (!stateCommit.tryLock()) {
					retryPreparedDispatch(user, forceBungee, storedIds, canCommit, commitState, attempt);
					return;
				}
				boolean commitOwned = false;
				try {
					if (!ingressCommitFence.compareAndSet(0, -1)) {
						retryPreparedDispatch(user, forceBungee, storedIds, canCommit, commitState, attempt);
						return;
					}
					commitOwned = true;
					if (!scalarCommit.tryLock()) {
						retryPreparedDispatch(user, forceBungee, storedIds, canCommit, commitState, attempt);
						return;
					}
					try {
						if (attempt instanceof StoredAttempt current && current.committed) return;
						if (storedPreparation.get() != attempt || !storedIds.isCurrent()
								|| !rewardHandler.needsStoredPreparation() || !canCommit.getAsBoolean()) {
							finishStoredParty(attempt);
							return;
						}
						try { dispatch = storedIds.forDispatch(); }
						catch (VotePartyRewardHandler.RecipientEnumerationRequired changed) {
							StoredAttempt current = (StoredAttempt) attempt;
							schedulePendingCheck(new PendingCheck(attempt, current.generation, current.refresh));
							return;
						}
						List<String> voters = new ArrayList<>(state.getVotedUsers());
						int total = getTotalVotes();
						int extra = plugin.getServerData().getVotePartyExtraRequired();
						int day = plugin.getServerData().getData().getInt("LastVoteParty", 0);
						int week = plugin.getServerData().getData().getInt("LastVotePartyWeek", -1);
						try {
							ArrayList<String> nextVoters = new ArrayList<>();
							int index = 0;
							for (Contribution contribution : waitingContributions) {
								if (index++ >= ((StoredAttempt) attempt).includedContributions
										&& contribution.uuid() != null && !nextVoters.contains(contribution.uuid()))
									nextVoters.add(contribution.uuid());
							}
							state.setVotedUsers(nextVoters);
							commitState.run();
							rewardHandler.broadcastParty();
							counterResetAttempt = attempt;
							for (int i = 0; i < ((StoredAttempt) attempt).includedContributions; i++)
								waitingContributions.removeFirst().slot().close();
							((StoredAttempt) attempt).committed = true;
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
				} finally {
					if (commitOwned) ingressCommitFence.set(0);
					stateCommit.unlock();
				}
				try {
					Runnable reset = () -> resetPreparedCounters(dispatch, attempt);
					pendingCounterReset.set(reset);
					if (attempt instanceof StoredAttempt current) current.continuation = null;
					rewardHandler.dispatchRewards(user, forceBungee, dispatch);
					retryPendingCounterReset();
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
		Runnable reset = pendingCounterReset.get();
		Object active = storedPreparation.get();
		if (reset == null || !(active instanceof StoredAttempt attempt)) return;
		PendingCheck wakeup;
		synchronized (admissionOwnership) {
			if (pendingCounterReset.get() != reset || storedPreparation.get() != attempt) return;
			if (resetWakeup == null || resetWakeup.action() != reset) {
				resetWakeup = new PendingCheck(attempt, attempt.generation, reset, null, true);
			}
			wakeup = resetWakeup;
		}
		schedulePendingCheck(wakeup);
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
							for (NewPartyVotes delta : ((StoredAttempt) attempt).nextPartyVotes.values())
								delta.user().setVotePartyVotes(delta.count());
							((StoredAttempt) attempt).nextPartyVotes.clear();
							counterResetAttempt = null;
							pendingCounterReset.set(null);
							resetWakeup = null;
							((StoredAttempt) attempt).completed = true;
							scalarCommit.lock();
							try { retainedPreparationBoundary = !waitingContributions.isEmpty(); }
							finally { scalarCommit.unlock(); }
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
			setExtraVotesRequired(0);
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
			setExtraVotesRequired(0);
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
			synchronized (admissionOwnership) {
				rewardGeneration.incrementAndGet();
				for (AdmissionSlot slot : new ArrayList<>(liveAdmissions)) slot.close();
				scheduledChecks.clear();
				checkWakeups.clear();
				automaticWakeup = null;
				pendingNonResetChecks.clear();
				retainedCheck = null;
				pendingStoredCheck.set(null);
				pendingScalarRollback.set(null);
				scalarRecovery.set(null);
				pendingCounterReset.set(null);
				resetWakeup = null;
				storedPreparation.set(null);
				counterResetAttempt = null;
				retainedPreparationBoundary = false;
			}
			for (Contribution contribution : waitingContributions) contribution.slot().close();
			waitingContributions.clear();
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
			ScalarRollback rollback = pendingScalarRollback.get();
			if (rollback != null) pendingScalarRollback.set(new ScalarRollback(rollback.attempt(),
					rollback.voters(), value, rollback.extra(), rollback.day(), rollback.week()));
		} finally { scalarCommit.unlock(); }
	}

	/** Sets extra required votes without allowing a retained rollback to discard the edit. */
	public void setExtraVotesRequired(int value) {
		scalarCommit.lock();
		try {
			plugin.getServerData().setVotePartyExtraRequired(value);
			ScalarRollback rollback = pendingScalarRollback.get();
			if (rollback != null) pendingScalarRollback.set(new ScalarRollback(rollback.attempt(),
					rollback.voters(), rollback.total(), value, rollback.day(), rollback.week()));
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
			ScalarRollback rollback = pendingScalarRollback.get();
			if (rollback != null) pendingScalarRollback.set(new ScalarRollback(rollback.attempt(),
					new ArrayList<>(value), rollback.total(), rollback.extra(), rollback.day(), rollback.week()));
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
		AdmissionContext scope = admissionScope.get();
		if (staleAdmission() || (scope != null && !scope.enabled())
				|| !plugin.getSpecialRewardsConfig().isVotePartyEnabled()) return;
		if (scope == null || scope.slots().size() < 2) {
			if (!tryWithVoteAdmission(() -> vote(user, realVote, forceBungee, onlineAtVoteTime)))
				throw new RejectedExecutionException("VoteParty vote was not admitted; no counters changed");
			return;
		}
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
