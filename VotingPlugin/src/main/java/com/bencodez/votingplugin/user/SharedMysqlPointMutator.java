package com.bencodez.votingplugin.user;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.IntFunction;

import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.usercache.UserDataCache;
import com.bencodez.advancedcore.api.user.userstorage.mysql.MySQL;
import com.bencodez.simpleapi.sql.mysql.DbType;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.BukkitCompletionScheduler;

/** Performs point writes that must remain atomic across shared MySQL servers. */
final class SharedMysqlPointMutator {
	private final VotingPluginMain plugin;

	SharedMysqlPointMutator(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	boolean applies() {
		return usesSharedMysqlPoints(plugin);
	}

	static boolean usesSharedMysqlPoints(VotingPluginMain plugin) {
		return plugin != null && UserStorage.MYSQL.equals(plugin.getStorageType())
				&& !plugin.getBungeeSettings().isPerServerPoints();
	}

	/**
	 * Recovers a bounded batch immediately and periodically. The executor belongs
	 * to the plugin lifecycle, so no independent task survives shutdown.
	 */
	static void scheduleTransferRecovery(VotingPluginMain plugin) {
		plugin.getTimer().execute(() -> recoverTransfers(plugin));
		plugin.getTimer().scheduleWithFixedDelay(() -> recoverTransfers(plugin), 1L, 1L, TimeUnit.MINUTES);
	}

	private static void recoverTransfers(VotingPluginMain plugin) {
		if (!usesSharedMysqlPoints(plugin)) return;
		try {
			recoverTransfers(plugin, SharedPointTransferJournal.forTable(plugin.getMysql()));
		} catch (SQLException failure) {
			plugin.getLogger().severe("Unable to recover shared MySQL point transfers: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	static void recoverTransfers(VotingPluginMain plugin, SharedPointTransferJournal journal)
			throws SQLException {
		retryPendingCompensationMarkers(plugin, journal);
		for (SharedPointTransferJournal.RefundedTransfer refund : journal.recoverAndCleanup(System.currentTimeMillis())) {
			SharedMysqlCacheReconciler.invalidate(plugin, refund.uuid(), refund.pointsColumn());
		}
	}

	int add(VotingPluginUser user, int amount, boolean async) {
		if (async) {
			int previousTotal = cachedPoints(user);
			int predictedTotal = previousTotal + amount;
			cachePredictedPoints(user, predictedTotal);
			if (!run(() -> update(user, amount, false), true)) {
				discardPointsCache(user);
				return previousTotal;
			}
			// The mutation has not happened yet, so the historical asynchronous API
			// returns its predicted post-event total without blocking for storage.
			return predictedTotal;
		}
		return addAndReadCommitted(user, amount);
	}

	private void cachePredictedPoints(VotingPluginUser user, int predictedTotal) {
		UserDataCache cache = user.getCache();
		if (cache == null) return;
		synchronized (cache) {
			var values = cache.getCache();
			if (values != null) values.put(user.getPointsPath(), new DataValueInt(predictedTotal));
		}
	}

	AddResult addCommitted(VotingPluginUser user, int amount) {
		return addAndReadCommittedResult(user, amount);
	}

	void set(VotingPluginUser user, int value, boolean async) {
		run(() -> setAbsolute(user, value), async);
	}

	boolean setCommitted(VotingPluginUser user, int value) {
		return setAbsolute(user, value);
	}

	void cap(VotingPluginUser user, int maximum, boolean async) {
		run(() -> capAt(user, maximum), async);
	}

	/**
	 * Adds points and applies the configured upper bound in one accepted
	 * persistence operation. This prevents executor saturation from accepting
	 * the addition while dropping a separately submitted cap.
	 */
	void addAndCap(VotingPluginUser user, int amount, int maximum, boolean async) {
		if (!async) {
			addAndCapAt(user, amount, maximum);
			return;
		}
		int previousTotal = cachedPoints(user);
		int predictedTotal = (int) Math.max(Integer.MIN_VALUE,
				Math.min((long) previousTotal + amount, maximum));
		cachePredictedPoints(user, predictedTotal);
		if (!run(() -> addAndCapAt(user, amount, maximum), true)) {
			discardPointsCache(user);
		}
	}

	boolean remove(VotingPluginUser user, int amount) {
		return update(user, -amount, true);
	}

	boolean remove(VotingPluginUser user, int amount, boolean async) {
		if (!async) return remove(user, amount);
		boolean predictedSuccess = cachedPoints(user) >= amount;
		boolean submitted = run(() -> update(user, -amount, true), true);
		// Preserve the historical asynchronous API contract: the caller receives
		// the cached prediction while the conditional database debit runs later.
		return submitted && predictedSuccess;
	}

	private int cachedPoints(VotingPluginUser user) {
		String path = user.getPointsPath();
		UserDataCache cache = user.getCache();
		if (cache != null) {
			synchronized (cache) {
				DataValue value = cache.getCache() == null ? null : cache.getCache().get(path);
				if (value != null) {
					if (value.isInt()) return value.getInt();
					if (value.isString()) {
						try {
							return Integer.parseInt(value.getString());
						} catch (NumberFormatException ignored) {
							// Fall through to the temporary cache/default below.
						}
					}
				}
			}
		}
		return user.getUserData().getInt(path, UserDataFetchMode.TEMP_ONLY);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int amount) {
		return transfer(source, target, amount, amount);
	}

	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount, int creditAmount) {
		return transferAtomically(source, target, debitAmount, creditAmount);
	}

	/**
	 * Transfers points while allowing the recipient hook to approve or adjust the
	 * credit after the conditional debit has succeeded. The reservation transaction
	 * is committed and its connection is closed before the callback is invoked, so
	 * arbitrary listeners may safely read from the database. A durable journal then
	 * makes the refund/credit settlement idempotent.
	 */
	boolean transfer(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			IntFunction<Integer> creditAmountProvider) {
		drainCache(source);
		drainCache(target);
		MySQL table = plugin.getMysql();
		String sourcePoints = source.getPointsPath();
		String targetPoints = target.getPointsPath();
		String transferId = UUID.randomUUID().toString();
		String owner = UUID.randomUUID().toString();
		SharedPointTransferJournal journal = null;
		try {
			journal = SharedPointTransferJournal.forTable(table);
			recoverTransfers(plugin, journal);
			try {
				if (!journal.reserve(transferId, source.getUUID(), sourcePoints, debitAmount, target.getUUID(), debitAmount,
						System.currentTimeMillis())) return false;
			} catch (SQLException failure) {
				// If reservation commit acknowledgement was lost, release the source
				// only when the journal still proves the hook never started.
				journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				throw failure;
			}
			SharedPointTransferJournal.ClaimOutcome claim = journal.claimHookWithConfirmation(transferId, owner,
					System.currentTimeMillis());
			if (claim == SharedPointTransferJournal.ClaimOutcome.NOT_CLAIMED) {
				journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				return false;
			}
			if (claim == SharedPointTransferJournal.ClaimOutcome.INDETERMINATE) {
				logIndeterminateClaim(transferId);
				return true;
			}
			Integer creditAmount;
			try {
				creditAmount = creditAmountProvider.apply(debitAmount);
			} catch (RuntimeException failure) {
				SharedPointTransferJournal.SettlementOutcome outcome;
				try {
					outcome = journal.settleWithConfirmation(transferId, owner, source.getUUID(), sourcePoints,
							target.getUUID(), targetPoints, debitAmount, null);
				} finally {
					// A listener may have recreated the recipient cache while the
					// settlement transaction was running. Invalidate only its stale
					// points value without dumping it back to storage.
					discardPointsCache(target, targetPoints);
				}
				logApprovalFailure(failure);
				return isAcceptedSettlement(outcome);
			}
			// The approval hook may inspect or mutate the recipient and recreate its
			// cache after the initial drain. Persist and remove that cache before the
			// settlement credit so no queued pre-settlement value can overwrite it.
			drainCache(target);
			SharedPointTransferJournal.SettlementOutcome outcome;
			try {
				outcome = journal.settleWithConfirmation(transferId, owner, source.getUUID(), sourcePoints,
						target.getUUID(), targetPoints, debitAmount, creditAmount);
			} finally {
				// A concurrent lookup can recreate the cache after the final
				// pre-settlement drain. Never dump its stale points snapshot after
				// the credit commits; preserve unrelated cached fields.
				discardPointsCache(target, targetPoints);
			}
			return isAcceptedSettlement(outcome);
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		}
	}

	/**
	 * Runs the durable transfer phases around a Bukkit-thread approval hook. The
	 * reservation and claim happen on the persistence executor, the arbitrary
	 * listener runs on Bukkit's thread, and settlement returns to persistence
	 * before the completion callback is posted back to the source entity lane.
	 */
	void transferWithBukkitApproval(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			IntFunction<Integer> creditAmountProvider, Consumer<Boolean> completion) {
		try {
			plugin.getTimer().execute(() -> {
			drainCache(source);
			drainCache(target);
			MySQL table = plugin.getMysql();
			String sourcePoints = source.getPointsPath();
			String targetPoints = target.getPointsPath();
			String transferId = UUID.randomUUID().toString();
			String owner = UUID.randomUUID().toString();
			SharedPointTransferJournal journal;
			try {
				journal = SharedPointTransferJournal.forTable(table);
				recoverTransfers(plugin, journal);
				if (!journal.reserve(transferId, source.getUUID(), sourcePoints, debitAmount, target.getUUID(), debitAmount,
						System.currentTimeMillis())) {
					completeOnBukkit(source, completion, false);
					return;
				}
				// The source cache may have been recreated while the reservation was being
				// committed. Invalidate its points after the durable debit, before any later
				// dump can restore the pre-debit balance.
				discardPointsCache(source, sourcePoints);
			} catch (SQLException failure) {
				logFailure(failure);
				completeOnBukkit(source, completion, false);
				return;
			}

			/*
			 * Do not claim the reservation until the Bukkit approval task has actually
			 * started. If scheduling is rejected, the row remains RESERVED and startup
			 * recovery can safely return the debit. JDBC claim work remains on the
			 * persistence executor, never on the Bukkit lane.
			 */
			try {
				plugin.getBukkitScheduler().runTask(plugin, () -> {
					try {
						plugin.getTimer().execute(() -> claimTransferForApproval(source, target, debitAmount,
								creditAmountProvider, completion, journal, transferId, owner, sourcePoints, targetPoints));
					} catch (RuntimeException schedulingFailure) {
						// This callback is on Bukkit's lane. The durable RESERVED row is
						// intentionally left for the bounded periodic/startup recovery instead
						// of running its JDBC refund inline after executor rejection.
						completeRejectedPersistenceSubmission(source, completion, schedulingFailure);
					}
				});
			} catch (RuntimeException schedulingFailure) {
				refundReservedAfterSchedulingFailure(source, completion, journal, transferId, sourcePoints, debitAmount,
						schedulingFailure);
			}
			});
		} catch (RuntimeException schedulingFailure) {
			// No reservation exists when the initial persistence task is rejected.
			// Still complete the command contract on the source entity lane.
			plugin.debug(schedulingFailure);
			completeOnBukkit(source, completion, false);
		}
	}

	private void claimTransferForApproval(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			IntFunction<Integer> creditAmountProvider, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String owner, String sourcePoints, String targetPoints) {
		SharedPointTransferJournal.ClaimOutcome claim = journal.claimHookWithConfirmation(transferId, owner,
				System.currentTimeMillis());
		if (claim == SharedPointTransferJournal.ClaimOutcome.NOT_CLAIMED) {
			try {
				journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				discardPointsCache(source, sourcePoints);
			} catch (SQLException failure) {
				logFailure(failure);
			}
			completeOnBukkit(source, completion, false);
			return;
		}
		if (claim == SharedPointTransferJournal.ClaimOutcome.INDETERMINATE) {
			// The approval task has not been submitted yet, so an ambiguous claim
			// cannot have invoked the recipient hook. Compensate the durable claim
			// instead of reporting success and leaving a HOOK_STARTED debit behind.
			refundIndeterminateClaimBeforeApproval(source, completion, journal, transferId, sourcePoints,
					debitAmount);
			return;
		}
		discardPointsCache(source, sourcePoints);
		org.bukkit.entity.Player targetPlayer = target.getPlayer();
		org.bukkit.entity.Player approvalPlayer = targetPlayer != null ? targetPlayer : source.getPlayer();
		AtomicInteger approvalState = new AtomicInteger(0);
		Runnable rejectBeforeStart = () -> {
			if (!approvalState.compareAndSet(0, 2)) return;
			scheduleRejectedTransferCompensation(source, completion, journal, transferId, sourcePoints, debitAmount);
		};
		try {
			CompletableFuture<EntityTaskResult> approval = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(approvalPlayer, ignored -> {
				if (!approvalState.compareAndSet(0, 1)) return;
				Integer approvedAmount;
				try {
					approvedAmount = creditAmountProvider.apply(debitAmount);
				} catch (RuntimeException failure) {
					approvedAmount = null;
					logApprovalFailure(failure);
				}
				Integer finalApprovedAmount = approvedAmount;
				try {
					plugin.getTimer().execute(() -> settleTransfer(source, target, debitAmount, completion, journal,
							transferId, owner, sourcePoints, targetPoints, finalApprovedAmount));
				} catch (RuntimeException schedulingFailure) {
					plugin.debug(schedulingFailure);
					try {
						plugin.getBukkitScheduler().runTaskAsynchronously(plugin,
								() -> settleTransfer(source, target, debitAmount, completion, journal, transferId, owner,
										sourcePoints, targetPoints, finalApprovedAmount));
					} catch (RuntimeException asyncSchedulingFailure) {
						plugin.debug(asyncSchedulingFailure);
						logIndeterminateClaim(transferId);
						completeOnBukkit(source, completion, true);
					}
				} finally {
					approvalState.set(2);
				}
			}, rejectBeforeStart);
			approval.whenComplete((result, failure) -> {
				if (failure != null || result != EntityTaskResult.SUCCESS) rejectBeforeStart.run();
			});
		} catch (RuntimeException schedulingFailure) {
			plugin.debug(schedulingFailure);
			rejectBeforeStart.run();
		}
	}

	private void scheduleRejectedTransferCompensation(VotingPluginUser source, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String sourcePoints, int debitAmount) {
		Runnable compensation = () -> compensateRejectedTransfer(source, completion, journal, transferId,
				sourcePoints, debitAmount);
		try {
			plugin.getTimer().execute(compensation);
		} catch (RuntimeException persistenceRejected) {
			plugin.debug(persistenceRejected);
			try {
				plugin.getBukkitScheduler().runTaskAsynchronously(plugin, compensation);
			} catch (RuntimeException asyncRejected) {
				plugin.debug(asyncRejected);
				rememberPendingCompensationMarker(plugin, transferId);
				completeOnBukkit(source, completion, false);
			}
		}
	}

	private void rememberPendingCompensationMarker(VotingPluginMain plugin, String transferId) {
		try {
			compensationStore(plugin).record(transferId);
		} catch (IOException persistenceFailure) {
			plugin.getLogger().severe("Unable to persist a shared point transfer compensation marker: "
					+ persistenceFailure.getClass().getSimpleName());
			plugin.debug(persistenceFailure);
		}
	}

	private static void retryPendingCompensationMarkers(VotingPluginMain plugin,
			SharedPointTransferJournal journal) {
		SharedPointTransferCompensationStore store = compensationStore(plugin);
		final java.util.List<String> pending;
		try {
			pending = store.loadBatch();
		} catch (IOException loadFailure) {
			plugin.debug(loadFailure);
			return;
		}
		for (String transferId : pending) {
			try {
				journal.markCompensating(transferId);
				store.remove(transferId);
			} catch (SQLException retryFailure) {
				plugin.debug(retryFailure);
			} catch (IOException removalFailure) {
				plugin.debug(removalFailure);
			}
		}
	}

	private static SharedPointTransferCompensationStore compensationStore(VotingPluginMain plugin) {
		return new SharedPointTransferCompensationStore(plugin.getDataFolder().toPath());
	}

	private void compensateRejectedTransfer(VotingPluginUser source, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String sourcePoints, int debitAmount) {
		try {
			// The CAS fence proves the approval callback cannot run. Write the
			// recoverable state before relying on completion delivery.
			if (!journal.markCompensating(transferId)) {
				completeOnBukkit(source, completion, false);
				return;
			}
		} catch (SQLException markerFailure) {
			boolean refunded = false;
			try {
				refunded = journal.refundHookStarted(transferId, source.getUUID(), sourcePoints, debitAmount);
				if (refunded) {
					discardPointsCache(source, sourcePoints);
				}
			} catch (SQLException refundFailure) {
				logFailure(refundFailure);
			}
			// The scheduler fence proves the approval hook cannot run. If neither
			// MySQL compensation operation completed, retain the local durable proof
			// so periodic recovery can move the HOOK_STARTED row to COMPENSATING.
			if (!refunded) rememberPendingCompensationMarker(plugin, transferId);
			logFailure(markerFailure);
			completeOnBukkit(source, completion, false);
			return;
		}
		refundClaimedAfterSchedulingFailure(source, completion, journal, transferId, sourcePoints,
				debitAmount, new IllegalStateException("Transfer approval task did not start"));
	}

	private void settleTransfer(VotingPluginUser source, VotingPluginUser target, int debitAmount,
			Consumer<Boolean> completion, SharedPointTransferJournal journal, String transferId, String owner,
			String sourcePoints, String targetPoints, Integer approvedAmount) {
		boolean transferred;
		try {
			// The hook may have recreated either cache while it ran on Bukkit.
			drainCache(target);
			SharedPointTransferJournal.SettlementOutcome outcome;
			try {
				outcome = journal.settleWithConfirmation(transferId, owner, source.getUUID(), sourcePoints,
						target.getUUID(), targetPoints, debitAmount, approvedAmount);
			} finally {
				discardPointsCache(source, sourcePoints);
				discardPointsCache(target, targetPoints);
			}
			transferred = isAcceptedSettlement(outcome);
		} catch (RuntimeException failure) {
			plugin.getLogger().severe("Unable to settle shared MySQL point transfer: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
			transferred = false;
		}
		completeOnBukkit(source, completion, transferred);
	}

	private void refundReservedAfterSchedulingFailure(VotingPluginUser source, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String sourcePoints, int debitAmount,
			RuntimeException failure) {
		try {
			if (journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount)) {
				discardPointsCache(source, sourcePoints);
			}
		} catch (SQLException refundFailure) {
			logFailure(refundFailure);
		}
		plugin.debug(failure);
		completeOnBukkit(source, completion, false);
	}

	private void refundIndeterminateClaimBeforeApproval(VotingPluginUser source, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String sourcePoints, int debitAmount) {
		boolean refunded = false;
		try {
			// HOOK_STARTED is safe to compensate because the approval task has not
			// been submitted yet. A RESERVED row is handled by its normal refund.
			if (journal.markCompensating(transferId)) {
				refunded = journal.refundHookStarted(transferId, source.getUUID(), sourcePoints, debitAmount);
			} else {
				refunded = journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
			}
		} catch (SQLException markerFailure) {
			// A lost marker acknowledgement may still have committed. Both refund
			// operations are idempotent and cover either durable pre-hook state.
			try {
				refunded = journal.refundHookStarted(transferId, source.getUUID(), sourcePoints, debitAmount);
				if (!refunded) {
					refunded = journal.refundReserved(transferId, source.getUUID(), sourcePoints, debitAmount);
				}
			} catch (SQLException refundFailure) {
				logFailure(refundFailure);
			}
			logFailure(markerFailure);
		}
		if (refunded) discardPointsCache(source, sourcePoints);
		if (!refunded) logIndeterminateClaim(transferId);
		completeOnBukkit(source, completion, false);
	}

	void completeRejectedPersistenceSubmission(VotingPluginUser source, Consumer<Boolean> completion,
			RuntimeException failure) {
		plugin.debug(failure);
		completeOnBukkit(source, completion, false);
	}

	private void refundClaimedAfterSchedulingFailure(VotingPluginUser source, Consumer<Boolean> completion,
			SharedPointTransferJournal journal, String transferId, String sourcePoints, int debitAmount,
			RuntimeException failure) {
		try {
			if (journal.refundHookStarted(transferId, source.getUUID(), sourcePoints, debitAmount)) {
				discardPointsCache(source, sourcePoints);
			}
		} catch (SQLException refundFailure) {
			logFailure(refundFailure);
		}
		plugin.debug(failure);
		completeOnBukkit(source, completion, false);
	}

	private boolean isAcceptedSettlement(SharedPointTransferJournal.SettlementOutcome outcome) {
		if (outcome == SharedPointTransferJournal.SettlementOutcome.INDETERMINATE) {
			// The callback has already run. Reporting a retryable failure could create
			// a second transfer after an unconfirmed credit, so retain the journal row
			// for explicit reconciliation and suppress a new debit attempt.
			plugin.getLogger().severe("Shared MySQL point transfer outcome is indeterminate; retaining journal entry for reconciliation");
			return true;
		}
		return outcome == SharedPointTransferJournal.SettlementOutcome.COMPLETED;
	}

	private void logIndeterminateClaim(String transferId) {
		plugin.getLogger().severe("Shared MySQL point transfer " + transferId
				+ " has indeterminate claim state (RESERVED or HOOK_STARTED); retaining it for explicit reconciliation");
	}

	private boolean transferAtomically(VotingPluginUser source, VotingPluginUser target,
			int debitAmount, int creditAmount) {
		drainCache(source);
		drainCache(target);
		MySQL table = plugin.getMysql();
		String sourcePoints = source.getPointsPath();
		String targetPoints = target.getPointsPath();
		String uuidMatch = table.qi("uuid") + (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		String debit = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(sourcePoints) + " = "
				+ table.qi(sourcePoints) + " - ? WHERE " + uuidMatch + " AND " + table.qi(sourcePoints) + " >= ?";
		String credit = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(targetPoints) + " = "
				+ table.qi(targetPoints) + " + ? WHERE " + uuidMatch;
		try (Connection connection = requireConnection(table)) {
			connection.setAutoCommit(false);
			try (PreparedStatement debitStatement = connection.prepareStatement(debit);
					PreparedStatement creditStatement = connection.prepareStatement(credit)) {
				debitStatement.setInt(1, debitAmount);
				debitStatement.setString(2, source.getUUID());
				debitStatement.setInt(3, debitAmount);
				if (debitStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
				creditStatement.setInt(1, creditAmount);
				creditStatement.setString(2, target.getUUID());
				if (creditStatement.executeUpdate() != 1) {
					connection.rollback();
					return false;
				}
				connection.commit();
				return true;
			} catch (SQLException failure) {
				connection.rollback();
				throw failure;
			}
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		}
	}

	private boolean run(Runnable operation, boolean async) {
		if (async) {
			try {
				plugin.getTimer().execute(operation);
				return true;
			} catch (RuntimeException rejected) {
				plugin.debug(rejected);
				return false;
			}
		} else {
			operation.run();
			return true;
		}
	}

	private boolean update(VotingPluginUser user, int delta, boolean requireNonnegative) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		StringBuilder sql = new StringBuilder("UPDATE ").append(table.qi(table.getTableName())).append(" SET ")
				.append(table.qi(points)).append(" = ").append(table.qi(points)).append(" + ? WHERE ")
				.append(table.qi("uuid")).append(table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		if (requireNonnegative) {
			sql.append(" AND ").append(table.qi(points)).append(" >= ?");
		}
		try (Connection connection = requireConnection(table);
				PreparedStatement statement = connection.prepareStatement(sql.toString())) {
			statement.setInt(1, delta);
			statement.setString(2, user.getUUID());
			if (requireNonnegative) statement.setInt(3, -delta);
			return statement.executeUpdate() == 1;
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		} finally {
			discardPointsCache(user);
		}
	}

	/**
	 * Adds points and reads the resulting value through the same JDBC connection.
	 * This bypasses the wrapper's temporary user-data cache, which can remain stale
	 * even when the caller requests {@code NO_CACHE}.
	 */
	private int addAndReadCommitted(VotingPluginUser user, int amount) {
		return addAndReadCommittedResult(user, amount).total();
	}

	private AddResult addAndReadCommittedResult(VotingPluginUser user, int amount) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		String uuidMatch = table.qi("uuid") + (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		String update = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(points) + " = "
				+ table.qi(points) + " + ? WHERE " + uuidMatch;
		String read = "SELECT " + table.qi(points) + " FROM " + table.qi(table.getTableName()) + " WHERE " + uuidMatch;
		boolean updateCommitted = false;
		Integer committedTotal = null;
		try (Connection connection = requireConnection(table);
				PreparedStatement updateStatement = connection.prepareStatement(update);
				PreparedStatement readStatement = connection.prepareStatement(read)) {
			updateStatement.setInt(1, amount);
			updateStatement.setString(2, user.getUUID());
			if (updateStatement.executeUpdate() == 1) {
				// With JDBC auto-commit, executeUpdate returning one means the mutation
				// completed. A later read may still fail after the points have been
				// committed, so never turn that outcome into a retryable failure.
				updateCommitted = true;
				readStatement.setString(1, user.getUUID());
				try (java.sql.ResultSet result = readStatement.executeQuery()) {
					if (result.next()) committedTotal = result.getInt(1);
				}
			}
		} catch (SQLException failure) {
			logFailure(failure);
		} finally {
			discardPointsCache(user);
		}
		// Do not evaluate the fallback while the JDBC handle is still held. With a
		// one-connection pool, getPoints() may need that same handle after a missing
		// row or a failed follow-up read.
		if (committedTotal != null) return new AddResult(true, committedTotal);
		// A failed acquisition cannot support the fallback read either. Report the
		// mutation failure without checking out a second connection and let callers
		// complete their callback deterministically.
		if (!updateCommitted) return new AddResult(false, 0);
		return new AddResult(true, user.getPoints());
	}

	record AddResult(boolean success, int total) {}

	private boolean setAbsolute(VotingPluginUser user, int value) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String sql = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(user.getPointsPath())
				+ " = ? WHERE " + table.qi("uuid")
				+ (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = requireConnection(table);
				PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setInt(1, value);
			statement.setString(2, user.getUUID());
			return statement.executeUpdate() == 1;
		} catch (SQLException failure) {
			logFailure(failure);
			return false;
		} finally {
			discardPointsCache(user);
		}
	}

	private void capAt(VotingPluginUser user, int maximum) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		String sql = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(points) + " = LEAST("
				+ table.qi(points) + ", ?) WHERE " + table.qi("uuid")
				+ (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = requireConnection(table);
				PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setInt(1, maximum);
			statement.setString(2, user.getUUID());
			statement.executeUpdate();
		} catch (SQLException failure) {
			logFailure(failure);
		} finally {
			discardPointsCache(user);
		}
	}

	private void addAndCapAt(VotingPluginUser user, int amount, int maximum) {
		drainCache(user);
		MySQL table = plugin.getMysql();
		String points = user.getPointsPath();
		String sql = "UPDATE " + table.qi(table.getTableName()) + " SET " + table.qi(points) + " = LEAST("
				+ table.qi(points) + " + ?, ?) WHERE " + table.qi("uuid")
				+ (table.getDbType() == DbType.POSTGRESQL ? " = ?::uuid" : " = ?");
		try (Connection connection = requireConnection(table);
				PreparedStatement statement = connection.prepareStatement(sql)) {
			statement.setInt(1, amount);
			statement.setInt(2, maximum);
			statement.setString(3, user.getUUID());
			statement.executeUpdate();
		} catch (SQLException failure) {
			logFailure(failure);
		} finally {
			discardPointsCache(user);
		}
	}

	private void drainCache(VotingPluginUser user) {
		if (user.isCached()) {
			user.getCache().dump();
			plugin.getUserManager().getDataManager().removeCache(UUID.fromString(user.getUUID()), null);
		}
	}

	private static Connection requireConnection(MySQL table) throws SQLException {
		Connection connection = table.getMysql().getConnectionManager().getConnection();
		if (connection == null) throw new SQLException("Unable to acquire shared MySQL connection");
		return connection;
	}

	/**
	 * Removes only the value made stale by a direct shared-MySQL point mutation.
	 * The cache can be recreated while JDBC is in progress by vote processing on
	 * the Bukkit lane; dropping that whole cache would also lose unrelated queued
	 * streak, milestone, or cooldown updates. VotingPlugin routes every supported
	 * Points writer through this mutator; a later generic UserData Points change is
	 * deliberately not discarded here because it is a distinct, later write and
	 * the generic absolute-value API cannot provide cross-server atomic semantics.
	 */
	private void discardPointsCache(VotingPluginUser user) {
		discardPointsCache(user, user.getPointsPath());
	}

	private void discardPointsCache(VotingPluginUser user, String pointsColumn) {
		SharedMysqlCacheReconciler.invalidate(plugin, user.getUUID(), pointsColumn);
	}

	private void completeOnBukkit(VotingPluginUser source, Consumer<Boolean> completion, boolean transferred) {
		BukkitCompletionScheduler.run(plugin, source.getPlayer(), () -> completion.accept(transferred));
	}

	private void logFailure(SQLException failure) {
		plugin.getLogger().severe("Unable to update shared MySQL vote points: " + failure.getClass().getSimpleName());
		plugin.debug(failure);
	}

	private void logApprovalFailure(RuntimeException failure) {
		plugin.getLogger().severe("Unable to approve shared MySQL point transfer on the persistence worker: "
				+ failure.getClass().getSimpleName());
		plugin.debug(failure);
	}
}
