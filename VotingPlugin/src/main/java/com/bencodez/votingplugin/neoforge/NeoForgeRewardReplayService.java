package com.bencodez.votingplugin.neoforge;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.bencodez.votingplugin.util.MinecraftUsernameValidator;

/** Bounded storage-worker replay for retained COMPLETE NeoForge votes. */
public final class NeoForgeRewardReplayService implements AutoCloseable {
    private static final Logger LOGGER = Logger.getLogger(NeoForgeRewardReplayService.class.getName());
    static final int MAX_PER_RUN = 16;
    static final int MAX_USERS_PER_RUN = 32;
    private final NeoForgeVoteConfiguration configuration;
    private final NeoForgeRewardConfiguration rewards;
    private final NeoForgeVoteAccountingStore accounting;
    private final NeoForgeDeferredVoteStore deferred;
    private final NeoForgePlayerDirectory players;
    private final NeoForgeRewardActions actions;
    private final ScheduledExecutorService worker;
    private final AtomicBoolean scanning = new AtomicBoolean();
    private final AtomicBoolean open = new AtomicBoolean(true);
    private final ConcurrentHashMap<UUID, Long> retryAfter = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<UUID, Integer> occurrenceOffsets = new ConcurrentHashMap<>();
    private int userOffset;
    private volatile ScheduledFuture<?> periodic;

    NeoForgeRewardReplayService(NeoForgeVoteConfiguration configuration,
            NeoForgeRewardConfiguration rewards, NeoForgeVoteAccountingStore accounting,
            NeoForgeDeferredVoteStore deferred, NeoForgePlayerDirectory players,
            NeoForgeRewardActions actions) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        this.rewards = Objects.requireNonNull(rewards, "rewards");
        this.accounting = Objects.requireNonNull(accounting, "accounting");
        this.deferred = Objects.requireNonNull(deferred, "deferred");
        this.players = Objects.requireNonNull(players, "players");
        this.actions = Objects.requireNonNull(actions, "actions");
        ThreadFactory factory = task -> {
            Thread thread = new Thread(task, "VotingPlugin-NeoForge-Replay");
            thread.setDaemon(true);
            return thread;
        };
        worker = Executors.newSingleThreadScheduledExecutor(factory);
    }

    void start() {
        periodic = worker.scheduleWithFixedDelay(this::replayScheduled, 1, 1, TimeUnit.SECONDS);
    }

    void replayScheduled() {
        replayOnce().whenComplete((ignored, failure) -> {
            if (failure != null && open.get()) {
                LOGGER.log(Level.SEVERE, "Scheduled NeoForge reward replay failed", failure);
            }
        });
    }

    public CompletableFuture<List<ReplayResult>> replayOnce() {
        CompletableFuture<List<ReplayResult>> result = new CompletableFuture<>();
        if (!open.get()) {
            result.completeExceptionally(new RejectedExecutionException("NeoForge reward replay has stopped"));
            return result;
        }
        if (!scanning.compareAndSet(false, true)) {
            result.complete(List.of());
            return result;
        }
        try {
            worker.execute(() -> {
                try {
                    deferred.initializeRelevantRowIndexes();
                    ArrayList<CompletableFuture<ReplayResult>> work = new ArrayList<>();
                    List<UUID> users = deferred.users();
                    int visits = Math.min(MAX_USERS_PER_RUN, users.size());
                    int start = users.isEmpty() ? 0 : Math.floorMod(userOffset, users.size());
                    int visited = 0;
                    for (; visited < visits && work.size() < MAX_PER_RUN; visited++) {
                        UUID playerId = users.get((start + visited) % users.size());
                        List<NeoForgeDeferredVote> pending;
                        try {
                            pending = deferred.pending(playerId);
                        } catch (NeoForgeDeferredVoteStore.MalformedDeferredVoteData malformed) {
                            // The durable row stays untouched and observable through the store;
                            // another user's healthy queue must continue replaying.
                            continue;
                        }
                        if (pending.isEmpty()) {
                            occurrenceOffsets.remove(playerId);
                            continue;
                        }
                        int voteStart = Math.floorMod(occurrenceOffsets.getOrDefault(playerId, 0), pending.size());
                        for (int checked = 0; checked < pending.size(); checked++) {
                            int index = (voteStart + checked) % pending.size();
                            NeoForgeDeferredVote vote = pending.get(index);
                            if (retryAfter.getOrDefault(vote.voteId(), 0L) > System.nanoTime()) continue;
                            occurrenceOffsets.put(playerId, (index + 1) % pending.size());
                            work.add(replay(vote));
                            break;
                        }
                    }
                    if (!users.isEmpty()) userOffset = (start + visited) % users.size();
                    CompletableFuture.allOf(work.toArray(CompletableFuture[]::new))
                            .whenCompleteAsync((ignored, failure) -> {
                                try {
                                    ArrayList<ReplayResult> completed = new ArrayList<>(work.size());
                                    for (CompletableFuture<ReplayResult> item : work) completed.add(item.join());
                                    scanning.set(false);
                                    result.complete(List.copyOf(completed));
                                } catch (CompletionException completionFailure) {
                                    scanning.set(false);
                                    result.completeExceptionally(completionFailure.getCause());
                                }
                            }, worker);
                } catch (Throwable failure) {
                    scanning.set(false);
                    result.completeExceptionally(failure);
                }
            });
        } catch (RuntimeException stopped) {
            scanning.set(false);
            result.completeExceptionally(stopped);
        }
        return result;
    }

    private CompletableFuture<ReplayResult> replay(NeoForgeDeferredVote vote) {
        if (vote.quarantined()) {
            retryAfter.put(vote.voteId(), Long.MAX_VALUE);
            return CompletableFuture.completedFuture(result(vote, Status.REWARD_UNCERTAIN,
                    "Durably quarantined after an uncertain external effect; operator action is required"));
        }
        if (vote.accountingDecision() == null) {
            return CompletableFuture.completedFuture(delayed(vote, Status.BLOCKED_UNSUPPORTED,
                    "Legacy retained vote has no accepted accounting snapshot; operator action is required", 60));
        }
        var onlineIdentity = players.online(vote.playerId());
        String currentName = onlineIdentity.map(identity -> identity.playerName()).orElse(vote.playerName());
        if (!MinecraftUsernameValidator.isValid(currentName, configuration.bedrockPlayerPrefix())) {
            return CompletableFuture.completedFuture(delayed(vote, Status.BLOCKED_UNSUPPORTED,
                    "Retained player name is invalid and cannot be used in rewards", 60));
        }
        NeoForgeVoteSite site = configuration.configuredSite(vote.siteKey()).orElse(null);
        if (site == null) return CompletableFuture.completedFuture(
                delayed(vote, Status.BLOCKED_UNSUPPORTED, "Configured vote site no longer exists", 60));
        NeoForgeDeferredVote effectiveVote = vote.withReplayContext(currentName, site.serviceSite());
        NeoForgeRewardPlan plan = rewards.plan(effectiveVote, site, onlineIdentity.isPresent());
        if (plan.status() == NeoForgeRewardPlan.Status.BLOCKED_UNSUPPORTED) {
            return CompletableFuture.completedFuture(delayed(vote, Status.BLOCKED_UNSUPPORTED, plan.detail(), 60));
        }
        if (plan.status() == NeoForgeRewardPlan.Status.WAITING_FOR_PLAYER) {
            return CompletableFuture.completedFuture(delayed(vote, Status.WAITING_FOR_PLAYER, plan.detail(), 5));
        }
        NeoForgeDeferredVoteStore.Claim claim = deferred.claim(vote.playerId(), vote.voteId()).orElse(null);
        if (claim == null) return CompletableFuture.completedFuture(
                result(vote, Status.NOT_CLAIMED, "Vote is already claimed or completion capacity is unavailable"));
        CompletableFuture<ReplayResult> completion = new CompletableFuture<>();
        CompletionStage<Void> action;
        try {
            action = plan.actions().isEmpty()
                    ? CompletableFuture.completedFuture(null) : actions.execute(effectiveVote, plan);
        } catch (RuntimeException failure) {
            claim.close();
            return CompletableFuture.completedFuture(rewardFailure(vote, failure));
        }
        action.whenComplete((ignored, failure) -> dispatchCompletion(claim, effectiveVote, site, failure, completion));
        return completion;
    }

    private ReplayResult rewardFailure(NeoForgeDeferredVote vote, Throwable failure) {
        if (containsUncertainOutcome(failure)) {
            return quarantine(vote, Status.REWARD_UNCERTAIN, safeFailure(failure));
        }
        return delayed(vote, Status.REWARD_FAILED, safeFailure(failure), 5);
    }

    private void dispatchCompletion(NeoForgeDeferredVoteStore.Claim claim, NeoForgeDeferredVote vote,
            NeoForgeVoteSite site, Throwable failure, CompletableFuture<ReplayResult> completion) {
        Runnable finish = () -> {
            if (failure != null) {
                claim.close();
                completion.complete(rewardFailure(vote, failure));
                return;
            }
            try {
                NeoForgeDeferredVoteStore.CompletionOutcome outcome;
                try (claim) {
                    outcome = claim.completeWithAccounting(accounting, site,
                            vote.wasOnline(), vote.playerName());
                }
                Status status = outcome.result() == NeoForgeDeferredVoteStore.CompletionResult.COMPLETED
                        || outcome.result() == NeoForgeDeferredVoteStore.CompletionResult.ALREADY_COMPLETED
                                ? Status.COMPLETED : Status.NOT_CLAIMED;
                if (status == Status.COMPLETED) {
                    retryAfter.remove(vote.voteId());
                    completion.complete(new ReplayResult(vote.voteId(), status, outcome.account(),
                            "Completion result: " + outcome.result()));
                } else {
                    completion.complete(quarantine(vote, Status.COMPLETION_UNCERTAIN,
                            "Completion result: " + outcome.result()));
                }
            } catch (Throwable completionFailure) {
                completion.complete(quarantine(vote, Status.COMPLETION_UNCERTAIN,
                        safeFailure(completionFailure)));
            }
        };
        try {
            worker.execute(finish);
        } catch (RuntimeException stopped) {
            claim.close();
            completion.completeExceptionally(stopped);
        }
    }

    private static ReplayResult result(NeoForgeDeferredVote vote, Status status, String detail) {
        return new ReplayResult(vote.voteId(), status, null, detail);
    }

    private ReplayResult delayed(NeoForgeDeferredVote vote, Status status, String detail, long seconds) {
        Long previous = retryAfter.put(vote.voteId(), System.nanoTime() + TimeUnit.SECONDS.toNanos(seconds));
        if (previous == null) LOGGER.warning("NeoForge deferred vote " + vote.voteId() + " is " + status);
        return result(vote, status, detail);
    }

    private ReplayResult quarantine(NeoForgeDeferredVote vote, Status status, String detail) {
        retryAfter.put(vote.voteId(), Long.MAX_VALUE);
        try {
            NeoForgeDeferredVoteStore.QuarantineResult outcome = deferred.quarantine(
                    vote.playerId(), vote.voteId());
            if (outcome == NeoForgeDeferredVoteStore.QuarantineResult.ALREADY_COMPLETED) {
                retryAfter.remove(vote.voteId());
                return result(vote, Status.COMPLETED, "Completion receipt was already durable");
            }
            if (outcome == NeoForgeDeferredVoteStore.QuarantineResult.NOT_PENDING) {
                LOGGER.severe("NeoForge deferred vote " + vote.voteId()
                        + " could not be durably quarantined because its pending record is missing");
                return result(vote, status, detail + "; quarantine=" + outcome);
            }
            LOGGER.warning("NeoForge deferred vote " + vote.voteId()
                    + " is durably quarantined after an uncertain external effect");
            return result(vote, status, detail + "; quarantine=" + outcome);
        } catch (Throwable quarantineFailure) {
            LOGGER.severe("Failed to durably quarantine NeoForge deferred vote " + vote.voteId()
                    + "; automatic replay is stopped for this process");
            return result(vote, status, detail + "; quarantine=" + safeFailure(quarantineFailure));
        }
    }

    private static String safeFailure(Throwable failure) {
        Throwable cause = failure instanceof CompletionException && failure.getCause() != null
                ? failure.getCause() : failure;
        return cause.getClass().getSimpleName();
    }

    private static boolean containsUncertainOutcome(Throwable failure) {
        for (Throwable current = failure; current != null; current = current.getCause()) {
            if (current instanceof NeoForgeNativeRewardActions.UncertainRewardOutcomeException) return true;
        }
        return false;
    }

    void stopAdmission() {
        open.set(false);
        ScheduledFuture<?> task = periodic;
        if (task != null) task.cancel(false);
    }

    @Override public void close() {
        stopAdmission();
        worker.shutdown();
        try {
            if (!worker.awaitTermination(5, TimeUnit.SECONDS)) worker.shutdownNow();
        } catch (InterruptedException interrupted) {
            worker.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    public enum Status {
        COMPLETED, WAITING_FOR_PLAYER, BLOCKED_UNSUPPORTED, REWARD_FAILED, REWARD_UNCERTAIN,
        COMPLETION_UNCERTAIN, NOT_CLAIMED
    }

    public record ReplayResult(UUID voteId, Status status, NeoForgeVoteAccount account, String detail) { }
}
