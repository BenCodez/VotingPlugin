package com.bencodez.votingplugin.neoforge;

import java.time.Clock;
import java.util.Objects;
import java.util.Optional;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.core.vote.SharedVoteInput;

/**
 * Internal accepted-vote boundary for the subset NeoForge can currently finish:
 * identity/site/delay decisions and atomic persisted accounting.
 *
 * <p>Complete production votes are retained without accounting mutation until
 * reward, offline-queue, vote-party, broadcast, streak, milestone, cooldown,
 * event, and placeholder operations have real NeoForge implementations.</p>
 */
public final class NeoForgeVoteProcessor {
    private final NeoForgeVoteConfiguration configuration;
    private final NeoForgeVoteAccountingStore accounting;
    private final NeoForgeDeferredVoteStore deferredVotes;
    private final NeoForgePlayerDirectory players;
    private final Clock clock;
    private volatile boolean stopped;

    NeoForgeVoteProcessor(NeoForgeVoteConfiguration configuration,
            NeoForgeVoteAccountingStore accounting, NeoForgeDeferredVoteStore deferredVotes,
            NeoForgePlayerDirectory players, Clock clock) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
        this.accounting = Objects.requireNonNull(accounting, "accounting");
        this.deferredVotes = Objects.requireNonNull(deferredVotes, "deferredVotes");
        this.players = Objects.requireNonNull(players, "players");
        this.clock = Objects.requireNonNull(clock, "clock");
    }

    public synchronized NeoForgeVoteResult process(NeoForgeVoteRequest request) {
        Objects.requireNonNull(request, "request");
        if (stopped) return result(NeoForgeVoteResult.Status.STOPPED, "NeoForge runtime is stopped");
        if (request.scope() == NeoForgeVoteRequest.Scope.COMPLETE) {
            NeoForgeDeferredVoteStore.OccurrenceState state = deferredVotes.state(request.playerId(), request.voteId());
            if (state == NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED) {
                return result(NeoForgeVoteResult.Status.ALREADY_COMPLETED,
                        "Vote was already completed and remains durably recognized");
            }
            if (state == NeoForgeDeferredVoteStore.OccurrenceState.PENDING) {
                return result(NeoForgeVoteResult.Status.DEFERRED,
                        "Vote was already retained for future complete processing");
            }
        }

        Optional<SharedVoteIdentity> online = players.online(request.playerId());
        Optional<NeoForgeVoteAccount> stored = accounting.load(request.playerId());
        if (request.online() && online.isEmpty()) {
            return result(NeoForgeVoteResult.Status.UNKNOWN_PLAYER,
                    "Player was marked online but is absent from the NeoForge player directory");
        }
        if (online.isEmpty() && stored.isEmpty() && !configuration.allowUnjoined()) {
            return result(NeoForgeVoteResult.Status.UNKNOWN_PLAYER, "Player identity is not known");
        }
        String name = online.map(SharedVoteIdentity::playerName)
                .orElseGet(() -> stored.map(NeoForgeVoteAccount::playerName).orElse(request.playerName()));
        SharedVoteIdentity identity = new SharedVoteIdentity(request.playerId(), name,
                online.isPresent());
        Optional<NeoForgeVoteSite> resolved = configuration.resolveEnabledSite(request.serviceSite());
        if (resolved.isEmpty()) {
            return result(NeoForgeVoteResult.Status.UNKNOWN_SITE, "No enabled vote site matches the service site");
        }
        long now = clock.millis();
        SharedVoteInput input = new SharedVoteInput(request.voteId(), identity.playerName(), request.serviceSite(),
                request.voteTime(), request.realVote(), request.addTotals(), false, false, identity.online())
                .normalizedVoteTime(now);
        NeoForgeVoteSite site = resolved.get();
        if (request.scope() == NeoForgeVoteRequest.Scope.COMPLETE) {
            NeoForgeDeferredVoteStore.DeferralResult deferred = deferredVotes.defer(identity, input, site);
            if (deferred.status() == NeoForgeDeferredVoteStore.Status.CAPACITY_REACHED) {
                return result(NeoForgeVoteResult.Status.DEFERRED_CAPACITY_REACHED,
                        "NeoForge deferred-vote capacity is exhausted; the caller must not acknowledge this vote");
            }
            if (deferred.status() == NeoForgeDeferredVoteStore.Status.ALREADY_COMPLETED) {
                return result(NeoForgeVoteResult.Status.ALREADY_COMPLETED,
                        "Vote was already completed and remains durably recognized");
            }
            return result(NeoForgeVoteResult.Status.DEFERRED,
                    deferred.status() == NeoForgeDeferredVoteStore.Status.ALREADY_RETAINED
                            ? "Vote was already retained for future complete processing"
                            : "Vote retained for future complete processing");
        }
        NeoForgeVoteAccountingStore.AccountingResult update = accounting.applyIfVoteDelayAllows(identity, input,
                configuration.policyFor(site), site, configuration.pointsOnVote(), configuration.limitVotePoints(),
                configuration.currentTime(clock), clock.getZone(), configuration.timeHourOffset());
        if (!update.accepted()) {
            return result(NeoForgeVoteResult.Status.VOTE_DELAY_ACTIVE, "Vote delay has not elapsed");
        }
        return new NeoForgeVoteResult(NeoForgeVoteResult.Status.ACCOUNTED, update.account(),
                "Accounting subset persisted; production follow-up operations were not requested");
    }

    synchronized void stop() { stopped = true; }

    private static NeoForgeVoteResult result(NeoForgeVoteResult.Status status, String detail) {
        return new NeoForgeVoteResult(status, null, detail);
    }
}
