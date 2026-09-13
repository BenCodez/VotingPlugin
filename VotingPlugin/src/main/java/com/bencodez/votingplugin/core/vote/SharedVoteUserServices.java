package com.bencodez.votingplugin.core.vote;

import java.util.UUID;
import java.util.concurrent.CompletionStage;

/**
 * Port for AdvancedCore's shared user/cache/storage runtime. VotingPlugin does
 * not own another SQL layer, user cache, or queued-write implementation here.
 */
public interface SharedVoteUserServices {
    /** Completes only after the logical vote mutation has been durably flushed. */
    CompletionStage<SharedVoteUserSnapshot> persistVote(SharedVoteIdentity identity, SharedVoteMutation mutation);

    /** Used by restart/replay consumers to inspect the same persisted user. */
    CompletionStage<SharedVoteUserSnapshot> load(UUID uuid);
}
